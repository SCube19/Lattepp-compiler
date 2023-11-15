{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
module Abstract.Optimizer.Data where
import           Abstract.Optimizer.Utils   (extractBool, extractInt,
                                             extractString, getDefsAndChanges,
                                             isELitBool, isELitInt,
                                             isELitString, isLiteral)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.State  (StateT, get, gets, modify, put)
import           Data.Foldable              (Foldable (foldl'))
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import           Syntax.AbsLattepp
import           Utils                      (rawBool, rawInt, rawPrimBool,
                                             rawPrimInt, rawPrimStr, rawStr)

type OptimizerState = StateT OptimizerS (ExceptT String IO)

newtype OptimizerS = OptimizerS {
  consts :: M.Map Ident (PrimType, Value)
}

data Value =  IntV Integer
            | BoolV Bool
            | StrV String

instance Show Value where
  show (IntV val)  = show val
  show (BoolV val) = show val
  show (StrV val)  = show val

resetConst :: OptimizerState()
resetConst = modify (\s -> s {consts = M.empty})

addConst :: Ident -> (PrimType, Value) -> OptimizerState ()
addConst i v = modify (\s -> s {consts = M.insert i v (consts s)})

removeConst :: Ident -> OptimizerState ()
removeConst i = modify (\s -> s {consts = M.delete i (consts s)})

removeChanges :: [Stmt] -> OptimizerState ()
removeChanges stmts = do
    let (_, changes) = getDefsAndChanges stmts (S.empty, S.empty)
    mapM_ removeConst changes

mapConst :: Expr -> OptimizerState Expr
mapConst e@(EVar pos i) = do
  c <- gets consts
  case M.lookup i c of
    Just (t, v) -> case t of
      Int _  -> return $ ELitInt pos $ castInteger v
      Bool _ -> return $ if castBool v then ELitTrue pos else ELitFalse pos
      Str _  -> return $ EString pos $ castString v
      _      -> undefined
    Nothing -> return e

mapConst _ = undefined

castBool :: Value -> Bool
castBool (IntV v)  = v > 0
castBool (BoolV v) = v
castBool _         = False

castInteger :: Value -> Integer
castInteger (IntV v)  = v
castInteger (BoolV v) = if v then 1 else 0
castInteger _         = 0

castString :: Value -> String
castString (StrV v) = v
castString _        = ""

isIntV :: Value -> Bool
isIntV (IntV _) = True
isIntV _        = False

isBoolV :: Value -> Bool
isBoolV (BoolV _) = True
isBoolV _         = False

isStrV :: Value -> Bool
isStrV (StrV _) = True
isStrV _        = False

retBoolLit :: BNFC'Position -> Bool -> OptimizerState (Expr, Maybe (Value, Either MulOp AddOp))
retBoolLit pos b =
  if b then
          return (ELitTrue pos, Nothing)
  else
          return (ELitFalse pos, Nothing)

constFromExpr :: Expr -> Ident -> OptimizerState ()
constFromExpr e ident = do
  if isELitInt e then
    addConst ident (rawPrimInt, IntV $ extractInt e)
  else if isELitBool e then
    addConst ident (rawPrimBool, BoolV $ extractBool e)
  else if isELitString e then
    addConst ident (rawPrimStr, StrV $ extractString e)
  else
    removeConst ident

localOptimizerEnv :: OptimizerS -> OptimizerState a -> OptimizerState a
localOptimizerEnv changedEnv action = do
  backup <- get
  put changedEnv
  result <- action
  put backup
  return result

class ValueType a where
  makeValue :: a -> Value

instance ValueType Integer where
  makeValue x = IntV x

instance ValueType Bool where
  makeValue x = BoolV x

instance ValueType String where
  makeValue x = StrV x

--(+), (*), abs, signum, fromInteger, (negate | (-))
instance Num Value where
  (IntV v1) + (IntV v2)   = IntV (v1 + v2)
  (BoolV v1) + (BoolV v2) = BoolV (v1 || v2)
  (StrV v1) + (StrV v2)   = StrV (v1 ++ v2)
  a + b                   = undefined

  (IntV v1) * (IntV v2)   = IntV (v1 * v2)
  (BoolV v1) * (BoolV v2) = BoolV (v1 && v2)
  (StrV v1) * (StrV v2)   = undefined
  a * b                   = undefined

  abs (IntV v)  = IntV (abs v)
  abs (BoolV v) = BoolV True
  abs (StrV v)  = StrV v

  signum (IntV v)  = IntV (signum v)
  signum (BoolV v) = BoolV v
  signum (StrV v)  = StrV v

  negate (IntV v)  = IntV (negate v)
  negate (BoolV v) = BoolV (not v)
  negate (StrV v)  = StrV v

  fromInteger i = IntV i


extractValue :: Expr -> Value
extractValue (ELitInt _ i) = makeValue i
extractValue (ELitFalse _) = makeValue False
extractValue (ELitTrue _)  = makeValue True
extractValue (EString _ s) = makeValue s
extractValue _             = undefined
