{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Utils where

import Syntax.AbsLattepp
import System.Directory.Internal.Prelude (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO ( stderr, hPutStrLn )
import Data.List ( intercalate )
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import qualified Data.Set as Set
import Control.Monad.Trans.State (StateT, get, put)
------------------------------GENERAL--------------------------------------------------------
exitError :: String -> IO ()
exitError e = do
    hPutStrLn stderr ("FATAL: " ++ e ++ " ¯\\_(ツ)_/¯")
    exitFailure

-- class Typical a where
--     toType :: a -> Type

-- getArgIdent :: Arg -> Ident
-- getArgIdent (Arg _ t ident) = ident

-- makeFalsyTuple :: a -> (a, Bool)
-- makeFalsyTuple x = (x, False)

-- throwException x = lift $ throwE $ show x

-- firstDuplicateIndex :: Ord a => [a] -> Maybe Int
-- firstDuplicateIndex xs = dup' xs Set.empty
--   where dup' [] _ = Nothing
--         dup' (x:xs) s = if Set.member x s
--                            then Just $ length s
--                            else dup' xs (Set.insert x s)

-- -------------PRETTY-------------------------------------------------------------------

-- prettyPosition :: BNFC'Position -> String
-- prettyPosition (Just (row, col)) = "position " ++ show row ++ ":" ++ show col
-- prettyPosition _ = "???"

-- prettyIdent :: Ident -> String
-- prettyIdent (Ident x) = x

-- prettyType :: Type -> String
-- prettyType (Int _) = "int"
-- prettyType (Str _) = "string"
-- prettyType (Bool _) = "bool"
-- prettyType (Void _) = "void"


-- ------------------------------------TYPE MANIP--------------------------------------------
class Raw a where
    raw :: a -> a


instance Raw PrimType where
    raw (Int _) = rawInt
    raw (Str _) = rawStr
    raw (Bool _) = rawBool
    raw (Void _) = rawVoid

instance Raw Type where
    raw (Primitive _ t) = Primitive Nothing (raw t)
    raw (ObjectType _ ident) = ObjectType Nothing ident
    raw (Array _ t) = Array Nothing (raw t)
    raw (Fun _ t ts) = Fun Nothing (raw t) (map raw ts)
instance Raw ExtIdent where
    raw :: ExtIdent -> ExtIdent
    raw (Id _ ident) = Id Nothing ident
    raw (ArrId _ ident e) = ArrId Nothing ident (raw e)
    raw (AttrId _ e1 e2) = AttrId Nothing (raw e1) (raw e2)

instance Raw Expr where 
    raw :: Expr -> Expr   
    raw (ECast _ ident e) = ECast Nothing ident (raw e) 
    raw (ECastPrim _ t e) = ECastPrim Nothing (raw t) (raw e)
    raw (ENewObject _ ident) = ENewObject Nothing ident
    raw (ENewArr _ t e) = ENewArr Nothing (raw t) (raw e) 
    raw (ENull _) = ENull Nothing 
    raw (EObject _ e1 e2) = EObject Nothing (raw e1) (raw e2)
    raw (EArr _ ident e) = EArr Nothing ident (raw e) 
    raw (EVar _ ident) = EVar Nothing ident
    raw (ELitInt _ val) = ELitInt Nothing val 
    raw (ELitTrue _) = ELitTrue Nothing
    raw (ELitFalse _) = ELitFalse Nothing
    raw (EApp _ ident es) = EApp Nothing ident (map raw es)
    raw (EString _ s) = EString Nothing s 
    raw (Neg _ e) = Neg Nothing (raw e)
    raw (Not _ e) = Not Nothing (raw e) 
    raw (EMul _ e1 op e2) = EMul Nothing (raw e1) (raw op) (raw e2)
    raw (EAdd _ e1 op e2) = EAdd Nothing (raw e1) (raw op) (raw e2)
    raw (ERel _ e1 op e2) = ERel Nothing (raw e1) (raw op) (raw e2)
    raw (EAnd _ e1 e2) = EAnd Nothing (raw e1) (raw e2)
    raw (EOr _ e1 e2) = EOr Nothing (raw e1) (raw e2)
instance Raw AddOp where
    raw (Plus _) = Plus Nothing
    raw (Minus _) = Minus Nothing

instance Raw MulOp where
    raw (Times _) = Times Nothing
    raw (Div _) = Div Nothing
    raw (Mod _) = Mod Nothing

instance Raw RelOp where
    raw (LTH _) = LTH Nothing
    raw (LE a) = LE Nothing
    raw (GTH a) = GTH Nothing 
    raw (GE a) = GE Nothing 
    raw (EQU a) = EQU Nothing
    raw (NE a) = NE Nothing

rawInt = Int Nothing
rawStr = Str Nothing
rawBool = Bool Nothing
rawVoid = Void Nothing

rawExtIdent :: Ident -> ExtIdent
rawExtIdent = Id Nothing