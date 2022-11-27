module Typechecker.TypeChecker where

import Syntax.AbsLattepp
import Control.Applicative.Lift ()
import Control.Monad ( unless, when, zipWithM_ )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class ()
import Control.Monad.Trans.Except ( ExceptT )
import Control.Monad.Trans.State (evalStateT, get, gets, put)
import Data.List (elemIndex)
import Debug.Trace (trace)
import Typechecker.Data
import Utils
import qualified Data.Set as S
import qualified Data.Map as M

typeCheck :: Program -> ExceptT String IO ()
typeCheck (Program _ defs) =
   evalStateT (mapM_ typeCheckTopDef defs) initTypeCheckerS

typeCheckTopDef :: TopDef -> TypeCheckerState ()
typeCheckTopDef (FnDef pos ret ident args block) = do
   typeCheckBlock block

typeCheckTopDef (ClassDef pos ident block) = return ()
typeCheckTopDef (ExtClassDef pos ident ext block) = return ()

typeCheckBlock :: Block -> TypeCheckerState ()
typeCheckBlock (Block pos stmts) = do
   mapM_ typeCheckStmt stmts

typeCheckClassBlock :: ClassBlock -> TypeCheckerState ()
typeCheckClassBlock (ClassBlock pos stmts) = return ()

typeCheckClassStmt :: ClassStmt -> TypeCheckerState ()
typeCheckClassStmt (ClassEmpty pos) = return ()
typeCheckClassStmt (ClassDecl pos t item) = return ()
typeCheckClassStmt (ClassMethod pos ret ident args block) = return ()

typeCheckExtIdent :: ExtIdent -> TypeCheckerState ()
typeCheckExtIdent (Id pos ident) = return ()
typeCheckExtIdent (ArrId pos ident expr) = return  ()
typeCheckExtIdent (AttrId pos expr1 expr2) = return  ()

typeCheckStmt :: Stmt -> TypeCheckerState ()
typeCheckStmt (Empty pos) = return ()
typeCheckStmt (BStmt pos block) = return ()
typeCheckStmt (Decl pos t item) = return ()
typeCheckStmt (Ass pos ident expr) = return ()
typeCheckStmt (Incr pos ident) = return ()
typeCheckStmt (Decr pos ident) = return ()
typeCheckStmt (Ret pos expr) = return ()
typeCheckStmt (VRet pos) = return ()
typeCheckStmt (Cond pos expr stmt) = return ()
typeCheckStmt (CondElse pos expr istmt estmt) = return ()
typeCheckStmt (While pos expr stmt) = return ()
typeCheckStmt (For pos t ident collection stmt) = return ()

typeCheckStmt (SExp pos expr) = do
   val <- typeCheckExpr expr
   return ()

typeCheckItem :: Item -> TypeCheckerState ()
typeCheckItem (NoInit pos ident) = return ()
typeCheckItem (Init pos ident expr) = return ()

typeCheckExpr :: Expr -> TypeCheckerState Type
typeCheckExpr (ECast pos ident expr) = do
   ensureTypeExists (ObjectType pos ident)
   type1 <- typeCheckExpr expr
   
typeCheckExpr (ECastPrim pos t expr) = return $ Primitive pos (Int pos)
typeCheckExpr (ENewObject pos type1) = do
   st <- get
   case M.lookup type1 (classEnv st) of
      Nothing -> throwException $ UndefinedTypeException pos (ObjectType pos type1)
      Just _ -> return $ ObjectType pos type1

typeCheckExpr (ENewArr pos t expr) = do
   ensureType rawInt expr
   ensureTypeExists t
   return t

typeCheckExpr (ENull pos) = return $ Primitive pos (Void pos)

typeCheckExpr (EObject pos expr1 expr2) = do -- do naprawy
   type1 <- typeCheckExpr expr1
   ensureObject pos type1
   return $ Primitive pos (Void pos)


typeCheckExpr (EArr pos ident expr) = do
   st <- get
   case M.lookup ident (typeEnv st) of
     Nothing -> throwException $ UndefinedVariableException pos ident
     Just t -> do
      ensureType rawInt expr
      ensureArray pos t
      return t

typeCheckExpr (EVar pos ident) = do
  st <- get
  case M.lookup ident (typeEnv st) of
    Nothing -> throwException $ UndefinedVariableException pos ident
    Just t -> return t

typeCheckExpr (ELitInt pos val) = return $ Primitive pos (Int pos)

typeCheckExpr (ELitTrue pos) = return $ Primitive pos (Bool pos)

typeCheckExpr (ELitFalse pos) = return$ Primitive pos (Bool pos)

typeCheckExpr (EApp pos ident exprs) = do
  st <- get
  case M.lookup ident (funEnv st) of
    Nothing -> throwException $ UndefinedFunctionException pos ident
    Just (rType, args) -> do
      ensureArgTypes pos args exprs
      return rType

typeCheckExpr (EString pos s) = return $ Primitive pos (Str pos)

typeCheckExpr (Neg pos expr) = do
   type1 <- ensureType rawInt expr
   return $ Primitive pos (Int pos)

typeCheckExpr (Not pos expr) = do
   type1 <- ensureType rawBool expr
   return $ Primitive pos (Bool pos)

typeCheckExpr (EMul pos expr1 op expr2) = do
  type1 <- ensureType rawInt expr1
  type2 <- ensureType rawInt expr2
  return $ Primitive pos (Int pos)

typeCheckExpr (EAdd pos expr1 op expr2) = do
  type1 <- allowTypes [rawInt, rawStr] expr1
  type2 <- allowTypes [rawInt, rawStr] expr2
  ensureTypeMatch pos type1 type2
  case type1 of
   Primitive _ (Int _) -> return $ Primitive pos (Int pos)
   Primitive _ (Str _) -> return $ Primitive pos (Str pos)
   _ -> undefined

typeCheckExpr (ERel pos expr1 op expr2) = do
   type1 <- ensureType rawInt expr1
   type2 <- ensureType rawInt expr2
   return $ Primitive pos (Bool pos)

typeCheckExpr (EAnd pos expr1 expr2) = do
   type1 <- ensureType rawBool expr1
   type2 <- ensureType rawBool expr2
   return $ Primitive pos (Bool pos)

typeCheckExpr (EOr pos expr1 expr2) = do
   type1 <- ensureType rawBool expr1
   type2 <- ensureType rawBool expr2
   return $ Primitive pos (Bool pos)


---------------------------------------------------------------------------

-- --------------------ENSURE------------------------------------------------------
ensureArray :: BNFC'Position -> Type -> TypeCheckerState ()
ensureArray pos type1 = do
   case type1 of
     Array _ _ -> return ()
     _ -> throwException $ InvalidExpectedArrayException pos

ensureObject :: BNFC'Position -> Type -> TypeCheckerState ()
ensureObject pos type1 = do
   case type1 of
     ObjectType _ _ -> return ()
     _ -> throwException $ InvalidExpectedObjectException pos

ensureTypeMatch :: BNFC'Position -> Type -> Type -> TypeCheckerState ()
ensureTypeMatch pos type1 type2 =
  if raw type1 == raw type2
    then return ()
    else throwException $ InvalidTypeExpectedException pos type2 type1

ensureType :: Type -> Expr -> TypeCheckerState ()
ensureType t expr = do
  eType <- typeCheckExpr expr
  ensureTypeMatch (hasPosition eType) t eType

allowTypes :: [Type] -> Expr -> TypeCheckerState Type
allowTypes ts expr = do
   eType <- typeCheckExpr expr
   _allowTypes (hasPosition  eType) ts eType
   return eType

_allowTypes :: BNFC'Position -> [Type] -> Type -> TypeCheckerState ()
_allowTypes pos ts type1 = 
   case type1 `elemIndex` ts of
      Nothing -> throwException $ InvalidTypeException pos type1
      Just _ -> return () 

ensureArgTypes :: BNFC'Position -> [Type] -> [Expr] -> TypeCheckerState ()
ensureArgTypes pos ts exprs =
  if length ts /= length exprs
    then throwException $ InvalidNumberOfParametersException pos
    else zipWithM_ ensureType ts exprs

ensureUniqueIdents :: [Arg] -> TypeCheckerState ()
ensureUniqueIdents args =
  case firstDuplicateIndex $ map getArgIdent args of
    Just index -> throwException $ ArgumentRedeclarationException (hasPosition $ args !! index) (getArgIdent $ args !! index)
    Nothing -> return ()

ensureTypeExists :: Type -> TypeCheckerState ()
ensureTypeExists type1 = 
   case type1 of
     Primitive _ _ -> dontAllowVoid type1
     ObjectType pos ident -> do
      st <- get
      case M.lookup ident (classEnv st) of
        Nothing -> throwException $ UndefinedTypeException pos type1
        Just _ -> return ()
     Array _ type2 -> ensureTypeExists type2
     _ -> undefined 


-- -------------DONT ALLOW---------------------------------------------------------------
dontAllowTypeMatch :: BNFC'Position -> Type -> Type -> TypeCheckerState ()
dontAllowTypeMatch pos type1 type2 =
  if raw type1 /= raw type2
    then return ()
    else throwException $ InvalidTypeException pos type2

dontAllowTypes :: [Type] -> Expr -> TypeCheckerState Type
dontAllowTypes ts expr = do
  eType <- typeCheckExpr expr
  mapM_ (dontAllowTypeMatch (hasPosition eType) eType) ts
  return eType

dontAllowType :: Type -> Expr -> TypeCheckerState Type
dontAllowType t expr = do
  eType <- typeCheckExpr expr
  dontAllowTypeMatch (hasPosition eType) t eType
  return t

dontAllowVoidArgs :: [Type] -> TypeCheckerState ()
dontAllowVoidArgs = mapM_ dontAllowVoid

dontAllowVoid :: Type -> TypeCheckerState ()
dontAllowVoid t = 
   when (raw t == rawVoid) $ throwException $ VoidNotAllowedException (hasPosition t)

-- dontAllowFun t ident =
--   when (raw t == rawFun) $ throwException $ FunctionNotDefinedException (hasPosition t) ident
------------------FUNCTION STATE-----------------------------
functionState :: TypeCheckerS -> [Arg] -> Type -> TypeCheckerS
functionState s args rType = setTypes (emptyScope $ setExpectedReturnType s $ Just rType)
                              (map getArgIdent args) (map getArgType args)

---------------------------------------------------------------------------

checkReturns :: Program -> ExceptT String IO ()
checkReturns (Program _ stmts) = return ()




















----------------FOR FUTURE USE ------------------------------

--  :: Program -> ExceptT String IO ()
-- typeCheck (Program _ defs) = 
--    evalStateT (mapM_ typeCheckTopDef defs) initTypeCheckerS

-- TopDef :: TopDef -> TypeCheckerState ()
-- TopDef (FnDef pos ret ident args block) = return  ()
-- TopDef (ClassDef pos ident block) = return ()
-- TopDef (ExtClassDef pos ident ext block) = return ()
-- Block :: Block -> TypeCheckerState ()
-- Block (Block pos stmts) = return ()
-- ClassBlock :: ClassBlock -> TypeCheckerState ()
-- ClassBlock (ClassBlock pos stmts) = return ()
-- ClassStmt :: ClassStmt -> TypeCheckerState ()
-- ClassStmt (ClassEmpty pos) = return ()
-- ClassStmt (ClassDecl pos t item) = return ()
-- ClassStmt (ClassMethod pos ret ident args block) = return  ()
-- ExtIdent :: ExtIdent -> TypeCheckerState ()
-- ExtIdent (Id pos ident) = return ()
-- ExtIdent (ArrId pos ident expr) = return  ()
-- ExtIdent (AttrId pos expr1 expr2) = return  ()
-- Stmt :: Stmt -> TypeCheckerState ()
-- Stmt (Empty pos) = return ()
-- Stmt (BStmt pos block) = return ()
-- Stmt (Decl pos t item) = return ()
-- Stmt (Ass pos ident expr) = return ()
-- Stmt (Incr pos ident) = return ()
-- Stmt (Decr pos ident) = return ()
-- Stmt (Ret pos expr) = return ()
-- Stmt (VRet pos) = return ()
-- Stmt (Cond pos expr stmt) = return ()
-- Stmt (CondElse pos expr istmt estmt) = return ()
-- Stmt (While pos expr stmt) = return ()
-- Stmt (For pos t ident collection stmt) = return ()
-- Stmt (SExp pos expr) = return ()
-- Item :: Item -> TypeCheckerState ()
-- Item (NoInit pos ident) = return ()
-- Item (Init pos ident expr) = return ()
-- Expr :: Expr -> TypeCheckerState Type
-- Expr (ECast pos ident expr) = return $ Primitive pos (Int pos)
-- Expr (ECastPrim pos t expr) = return $ Primitive pos (Int pos)
-- Expr (ENewObject pos ident) = return $ Primitive pos (Int pos)
-- Expr (ENewArr pos t expr) = return $ Primitive pos (Int pos)
-- Expr (ENull pos) = return $ Primitive pos (Int pos)
-- Expr (EObject pos expr1 expr2) = return $ Primitive pos (Int pos)
-- Expr (EArr pos ident expr) = return $ Primitive pos (Int pos)
-- Expr (EVar pos ident) = return $ Primitive pos (Int pos)
-- Expr (ELitInt pos val) = return $ Primitive pos (Int pos)
-- Expr (ELitTrue pos) = return $ Primitive pos (Int pos)
-- Expr (ELitFalse pos) = return$ Primitive pos (Int pos)
-- Expr (EApp pos ident expr) = return $ Primitive pos (Int pos)
-- Expr (EString pos s) = return $ Primitive pos (Int pos)
-- Expr (Neg pos expr) = return $ Primitive pos (Int pos)
-- Expr (Not pos expr) = return $ Primitive pos (Int pos)
-- Expr (EMul pos expr1 op expr2) = return $ Primitive pos (Int pos)
-- Expr (EAdd pos expr1 op expr2) = return $ Primitive pos (Int pos)
-- Expr (ERel pos expr1 op expr2) = return $ Primitive pos (Int pos)
-- Expr (EAnd pos expr1 expr2) = return $ Primitive pos (Int pos)
-- Expr (EOr pos expr1 expr2) = return $ Primitive pos (Int pos)