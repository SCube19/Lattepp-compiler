module Typechecker.TypeChecker where

import Syntax.AbsLattepp
import Control.Applicative.Lift ()
import Control.Monad ( unless, when, zipWithM_ )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class ()
import Control.Monad.Trans.Except ( ExceptT )
import Control.Monad.Trans.State (evalStateT, get, gets, put)
import Data.List ()
import Debug.Trace (trace)
import Typechecker.TypeCheckerData
import Utils
import qualified Data.Set as S

typeCheck :: Program -> ExceptT String IO ()
typeCheck (Program _ stmts) = return ()
  -- evalStateT (typeCheckBlock stmts) initTypeCheckerS

checkReturns :: Program -> ExceptT String IO ()
checkReturns (Program _ stmts) = return ()

-- typeCheckBlock :: [Stmt] -> TypeCheckerState ()
-- typeCheckBlock stmts = do
--   env <- get
--   localTypeEnv (emptyScope env) (mapM_ typeCheckStmt stmts)

-- --------------------------------STMT------------------------------------------------------------------------------
-- typeCheckStmt :: Stmt -> TypeCheckerState ()
-- typeCheckStmt (Empty _) = return ()

-- typeCheckStmt (BStmt _ (Block _ stmts)) = typeCheckBlock stmts

-- typeCheckStmt (Decl pos t item) = typeCheckDecl pos t item False

-- typeCheckStmt (ConstDecl pos t item) = typeCheckDecl pos t item True

-- typeCheckStmt (Ass pos ident expr) = do
--   env <- get
--   case getType env ident of
--     Nothing -> throwException $ UndefinedException pos ident
--     Just t -> do
--       if snd t
--         then throwException $ ImmutableCannotBeChangedException pos ident
--         else ensureType (fst t) expr

-- typeCheckStmt (Incr pos ident) =
--   typeCheckStmt (Ass pos ident (EAdd pos (ELitInt pos 1) (Plus pos) (EVar pos ident)))

-- typeCheckStmt (Decr pos ident) =
--   typeCheckStmt (Ass pos ident (EAdd pos (ELitInt pos 1) (Minus pos) (EVar pos ident)))

-- typeCheckStmt (Ret pos expr) = do
--   env <- get
--   case expectedReturnType env of
--     Nothing -> throwException $ InvalidReturnException pos
--     Just t -> ensureType t expr

-- typeCheckStmt (VRet pos) = do
--   env <- get
--   case expectedReturnType env of
--     Nothing -> throwException $ InvalidReturnException pos
--     Just t -> ensureTypeMatch pos rawVoid t

-- typeCheckStmt (Cond pos expr stmt) = do
--   dontAllowTypes [rawFun, rawVoid, rawChar, rawStr] expr
--   env <- get
--   localTypeEnv (emptyScope env) (typeCheckStmt stmt)

-- typeCheckStmt (CondElse pos expr (Block _ ifBlock) (Block _ elseBlock)) = do
--   dontAllowTypes [rawFun, rawVoid, rawChar, rawStr] expr
--   typeCheckBlock ifBlock
--   typeCheckBlock elseBlock

-- typeCheckStmt (While pos expr stmt) = do
--   dontAllowTypes [rawFun, rawVoid, rawChar, rawStr] expr
--   env <- get
--   localTypeEnv (emptyScope $ setInsideLoop env True) (typeCheckStmt stmt)

-- typeCheckStmt (For pos ident from to stmt) = do
--   ensureType rawInt from
--   ensureType rawInt to
--   env <- get
--   localTypeEnv (setType (emptyScope $ setInsideLoop env True) ident (Int pos, True)) (typeCheckStmt stmt)

-- typeCheckStmt (Print pos expr) = do
--   dontAllowTypes [rawFun, rawVoid] expr
--   return ()

-- typeCheckStmt (PrintLn pos expr) = typeCheckStmt (Print pos expr)

-- typeCheckStmt (Break pos) = do
--   env <- get
--   unless (insideLoop env) (throwException $ InvalidExitException pos)

-- typeCheckStmt (Continue pos) = do
--   env <- get
--   unless (insideLoop env) (throwException $ InvalidSkipException pos)

-- typeCheckStmt (SExp _ expr) = do
--   typeCheckExpr expr
--   return ()

-- -----------------DECL-------------------------------------------------------------------------------
-- typeCheckDecl :: BNFC'Position -> Type -> Item -> Bool -> TypeCheckerState ()
-- typeCheckDecl pos t (NoInit _ ident) True = throwException $ ImmutableNotInitializedException pos ident

-- typeCheckDecl pos t (NoInit _ ident) False = do
--   dontAllowVoid t
--   dontAllowFun t ident 
--   env <- get
--   case getType env ident of
--     Nothing -> put $ setType env ident (t, False)
--     Just _ -> if S.member ident (scope env)
--                   then throwException $ RedeclarationException pos ident
--                   else put $ setType env ident (t, False)

-- typeCheckDecl pos t (Init _ ident expr) isImmutable = do
--   env <- get
--   dontAllowVoid t
--   case getType env ident of
--     Nothing -> put $ setType env ident (t, isImmutable)
--     Just _ -> when (S.member ident (scope env)) $ throwException $ RedeclarationException pos ident
--   eType <- typeCheckExpr expr
--   ensureTypeMatch pos t eType
  
-- ---------------EXPR---------------------------------------------------------------------------------------
-- typeCheckExpr :: Expr -> TypeCheckerState Type
-- typeCheckExpr (EVar pos ident) = do
--   env <- get
--   case getType env ident of
--     Nothing -> throwException $ UndefinedException pos ident
--     Just t -> return $ fst t

-- typeCheckExpr (EApp pos ident exprs) = do
--   env <- get
--   case getType env ident of
--     Nothing -> throwException $ UndefinedException pos ident
--     Just (Fun _ args rType, _) -> do
--       ensureArgTypes pos args exprs
--       return rType
--     Just (t, _) -> throwException $ InvalidFunctionApplicationException pos ident t

-- typeCheckExpr (Neg pos expr1) = do
--   type1 <- ensureType rawInt expr1
--   return (Int pos)

-- typeCheckExpr (EAdd pos expr1 (Minus _) expr2) = do
--   type1 <- ensureType rawInt expr1
--   type2 <- ensureType rawInt expr2
--   return (Int pos)

-- typeCheckExpr (EAdd pos expr1 (Plus _) expr2) = do
--   type1 <- dontAllowTypes [rawBool, rawChar, rawFun, rawVoid] expr1
--   type2 <- dontAllowTypes [rawBool, rawChar, rawFun, rawVoid] expr2
--   ensureTypeMatch pos type1 type2
--   return $ case type1 of
--     (Str _) -> Str pos
--     _ -> Int pos

-- typeCheckExpr (EMul pos expr1 _ expr2) = do
--   type1 <- ensureType rawInt expr1
--   type2 <- ensureType rawInt expr2
--   return (Int pos)

-- typeCheckExpr (Not pos expr1) = do
--   type1 <- dontAllowTypes [rawStr, rawChar, rawFun, rawVoid] expr1
--   return (Bool pos)

-- typeCheckExpr (ERel pos expr1 _ expr2) = do
--   type1 <- dontAllowTypes [rawFun, rawVoid] expr1
--   type2 <- dontAllowTypes [rawFun, rawVoid] expr2
--   ensureTypeMatch pos type1 type2
--   return (Bool pos)

-- typeCheckExpr (EAnd pos expr1 expr2) = do
--   type1 <- dontAllowTypes [rawChar, rawStr, rawFun, rawVoid] expr1
--   type2 <- dontAllowTypes [rawChar, rawStr, rawFun, rawVoid] expr2
--   return (Bool pos)

-- typeCheckExpr (EOr pos expr1 expr2) = do
--   type1 <- dontAllowTypes [rawChar, rawStr, rawFun, rawVoid] expr1
--   type2 <- dontAllowTypes [rawChar, rawStr, rawFun, rawVoid] expr2
--   return (Bool pos)

-- typeCheckExpr (ELambda pos args rType (Block _ stmts)) = do
--   dontAllowVoidArgs $ map getArgType args
--   ensureUniqueIdents args
--   env <- get
--   localTypeEnv (functionState env args rType) (typeCheckBlock stmts)
--   return $ Fun pos (map getArgType args) rType

-- typeCheckExpr (ELitInt pos _) = return $ Int pos
-- typeCheckExpr (ELitChar pos _) = return $ Char pos
-- typeCheckExpr (EString pos _) = return $ Str pos
-- typeCheckExpr (ELitTrue pos) = return $ Bool pos
-- typeCheckExpr (ELitFalse pos) = return $ Bool pos

-- --------------------ENSURE------------------------------------------------------
-- ensureTypeMatch :: BNFC'Position -> Type -> Type -> TypeCheckerState ()
-- ensureTypeMatch pos type1 type2 =
--   if raw type1 == raw type2
--     then return ()
--     else throwException $ InvalidTypeExpectedException pos type2 type1

-- ensureType :: Type -> Expr -> TypeCheckerState ()
-- ensureType t expr = do
--   eType <- typeCheckExpr expr
--   ensureTypeMatch (hasPosition eType) t eType

-- ensureArgTypes :: BNFC'Position -> [ArgType] -> [Expr] -> TypeCheckerState ()
-- ensureArgTypes pos ts exprs =
--   if length ts /= length exprs
--     then throwException $ InvalidNumberOfParametersException pos
--     else zipWithM_ ensureArgType ts exprs

-- ensureArgType :: ArgType -> Expr -> TypeCheckerState ()
-- ensureArgType (Val _ t) expr = do
--   ensureType t expr

-- ensureArgType (Ref pos t) expr = do
--   case expr of
--     (EVar pos ident) -> do
--       env <- get
--       case getType env ident of
--         Nothing -> throwException $ UndefinedException pos ident
--         Just t1 -> if snd t1 then throwException $ ConstReferenceException pos else ensureTypeMatch (hasPosition $ fst t1) t (fst t1)
--     _ -> throwException $ ReferenceNotVariableException pos

-- ensureUniqueIdents :: [Arg] -> TypeCheckerState ()
-- ensureUniqueIdents args =
--   case firstDuplicateIndex $ map getArgIdent args of
--     Just index -> throwException $ RedeclarationException (hasPosition . getArgType $ args !! index) (getArgIdent $ args !! index)
--     Nothing -> return ()

-- -------------DONT ALLOW---------------------------------------------------------------
-- dontAllowTypeMatch :: BNFC'Position -> Type -> Type -> TypeCheckerState ()
-- dontAllowTypeMatch pos type1 type2 =
--   if cleanRaw type1 /= cleanRaw type2
--     then return ()
--     else throwException $ InvalidTypeException pos type2

-- dontAllowTypes :: [Type] -> Expr -> TypeCheckerState Type
-- dontAllowTypes ts expr = do
--   eType <- typeCheckExpr expr
--   mapM_ (dontAllowTypeMatch (hasPosition eType) eType) ts
--   return eType

-- dontAllowType :: Type -> Expr -> TypeCheckerState Type
-- dontAllowType t expr = do
--   eType <- typeCheckExpr expr
--   dontAllowTypeMatch (hasPosition eType) t eType
--   return t

-- dontAllowVoidArgs :: [ArgType] -> TypeCheckerState ()
-- dontAllowVoidArgs = mapM_ (dontAllowVoid . toType)

-- dontAllowVoid :: Type -> TypeCheckerState ()
-- dontAllowVoid (Fun _ args _) =
--   dontAllowVoidArgs args

-- dontAllowVoid t =
--   when (raw t == rawVoid) $ throwException $ VoidNotAllowedException (hasPosition t)

-- dontAllowFun :: Type -> Ident -> TypeCheckerState ()
-- dontAllowFun t ident =
--   when (cleanRaw t == rawFun) $ throwException $ FunctionNotDefinedException (hasPosition t) ident
-- ------------------FUNCTION STATE-----------------------------
-- functionState :: TypeCheckerS -> [Arg] -> Type -> TypeCheckerS
-- functionState s args rType = setTypes (emptyScope $ setExpectedReturnType s $ Just rType)
--                               (map getArgIdent args)
--                               (map (makeFalsyTuple . toType . getArgType) args)