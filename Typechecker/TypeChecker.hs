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
import Typechecker.Utils (gatherHeaders, defineInheritance, checkMain, dontAllowVoid, gatherFields, functionState, classState, topoSort)
import Cmm (CmmNode(res))

typeCheck :: Program -> ExceptT String IO ()
typeCheck p =
   evalStateT (beginCheck p) initTypeCheckerS

beginCheck :: Program -> TypeCheckerState ()
beginCheck (Program _ defs) = do
   mapM_ gatherHeaders defs
   st1 <- get
   checkMain st1
   mapM_ defineInheritance defs
   st2 <- get
   newDefs <- topoSort defs
   mapM_ gatherFields newDefs
   mapM_ typeCheckTopDef defs

----------------------------------------------------

typeCheckTopDef :: TopDef -> TypeCheckerState ()
typeCheckTopDef (FnDef pos ret ident args block) = do
   st <- get
   localTypeEnv (functionState st args ret) (typeCheckBlock block)

typeCheckTopDef (ClassDef pos ident block) = do
   st <- get
   localTypeEnv (classState st (Just ident)) (typeCheckClassBlock block)

typeCheckTopDef (ExtClassDef pos ident ext block) = do
   st <- get
   localTypeEnv (classState st (Just ident)) (typeCheckClassBlock block)

-----------------------------------------------------------
typeCheckBlock :: Block -> TypeCheckerState ()
typeCheckBlock (Block pos stmts) = do
  st <- get
  localTypeEnv (emptyScope st) (mapM_ typeCheckStmt stmts)

-----------------------------------------------------------------

typeCheckClassBlock :: ClassBlock -> TypeCheckerState ()
typeCheckClassBlock (ClassBlock pos stmts) = do
   mapM_ typeCheckClassStmt stmts

--------------------------------------------------------------------
typeCheckClassStmt :: ClassStmt -> TypeCheckerState ()
typeCheckClassStmt (ClassEmpty pos) = return ()

typeCheckClassStmt (ClassDecl pos t items) = return ()

typeCheckClassStmt (ClassMethod pos ret ident args block) = do
   st <- get
   localTypeEnv (functionState st args ret) (typeCheckBlock block)

------------------------------------------------------------------------------

typeCheckExtIdent :: ExtIdent -> TypeCheckerState Type
typeCheckExtIdent (Id pos ident) = typeCheckExpr (EVar pos ident)

typeCheckExtIdent (ArrId pos ident expr) = typeCheckExpr (EArr pos ident expr)

typeCheckExtIdent (AttrId pos expr1 expr2) = do
   st <- get
   localTypeEnv (setEnforceAttr st True) (typeCheckExpr (EObject pos expr1 expr2))

-----------------------------------------------------------------------------------

typeCheckStmt :: Stmt -> TypeCheckerState ()
typeCheckStmt (Empty pos) = return ()

typeCheckStmt (BStmt pos block) = typeCheckBlock block

typeCheckStmt (Decl pos t items) = mapM_ (`typeCheckItem` t) items

typeCheckStmt (Ass pos ident expr) = do
  st <- get
  iType <- typeCheckExtIdent ident
  ensureType iType expr

typeCheckStmt (Incr pos ident) =
   typeCheckStmt (Ass pos ident (EAdd pos (ELitInt pos 1) (Plus pos) (case ident of
      var@(Id pos2 i) -> EVar pos2 i
      arr@(ArrId pos2 i e) -> EArr pos2 i e
      attr@(AttrId pos2 e1 e2) -> EObject pos2 e1 e2
   )))

typeCheckStmt (Decr pos ident) =
  typeCheckStmt (Ass pos ident (EAdd pos (ELitInt pos 1) (Minus pos) (case ident of
      var@(Id pos2 i) -> EVar pos2 i
      arr@(ArrId pos2 i e) -> EArr pos2 i e
      attr@(AttrId pos2 e1 e2) -> EObject pos2 e1 e2
   )))

typeCheckStmt (Ret pos expr) = do
  st <- get
  case expectedReturnType st of
    Nothing -> throwException $ WildCardException pos
    Just t -> do
      eType <- typeCheckExpr expr
      if isSameType t eType then
         return ()
      else
         throwException $ InvalidReturnTypeException pos eType t

typeCheckStmt (VRet pos) = do
  st <- get
  case expectedReturnType st of
    Nothing -> throwException $ WildCardException pos
    Just t -> do
      if isSameType t rawVoid then
         return ()
      else
         throwException $ InvalidReturnTypeException pos rawVoid t

typeCheckStmt (Cond pos expr stmt) = do
  ensureType rawBool expr
  st <- get
  localTypeEnv (emptyScope st) (typeCheckStmt stmt)

typeCheckStmt (CondElse pos expr istmt estmt) = do
  ensureType rawBool expr
  st <- get
  localTypeEnv (emptyScope st) (typeCheckStmt istmt)
  localTypeEnv (emptyScope st) (typeCheckStmt estmt)

typeCheckStmt (While pos expr stmt) = do
   ensureType rawBool expr
   st <- get
   localTypeEnv (emptyScope st) (typeCheckStmt stmt)

typeCheckStmt (For pos t ident collection stmt) = do
   colType <- typeCheckExtIdent collection
   ensureArray pos colType
   case colType of
      Array pos2 t2 -> do
         ensureTypeMatch pos t t2
         st <- get
         localTypeEnv (setType (emptyScope st) ident t) (typeCheckStmt stmt)
      _ -> throwException $ WildCardException pos

typeCheckStmt (SExp pos expr) = do
   val <- typeCheckExpr expr
   return ()

-------------------------------------------------------------------------------------------

typeCheckItem :: Item -> Type -> TypeCheckerState ()
typeCheckItem (NoInit pos ident) type1 =
  if checkPrimitive type1 then do
   dontAllowVoid type1
   st <- get
   case M.lookup ident (typeEnv st) of
      Nothing -> put $ setType st ident type1
      Just _ ->
         if S.member ident (scope st)
            then throwException $ VariableRedeclarationException pos ident
            else put $ setType st ident type1
   else
      throwException $ NoInitException pos type1

typeCheckItem (Init pos ident expr) type1 = do
  st <- get
  dontAllowVoid type1
  case M.lookup ident (typeEnv st) of
    Nothing -> put $ setType st ident type1
    Just _ -> 
      if S.member ident (scope st)
            then throwException $ VariableRedeclarationException pos ident
            else put $ setType st ident type1
  eType <- typeCheckExpr expr
  ensureTypeMatch pos type1 eType

-----------------------------------------------------------------------------

typeCheckExpr :: Expr -> TypeCheckerState Type
typeCheckExpr (ECast pos ident expr) = do
   let castType = ObjectType pos ident
   ensureTypeExists castType
   eType <- typeCheckExpr expr
   case eType of
      Primitive _ (Void _) -> return castType
      ObjectType _ _ -> do
         findParent pos castType eType
         return castType
      _ -> throwException $ InvalidCastException pos eType castType

typeCheckExpr (ECastPrim pos castType expr) = do
   eType <- typeCheckExpr expr
   throwException $ InvalidCastException pos eType (Primitive pos castType)

typeCheckExpr (ENewObject pos type1) = do
   st <- get
   case M.lookup type1 (classEnv st) of
      Nothing -> throwException $ UndefinedTypeException pos (ObjectType pos type1)
      Just _ -> return $ ObjectType pos type1

typeCheckExpr (ENewArr pos t expr) = do
   ensureType rawInt expr
   ensureTypeExists t
   return $ Array pos t

typeCheckExpr (ENull pos) = return $ Primitive pos (Void pos)

typeCheckExpr (EObject pos expr1 expr2) = do
   st <- get
   case objectCheck st of
     Nothing -> do
      type1 <- typeCheckExpr expr1
      ensureObject pos type1
      put $ setObjectCheck st (Just type1)
      typeCheckExpr expr2
     Just t -> do
      case expr1 of
         EVar _ ident -> do
            case findAttr st t ident of
               Nothing -> throwException $ UndefinedObjectFieldException pos ident
               Just t -> do
                  ensureObject pos t
                  put $ setObjectCheck st (Just t)
                  typeCheckExpr expr2
         EApp _ ident exprs -> do
            case findMethod st t ident of
               Nothing -> throwException $ UndefinedObjectFieldException pos ident
               Just (rType, args) -> do
                  ensureObject pos rType
                  ensureArgTypes pos args exprs
                  put $ setObjectCheck st (Just rType)
                  typeCheckExpr expr2
         _ -> throwException $ ObjectFieldException pos


typeCheckExpr (EArr pos ident expr) = do
   st <- get
   case findVarInState st ident of
     Nothing -> throwException $ UndefinedVariableException pos ident
     Just t -> do
      ensureType rawInt expr
      ensureArray pos t
      case t of
        Array pos2 t -> return t
        _ -> throwException $ WildCardException pos

typeCheckExpr (EVar pos ident) = do
  st <- get
  case objectCheck st of
    Nothing ->
      case findVarInState st ident of
         Nothing -> throwException $ UndefinedVariableException pos ident
         Just t -> return t
    Just t ->
      case findAttr st t ident of
         Nothing -> throwException $ UndefinedObjectFieldException pos ident
         Just t -> do
            put $ setObjectCheck st Nothing
            return t


typeCheckExpr (ELitInt pos val) = return $ Primitive pos (Int pos)

typeCheckExpr (ELitTrue pos) = return $ Primitive pos (Bool pos)

typeCheckExpr (ELitFalse pos) = return$ Primitive pos (Bool pos)

typeCheckExpr (EApp pos ident exprs) = do
  st <- get
  case objectCheck st of
    Nothing ->
      case findFunInState st ident of
         Nothing -> throwException $ UndefinedFunctionException pos ident
         Just (rType, args) -> do
            ensureArgTypes pos args exprs
            return rType
    Just t ->
      if enforceAttr st then
         throwException $ ObjectFieldGetException pos
      else
         case findMethod st t ident of
               Nothing -> throwException $ UndefinedObjectFieldException pos ident
               Just (rType, args) -> do
                  put $ setObjectCheck st Nothing
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
   _ -> throwException $ WildCardException pos

typeCheckExpr (ERel pos expr1 op expr2) = do
   type1 <- allowTypes [rawInt, rawBool] expr1
   type2 <- allowTypes [rawInt, rawBool] expr2
   ensureTypeMatch pos type1 type2
   case type1 of
    Primitive _ (Int _) -> return $ Primitive pos (Bool pos)
    Primitive _ (Bool _) ->
      case op of
         EQU _ -> return $ Primitive pos (Bool pos)
         NE _ -> return $ Primitive pos (Bool pos)
         _ -> throwException $ WildCardException pos
    _ -> throwException $ WildCardException pos

typeCheckExpr (EAnd pos expr1 expr2) = do
   type1 <- ensureType rawBool expr1
   type2 <- ensureType rawBool expr2
   return $ Primitive pos (Bool pos)

typeCheckExpr (EOr pos expr1 expr2) = do
   type1 <- ensureType rawBool expr1
   type2 <- ensureType rawBool expr2
   return $ Primitive pos (Bool pos)


---------------------------------------------------------------------------
--This section can't really be its own module due to circular dependency
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

--had to be done this way for it to make some sense in code
checkPrimitive :: Type -> Bool
checkPrimitive (Primitive _ _) = True
checkPrimitive _ = False

ensureTypeMatch :: BNFC'Position -> Type -> Type -> TypeCheckerState ()
ensureTypeMatch pos type1 type2 = do
  if isSameType type1 type2
    then return ()
    else throwException $ InvalidTypeExpectedException pos type2 type1

isSameType :: Type -> Type -> Bool
isSameType type1 type2 = raw type1 == raw type2

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
   case raw type1 `elemIndex` ts of
      Nothing -> throwException $ InvalidTypeException pos type1
      Just _ -> return ()

ensureArgTypes :: BNFC'Position -> [Type] -> [Expr] -> TypeCheckerState ()
ensureArgTypes pos ts exprs =
  if length ts /= length exprs
    then throwException $ InvalidNumberOfParametersException pos
    else zipWithM_ ensureType ts exprs

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

-------------------------------------------------------------
findParent :: BNFC'Position -> Type -> Type -> TypeCheckerState ()
findParent pos p c = _findParent pos p c c

_findParent :: BNFC'Position -> Type -> Type -> Type -> TypeCheckerState ()
_findParent pos0 pToFind@(ObjectType pos1 class1) c@(ObjectType pos2 class2) original =
   if class1 == class2 then do
      return ()
   else do
      st <- get
      case M.lookup class2 (classEnv st) of
         Nothing -> throwException $ UndefinedTypeException pos2 c
         Just (parent, _) ->
            case parent of
            (Nothing, _, _) -> throwException $ InvalidCastException pos0 original pToFind
            (Just pid, _, _) ->
               if pid == class1 then
                  return ()
               else
                  _findParent pos0 pToFind (ObjectType Nothing pid) original
_findParent pos _ _ _ = throwException $ WildCardException pos
---------------------------------------------------------------------------


checkReturn :: Program -> ExceptT String IO ()
checkReturn (Program _ defs) = 
   evalStateT (mapM_ checkReturnTopDef defs) initTypeCheckerS

checkReturnTopDef :: TopDef -> TypeCheckerState ()
checkReturnTopDef (FnDef pos ret ident args block) = 
   if raw ret == rawVoid then
      return ()
   else do
      res <- checkReturnBlock block
      if res then
         return ()
      else
         throwException $ NoReturnException pos ident

checkReturnTopDef (ClassDef pos ident block) = do
   res <- checkReturnClassBlock block
   if res then
      return ()
   else
      throwException $ NoReturnException pos ident

checkReturnTopDef (ExtClassDef pos ident ext block) = do
   res <- checkReturnClassBlock block
   if res then
      return ()
   else
      throwException $ NoReturnException pos ident

checkReturnBlock :: Block -> TypeCheckerState Bool
checkReturnBlock (Block pos stmts) = do
   res <- mapM checkReturnStmt stmts
   return $ or res
   
checkReturnClassBlock :: ClassBlock -> TypeCheckerState Bool
checkReturnClassBlock (ClassBlock pos stmts) = do
   res <- mapM checkReturnClassStmt stmts
   return $ and res

checkReturnClassStmt :: ClassStmt -> TypeCheckerState Bool
checkReturnClassStmt (ClassEmpty pos) = return True

checkReturnClassStmt (ClassDecl pos t item) = return True
checkReturnClassStmt (ClassMethod pos ret ident args block) = do
   if raw ret == rawVoid then
      return True
   else checkReturnBlock block

-- checkReturnExtIdent :: ExtIdent -> TypeCheckerState ()
-- checkReturnExtIdent (Id pos ident) = return ()
-- checkReturnExtIdent (ArrId pos ident expr) = return  ()
-- checkReturnExtIdent (AttrId pos expr1 expr2) = return  ()

checkReturnStmt :: Stmt -> TypeCheckerState Bool
checkReturnStmt (Empty pos) = return False
checkReturnStmt (BStmt pos block) = checkReturnBlock block
checkReturnStmt (Decl pos t item) = return False
checkReturnStmt (Ass pos ident expr) = return False
checkReturnStmt (Incr pos ident) = return False
checkReturnStmt (Decr pos ident) = return False 
checkReturnStmt (Ret pos expr) = return True
checkReturnStmt (VRet pos) = return True
checkReturnStmt (Cond pos expr stmt) = return False
checkReturnStmt (CondElse pos expr istmt estmt) = do
   ir <- checkReturnStmt istmt
   er <- checkReturnStmt estmt
   return $ ir && er

checkReturnStmt (While pos expr stmt) = 
   case expr of
      (ELitTrue _) -> return True
      _ -> checkReturnStmt stmt

checkReturnStmt (For pos t ident collection stmt) = checkReturnStmt stmt
checkReturnStmt (SExp pos expr) = return False

-- checkReturnItem :: Item -> TypeCheckerState ()
-- checkReturnItem (NoInit pos ident) = return ()
-- checkReturnItem (Init pos ident expr) = return ()

-- checkReturnExpr :: Expr -> TypeCheckerState Type
-- checkReturnExpr (ECast pos ident expr) = return $ Primitive pos (Int pos)
-- checkReturnExpr (ECastPrim pos t expr) = return $ Primitive pos (Int pos)
-- checkReturnExpr (ENewObject pos ident) = return $ Primitive pos (Int pos)
-- checkReturnExpr (ENewArr pos t expr) = return $ Primitive pos (Int pos)
-- checkReturnExpr (ENull pos) = return $ Primitive pos (Int pos)
-- checkReturnExpr (EObject pos expr1 expr2) = return $ Primitive pos (Int pos)
-- checkReturnExpr (EArr pos ident expr) = return $ Primitive pos (Int pos)
-- checkReturnExpr (EVar pos ident) = return $ Primitive pos (Int pos)
-- checkReturnExpr (ELitInt pos val) = return $ Primitive pos (Int pos)
-- checkReturnExpr (ELitTrue pos) = return $ Primitive pos (Int pos)
-- checkReturnExpr (ELitFalse pos) = return$ Primitive pos (Int pos)
-- checkReturnExpr (EApp pos ident expr) = return $ Primitive pos (Int pos)
-- checkReturnExpr (EString pos s) = return $ Primitive pos (Int pos)
-- checkReturnExpr (Neg pos expr) = return $ Primitive pos (Int pos)
-- checkReturnExpr (Not pos expr) = return $ Primitive pos (Int pos)
-- checkReturnExpr (EMul pos expr1 op expr2) = return $ Primitive pos (Int pos)
-- checkReturnExpr (EAdd pos expr1 op expr2) = return $ Primitive pos (Int pos)
-- checkReturnExpr (ERel pos expr1 op expr2) = return $ Primitive pos (Int pos)
-- checkReturnExpr (EAnd pos expr1 expr2) = return $ Primitive pos (Int pos)
-- checkReturnExpr (EOr pos expr1 expr2) = return $ Primitive pos (Int pos)