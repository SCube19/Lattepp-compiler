module Typechecker.Utils where
import Syntax.AbsLattepp (BNFC'Position, Type, Expr, Arg, Ident, HasPosition (hasPosition), Type' (Primitive))
import Typechecker.Data (TypeCheckerState, TypeCheckerS, TypeCheckerException (..), setTypes, emptyScope, setExpectedReturnType)
import Utils (Raw(raw), throwException, firstDuplicateIndex, getArgIdent, rawVoid, rawFun, getArgType)
import Typechecker.TypeChecker (typeCheckExpr)
import Control.Monad (zipWithM_, when)

-- --------------------ENSURE------------------------------------------------------
ensureTypeMatch :: BNFC'Position -> Type -> Type -> TypeCheckerState ()
ensureTypeMatch pos type1 type2 =
  if raw type1 == raw type2
    then return ()
    else throwException $ InvalidTypeExpectedException pos type2 type1

ensureType :: Type -> Expr -> TypeCheckerState ()
ensureType t expr = do
  eType <- typeCheckExpr expr
  ensureTypeMatch (hasPosition eType) t eType

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
  when (raw t == Primitive Nothing rawVoid) $ throwException $ VoidNotAllowedException (hasPosition t)

-- dontAllowFun :: Type -> Ident -> TypeCheckerState ()
-- dontAllowFun t ident =
--   when (raw t == rawFun) $ throwException $ FunctionNotDefinedException (hasPosition t) ident
------------------FUNCTION STATE-----------------------------
functionState :: TypeCheckerS -> [Arg] -> Type -> TypeCheckerS
functionState s args rType = setTypes (emptyScope $ setExpectedReturnType s $ Just rType)
                              (map getArgIdent args) (map getArgType args)