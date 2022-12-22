module Typechecker.Data where

import           Control.Monad              (when)
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Control.Monad.Trans.Except (Except, ExceptT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.State  (StateT, get, gets, modify, put)
import           Data.Char                  (toLower)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import qualified Data.Set                   as S
import           Debug.Trace                (trace)
import           Syntax.AbsLattepp          (Arg, BNFC'Position, Block, Expr,
                                             HasPosition (hasPosition),
                                             Ident (Ident), PrimType' (Int),
                                             TopDef' (ClassDef), Type,
                                             Type' (Array, Fun, ObjectType, Primitive))
import           Utils                      (Pretty (pretty), Raw (raw),
                                             firstDuplicateIndex, getArgIdent,
                                             getArgType, rawInt, rawStr,
                                             rawVoid, throwException)

predefinedFunctions = [
  (Ident "printInt",    (rawVoid, [rawInt])),
  (Ident "printString", (rawVoid, [rawStr])),
  (Ident "error",       (rawVoid, [])),
  (Ident "readInt",     (rawInt, [])),
  (Ident "readString",  (rawStr, []))]

type TypeCheckerState = StateT TypeCheckerS (ExceptT String IO)

data TypeCheckerS = TypeCheckerS {
  typeEnv :: M.Map Ident Type,
  classEnv :: M.Map Ident ((Maybe Ident, BNFC'Position, [Ident]), ClassDefS),
  funEnv :: M.Map Ident (Type, [Type]),
  scope :: S.Set Ident,
  expectedReturnType :: Maybe Type,
  objectCheck :: Maybe Ident,
  enforceAttr :: Bool,
  self :: Maybe Ident
} deriving (Show)

data ClassDefS = ClassDefS {
  attrs   :: M.Map Ident Type,
  methods :: M.Map Ident (Type, [Type])
} deriving (Show)

initClassDefS :: Ident -> ClassDefS
initClassDefS ident = ClassDefS {
  attrs = M.fromList [(Ident "self", ObjectType Nothing ident)],
  methods = M.empty
}

initTypeCheckerS :: TypeCheckerS
initTypeCheckerS = TypeCheckerS {
  typeEnv = M.empty,
  classEnv = M.empty,
  funEnv = M.fromList predefinedFunctions,
  scope = S.empty,
  expectedReturnType = Nothing,
  objectCheck = Nothing,
  enforceAttr = False,
  self = Nothing
}

setType :: TypeCheckerS -> Ident -> Type -> TypeCheckerS
setType s ident t = TypeCheckerS {
  typeEnv = M.insert ident t $ typeEnv s,
  classEnv = classEnv s,
  funEnv = funEnv s,
  scope = S.insert ident (scope s),
  expectedReturnType = expectedReturnType s,
  objectCheck = Nothing,
  enforceAttr = enforceAttr s,
  self = self s
}

setTypes :: TypeCheckerS -> [Ident] -> [Type] -> TypeCheckerS
setTypes s _ []          = s
setTypes s [] _          = s
setTypes s (i:is) (t:ts) = setTypes (setType s i t) is ts

setExpectedReturnType :: TypeCheckerS -> Maybe Type -> TypeCheckerS
setExpectedReturnType s r = TypeCheckerS {
  typeEnv = typeEnv s,
  classEnv = classEnv s,
  funEnv = funEnv s,
  scope = scope s,
  expectedReturnType = r,
  objectCheck = Nothing,
  enforceAttr = enforceAttr s,
  self = self s
}

emptyScope :: TypeCheckerS -> TypeCheckerS
emptyScope s = TypeCheckerS {
  typeEnv = typeEnv s,
  classEnv = classEnv s,
  funEnv = funEnv s,
  scope = S.empty,
  expectedReturnType = expectedReturnType s,
  objectCheck = Nothing,
  enforceAttr = enforceAttr s,
  self = self s
}

setObjectCheck :: TypeCheckerS -> Maybe Type -> TypeCheckerS
setObjectCheck s (Just (ObjectType _ i)) = TypeCheckerS {
  typeEnv = typeEnv s,
  classEnv = classEnv s,
  funEnv = funEnv s,
  scope = scope s,
  expectedReturnType = expectedReturnType s,
  objectCheck = Just i,
  enforceAttr = enforceAttr s,
  self = self s
}
setObjectCheck s Nothing = TypeCheckerS {
  typeEnv = typeEnv s,
  classEnv = classEnv s,
  funEnv = funEnv s,
  scope = scope s,
  expectedReturnType = expectedReturnType s,
  objectCheck = Nothing,
  enforceAttr = enforceAttr s,
  self = self s
}
setObjectCheck _ _ = undefined

setEnforceAttr :: TypeCheckerS -> Bool -> TypeCheckerS
setEnforceAttr s b = TypeCheckerS {
  typeEnv = typeEnv s,
  classEnv = classEnv s,
  funEnv = funEnv s,
  scope = scope s,
  expectedReturnType = expectedReturnType s,
  objectCheck = objectCheck s,
  enforceAttr = b,
  self = self s
}

setSelf :: TypeCheckerS -> Maybe Ident -> TypeCheckerS
setSelf s c = TypeCheckerS {
  typeEnv = typeEnv s,
  classEnv = classEnv s,
  funEnv = funEnv s,
  scope = scope s,
  expectedReturnType = expectedReturnType s,
  objectCheck = objectCheck s,
  enforceAttr = enforceAttr s,
  self = c
}

addFun :: TypeCheckerS -> Type -> Ident -> [Type] -> TypeCheckerS
addFun s ret ident args = TypeCheckerS {
  typeEnv = typeEnv s,
  classEnv = classEnv s,
  funEnv = M.insert ident (ret, args) (funEnv s),
  scope = scope s,
  expectedReturnType = expectedReturnType s,
  objectCheck = objectCheck s,
  enforceAttr = enforceAttr s,
  self = self s
}

addChild :: TypeCheckerS -> Maybe Ident -> Ident -> TypeCheckerS
addChild s c child = case c of
  Nothing -> s
  Just class1 ->
    case M.lookup class1 (classEnv s) of
      Nothing -> s
      Just ((ext, pos, childs), def) -> TypeCheckerS {
        typeEnv = typeEnv s,
        classEnv = M.insert class1 ((ext, pos, child : childs), def) (classEnv s),
        funEnv = funEnv s,
        scope = scope s,
        expectedReturnType = expectedReturnType s,
        objectCheck = objectCheck s,
        enforceAttr = enforceAttr s,
        self = self s
      }

addClass :: TypeCheckerS -> Ident -> Maybe Ident -> BNFC'Position -> TypeCheckerS
addClass s c ext pos =
  let ((_, p, childs), def) = fromMaybe ((Nothing, pos, []), initClassDefS c) (M.lookup c (classEnv s)) in
  addChild (TypeCheckerS {
    typeEnv = typeEnv s,
    classEnv = M.insert c ((ext, p, childs), def) (classEnv s),
    funEnv = funEnv s,
    scope = scope s,
    expectedReturnType = expectedReturnType s,
    objectCheck = objectCheck s,
    enforceAttr = enforceAttr s,
    self = self s
  }) ext c

addAttrs :: Ident -> Type -> [Ident] -> TypeCheckerState ()
addAttrs _ _ [] = return ()
addAttrs class1 type1 (i:is) = do
  addAttr class1 type1 i
  addAttrs class1 type1 is

_addAttr :: TypeCheckerS -> Ident -> Type -> Ident -> TypeCheckerS
_addAttr s c type1 ident =
  let (ext, def) = fromMaybe undefined (M.lookup c (classEnv s)) in
  TypeCheckerS {
  typeEnv = typeEnv s,
  classEnv = M.insert c (ext, ClassDefS {
    attrs = M.insert ident type1 (attrs def),
    methods = methods def
  }) (classEnv s),
  funEnv = funEnv s,
  scope = scope s,
  expectedReturnType = expectedReturnType s,
  objectCheck = objectCheck s,
  enforceAttr = enforceAttr s,
  self = self s
}

dontAllowSelf :: BNFC'Position -> Ident -> TypeCheckerState ()
dontAllowSelf pos i =
  when (i == Ident "self") $
  throwException $ SelfKeywordException pos

addAttr :: Ident -> Type -> Ident -> TypeCheckerState ()
addAttr c type1 ident = do
  _ensureTypeExists type1
  dontAllowSelf (hasPosition type1) ident
  _dontAllowVoid type1
  st <- get
  case M.lookup c (classEnv st) of
    Nothing -> throwException $ UndefinedTypeException (hasPosition type1) (ObjectType Nothing c)
    Just (_, def) -> do
      case M.lookup ident (attrs def) of
        Nothing -> do
          case findAttr st c ident of
            Nothing -> case findMethod st c ident of
              Nothing -> put $ _addAttr st c type1 ident
              Just (ret, args) -> throwException $ CannotOverrideInheritedTypeException (hasPosition type1) ident type1 (Fun Nothing ret args)
            Just t ->
              when (raw t /= raw type1)
              (throwException $ CannotOverrideInheritedTypeException (hasPosition type1) ident type1 t)
        Just f -> throwException $ ClassFieldRedeclarationException (hasPosition type1) ident



_addMethod :: TypeCheckerS -> Ident -> Type -> Ident -> [Arg] -> TypeCheckerS
_addMethod s c ret ident args =
  let types = map getArgType args in
  let (ext, def) = fromMaybe undefined (M.lookup c (classEnv s)) in
  TypeCheckerS {
  typeEnv = typeEnv s,
  classEnv = M.insert c (ext, ClassDefS {
    attrs = attrs def,
    methods = M.insert ident (ret, types) (methods def)
  }) (classEnv s),
  funEnv = funEnv s,
  scope = scope s,
  expectedReturnType = expectedReturnType s,
  objectCheck = objectCheck s,
  enforceAttr = enforceAttr s,
  self = self s
}

--circular dep
_dontAllowVoid :: Type -> TypeCheckerState ()
_dontAllowVoid t = do
   when (raw t == rawVoid) $ throwException $ VoidNotAllowedException (hasPosition t)

_ensureUniqueIdents :: [Arg] -> TypeCheckerState ()
_ensureUniqueIdents args =
  case firstDuplicateIndex $ map getArgIdent args of
    Just index -> throwException $ ArgumentRedeclarationException (hasPosition $ args !! index) (getArgIdent $ args !! index)
    Nothing -> return ()

_ensureTypeExists :: Type -> TypeCheckerState ()
_ensureTypeExists type1 =
   case type1 of
     Primitive _ _ -> return ()
     ObjectType pos ident -> do
      st <- get
      case M.lookup ident (classEnv st) of
        Nothing -> throwException $ UndefinedTypeException pos type1
        Just _  -> return ()
     Array _ type2 -> _ensureTypeExists type2
     _ -> undefined

addMethod :: Ident -> Type -> Ident -> [Arg] -> TypeCheckerState ()
addMethod c ret ident args = do
  _ensureTypeExists ret
  _ensureUniqueIdents args
  st <- get
  case M.lookup c (classEnv st) of
    Nothing -> throwException $ UndefinedTypeException (hasPosition ret) (ObjectType Nothing c)
    Just (_, def) -> do
      case M.lookup ident (methods def) of
        Nothing -> do
          let types = map getArgType args
          case findMethod st c ident of
            Nothing -> case findAttr st c ident of
                Nothing -> do
                  mapM_ _dontAllowVoid types
                  put $ _addMethod st c ret ident args
                Just t -> throwException $ CannotOverrideInheritedTypeException (hasPosition ret) ident (Fun Nothing ret types) t
            Just (pRet, pTypes) -> do
              when (raw pRet /= raw ret || raw pTypes /= raw types) $
                throwException $ CannotOverrideInheritedTypeException (hasPosition ret) ident (Fun Nothing ret types) (Fun Nothing pRet pTypes)
        Just f -> throwException $ ClassFieldRedeclarationException (hasPosition ret) ident


findMethod :: TypeCheckerS -> Ident -> Ident -> Maybe (Type, [Type])
findMethod s c i =
  case M.lookup c (classEnv s) of
    Nothing -> Nothing
    Just (parent, def) -> case M.lookup i (methods def) of
      Nothing -> case parent of
        (Nothing, _, _)  -> Nothing
        (Just pid, _, _) -> findMethod s pid i
      Just t -> Just t

findAttr :: TypeCheckerS -> Ident -> Ident -> Maybe Type
findAttr s c i =
  case M.lookup c (classEnv s) of
    Nothing -> Nothing
    Just (parent, def) -> case M.lookup i (attrs def) of
      Nothing -> case parent of
        (Nothing, _, _)  -> Nothing
        (Just pid, _, _) -> findAttr s pid i
      Just t -> Just t

findFunInState :: TypeCheckerS -> Ident -> Maybe (Type, [Type])
findFunInState s ident = case M.lookup ident (funEnv s) of
  Nothing -> case self s of
    Nothing -> Nothing
    Just c  -> findMethod s c ident
  Just t -> Just t

findVarInState :: TypeCheckerS -> Ident -> Maybe Type
findVarInState s ident = case M.lookup ident (typeEnv s) of
  Nothing -> case self s of
    Nothing -> Nothing
    Just c  -> findAttr s c ident
  Just t -> Just t

localTypeEnv :: TypeCheckerS -> TypeCheckerState a -> TypeCheckerState a
localTypeEnv changedEnv action = do
  backup <- get
  put changedEnv
  result <- action
  put backup
  return result

-- --------------EXCEPTIONS---------------------------------------------------------------------------------
data TypeCheckerException  =  InvalidTypeExpectedException BNFC'Position Type Type
                            | InvalidExpectedArrayException BNFC'Position
                            | InvalidExpectedObjectException BNFC'Position
                            | InvalidTypeException BNFC'Position Type
                            | InvalidFunctionApplicationException BNFC'Position Ident Type
                            | InvalidMethodApplicationException BNFC'Position Ident Type
                            | InvalidNumberOfParametersException BNFC'Position
                            | InvalidReturnTypeException BNFC'Position Type Type
                            | InvalidCastException BNFC'Position Type Type
                            | NoReturnException BNFC'Position Ident
                            | UndefinedVariableException BNFC'Position Ident
                            | UndefinedObjectFieldException BNFC'Position Ident
                            | UndefinedFunctionException BNFC'Position Ident
                            | UndefinedTypeException BNFC'Position Type
                            | VariableRedeclarationException BNFC'Position Ident
                            | FunctionRedeclarationException BNFC'Position Ident
                            | ArgumentRedeclarationException BNFC'Position Ident
                            | ClassRedeclarationException BNFC'Position Ident
                            | ClassFieldRedeclarationException BNFC'Position Ident
                            | VoidNotAllowedException BNFC'Position
                            | CircularInheritanceException BNFC'Position Ident Ident
                            | CannotOverrideInheritedTypeException BNFC'Position Ident Type Type
                            | NoMainException
                            | ArgumentInMainException BNFC'Position
                            | MainReturnTypeException BNFC'Position Type
                            | ObjectFieldException BNFC'Position
                            | ObjectFieldGetException BNFC'Position
                            | SelfKeywordException BNFC'Position
                            | NoInitException BNFC'Position Type
                            | IllegalArrayField BNFC'Position
                            | WildCardException BNFC'Position



instance Show TypeCheckerException where

  show (InvalidTypeExpectedException position type1 type2) =
    "Invalid TYPE of " ++ pretty type1 ++ " at " ++ pretty position ++ "; EXPECTED " ++ pretty type2

  show (InvalidExpectedArrayException position) =
    "EXPECTED ARRAY TYPE at " ++ pretty position

  show (InvalidExpectedObjectException position) =
    "EXPECTED OBJECT TYPE at " ++ pretty position

  show (InvalidTypeException position type1) =
    "Invalid TYPE of " ++ pretty type1 ++ " at " ++ pretty position

  show (InvalidFunctionApplicationException position ident t) =
    "Invalid FUNCTION APPLICATION at " ++ pretty position ++ "; '" ++ pretty ident ++ "' is of type " ++ pretty t

  show (InvalidMethodApplicationException position ident t) =
    "Invalid METHOD APPLICATION at " ++ pretty position ++ "; '" ++ pretty ident ++ "' is of type " ++ pretty t

  show (InvalidNumberOfParametersException position) =
    "Invalid NUMBER OF PARAMETERS at " ++ pretty position

  show (InvalidReturnTypeException position type1 type2) =
    "Invalid RETURN TYPE of " ++ pretty type1 ++ " at " ++ pretty position ++ "; EXPECTED " ++ pretty type2

  show (NoReturnException position ident) =
    "NOT EVERY PATH RETURNS VALUE in " ++ pretty ident ++ " function definition; cause at " ++ pretty position

  show (InvalidCastException position type1 type2) =
    "invalid CAST FROM type " ++ pretty type1 ++ " TO type " ++ pretty type2 ++ " at " ++ pretty position

  show (UndefinedVariableException position ident) =
    "UNDEFINED VARIABLE '" ++ pretty ident ++ "' at " ++ pretty position

  show (UndefinedObjectFieldException position ident) =
    "UNDEFINED OBJECT FIELD '" ++ pretty ident ++ "' at " ++ pretty position

  show (UndefinedFunctionException position ident) =
    "UNDEFINED FUNCTION USAGE '" ++ pretty ident ++ "' at " ++ pretty position

  show (UndefinedTypeException position type1) =
    "UNDEFINED CLASS TYPE '" ++ pretty type1 ++ "' at " ++ pretty position

  show (VariableRedeclarationException position ident) =
    "REDECLARATION of variable '" ++ pretty ident ++ "' at " ++ pretty position

  show (FunctionRedeclarationException position ident) =
    "REDECLARATION of function '" ++ pretty ident ++ "' at " ++ pretty position

  show (ArgumentRedeclarationException position ident) =
    "REDECLARATION of argument '" ++ pretty ident ++ "' at " ++ pretty position

  show (ClassRedeclarationException position ident) =
    "REDECLARATION of class '" ++ pretty ident ++ "' at " ++ pretty position

  show (ClassFieldRedeclarationException position ident) =
    "REDECLARATION of class FIELD '" ++ pretty ident ++ "' at " ++ pretty position

  show (VoidNotAllowedException position) =
    "VOID type NOT ALLOWED outside function return type at " ++ pretty position

  show (CircularInheritanceException position ident1 ident2) =
    "CIRCULAR INHERITANCE of class " ++ pretty ident1 ++ " and " ++ pretty ident2 ++ " at " ++ pretty position

  show (CannotOverrideInheritedTypeException position ident type1 type2) =
    "INHERITED FIELD " ++ pretty ident ++ " has INVALID TYPE of " ++ pretty type1 ++ "; expected " ++ pretty type2

  show NoMainException =
    "NO MAIN FUNCTION FOUND"

  show (ArgumentInMainException position) =
    "MAIN FUNCTION CAN'T RECEIVE ARGUMENTS at " ++ pretty position

  show (MainReturnTypeException position type1) =
    "main function should RETURN INT but returns " ++ pretty type1 ++ " at " ++ pretty position

  show (ObjectFieldException position) =
    "invalid OBJECT FIELD USAGE at " ++ pretty position

  show (ObjectFieldGetException position) =
    "invalid OBJECT FIELD GET at " ++ pretty position ++ "; last expression should be object attribute"

  show (SelfKeywordException position) =
    "SELF can't be used as an object field at " ++ pretty position

  show (NoInitException position type1) =
    "type " ++ pretty type1 ++ " MUST BE INITIALIZED at " ++ pretty position

  show (IllegalArrayField position) =
    "array type CONTAINS ONLY length field at " ++ pretty position

  show (WildCardException position) =
    "Static Error: Unknown problem at " ++ pretty position
