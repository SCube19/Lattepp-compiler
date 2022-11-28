module Typechecker.Data where

import Syntax.AbsLattepp ( BNFC'Position, Ident (Ident), Arg, Type, Block, Expr, Type' (Array, Primitive, ObjectType), PrimType' (Int) )
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Trans.Except (ExceptT, Except)
import Control.Monad.Trans.State (StateT, get, modify, put, gets)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Utils (Pretty(pretty), rawVoid, rawInt, rawStr, throwException)

predefinedFunctions = [
  (Ident "printInt",    (rawVoid, [rawInt])),
  (Ident "printString", (rawVoid, [rawStr])),
  (Ident "error",       (rawVoid, [])),
  (Ident "readInt",     (rawInt, [])),
  (Ident "readString",  (rawStr, []))]

testClass :: [(Ident, (Maybe Ident, ClassDefS))]
testClass = [
  (Ident "Obj", (Just (Ident "Parent"), ClassDefS{
    attrs = M.fromList [(Ident "testObj", rawInt)],
    methods = M.empty
  })),
  (Ident "Parent", (Just (Ident "GParent"), ClassDefS{
    attrs = M.fromList [(Ident "testParent", rawStr)],
    methods = M.empty
  })),
  (Ident "GParent", (Nothing, ClassDefS{
    attrs = M.fromList [(Ident "testGParent", ObjectType Nothing (Ident "Obj"))],
    methods = M.empty
  }))]

testVar = [
  (Ident "testArr", Array (Just(1, 2)) (Primitive Nothing (Int Nothing))),
  (Ident "testInt", Primitive Nothing (Int Nothing)),
  (Ident "obj", ObjectType Nothing (Ident "Obj"))]

type TypeCheckerState = StateT TypeCheckerS (ExceptT String IO)

data TypeCheckerS = TypeCheckerS {
  typeEnv :: M.Map Ident Type,
  classEnv :: M.Map Ident (Maybe Ident, ClassDefS), -- add setter/adder
  funEnv :: M.Map Ident (Type, [Type]), -- add setter/adder
  scope :: S.Set Ident,
  expectedReturnType :: Maybe Type,
  objectCheck :: Maybe Ident
} deriving (Show)

data ClassDefS = ClassDefS {
  attrs :: M.Map Ident Type,
  methods :: M.Map Ident (Type, [Type])
} deriving (Show)

initTypeCheckerS :: TypeCheckerS
initTypeCheckerS = TypeCheckerS {
  typeEnv = M.fromList testVar,
  classEnv = M.fromList testClass,
  funEnv = M.fromList predefinedFunctions,
  scope = S.empty,
  expectedReturnType = Nothing,
  objectCheck = Nothing
}

setType :: TypeCheckerS -> Ident -> Type -> TypeCheckerS
setType s ident t = TypeCheckerS {
  typeEnv = M.insert ident t $ typeEnv s,
  classEnv = classEnv s,
  funEnv = funEnv s,
  scope = S.insert ident (scope s),
  expectedReturnType = expectedReturnType s,
  objectCheck = Nothing
}

setTypes :: TypeCheckerS -> [Ident] -> [Type] -> TypeCheckerS
setTypes s _ [] = s
setTypes s [] _ = s
setTypes s (i:is) (t:ts) = setTypes (setType s i t) is ts

setExpectedReturnType :: TypeCheckerS -> Maybe Type -> TypeCheckerS
setExpectedReturnType s r = TypeCheckerS {
  typeEnv = typeEnv s,
  classEnv = classEnv s,
  funEnv = funEnv s,
  scope = scope s,
  expectedReturnType = r,
  objectCheck = Nothing
}

emptyScope :: TypeCheckerS -> TypeCheckerS
emptyScope s = TypeCheckerS {
  typeEnv = typeEnv s,
  classEnv = classEnv s,
  funEnv = funEnv s,
  scope = S.empty,
  expectedReturnType = expectedReturnType s,
  objectCheck = Nothing
}

setObjectCheck :: TypeCheckerS -> Maybe Type -> TypeCheckerS
setObjectCheck s (Just (ObjectType _ i)) = TypeCheckerS {
  typeEnv = typeEnv s,
  classEnv = classEnv s,
  funEnv = funEnv s,
  scope = scope s,
  expectedReturnType = expectedReturnType s,
  objectCheck = Just i
}
setObjectCheck s Nothing = TypeCheckerS {
  typeEnv = typeEnv s,
  classEnv = classEnv s,
  funEnv = funEnv s,
  scope = scope s,
  expectedReturnType = expectedReturnType s,
  objectCheck = Nothing
}  
setObjectCheck _ _ = undefined

findMethod :: TypeCheckerS -> Ident -> Ident -> Maybe (Type, [Type])
findMethod s t i = Nothing

findAttr :: TypeCheckerS -> Ident -> Ident -> Maybe Type
findAttr s c i = 
  case M.lookup c (classEnv s) of
    Nothing -> Nothing
    Just (parent, def) -> case M.lookup i (attrs def) of
      Nothing -> case parent of
        Nothing -> Nothing
        Just pid -> findAttr s pid i
      Just t -> Just t  

localTypeEnv :: TypeCheckerS -> TypeCheckerState () -> TypeCheckerState ()
localTypeEnv changedEnv action = do
  backup <- get
  put changedEnv
  action
  put backup

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
                            | CircularInheritanceException BNFC'Position Type Type
                            | CannotOverrideInheritedTypeException BNFC'Position Ident Type Type
                            | NoMainException
                            | ArgumentInMainException BNFC'Position
                            | MainReturnTypeException BNFC'Position Type
                            | ObjectFieldException BNFC'Position
                            | ObjectFieldGetException BNFC'Position
                            | SelfKeywordException BNFC'Position
                            | NoInitException BNFC'Position Type
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

  show (CircularInheritanceException position type1 type2) =
    "CIRCULAR INHERITANCE of class " ++ pretty type1 ++ " and " ++ pretty type2 ++ " at " ++ pretty position

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

  show (WildCardException position) =
    "Static Error: Unknown problem at " ++ pretty position
