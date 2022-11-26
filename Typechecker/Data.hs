module Typechecker.Data where

import Syntax.AbsLattepp ( BNFC'Position, Ident, Arg, Type, Block, Expr )
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Trans.Except (ExceptT, Except)
import Control.Monad.Trans.State (StateT, get, modify, put, gets)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Utils (Pretty(pretty))

type TypeCheckerState = StateT TypeCheckerS (ExceptT String IO)

data TypeCheckerS = TypeCheckerS {
  typeEnv :: M.Map Ident (Type, Bool),
  scope :: S.Set Ident,
  expectedReturnType :: Maybe Type,
  insideLoop :: Bool
} deriving (Show)

initTypeCheckerS :: TypeCheckerS
initTypeCheckerS = TypeCheckerS {
  typeEnv = M.empty,
  scope = S.empty,
  expectedReturnType = Nothing,
  insideLoop = False
}

getType :: TypeCheckerS -> Ident -> Maybe (Type, Bool)
getType s ident = M.lookup ident (typeEnv s)

setTypes :: TypeCheckerS -> [Ident] -> [(Type, Bool)] -> TypeCheckerS
setTypes s _ [] = s
setTypes s [] _ = s
setTypes s (i:is) (t:ts) = setTypes (setType s i t) is ts

setType :: TypeCheckerS -> Ident -> (Type, Bool) -> TypeCheckerS
setType s ident t = TypeCheckerS {
      typeEnv = M.insert ident t (typeEnv s),
      scope = S.insert ident (scope s),
      expectedReturnType = expectedReturnType s,
      insideLoop = insideLoop s
    }

setExpectedReturnType :: TypeCheckerS -> Maybe Type -> TypeCheckerS
setExpectedReturnType s r = TypeCheckerS {
  typeEnv = typeEnv s,
  scope = scope s,
  expectedReturnType = r,
  insideLoop = insideLoop s
}

setInsideLoop :: TypeCheckerS -> Bool -> TypeCheckerS
setInsideLoop s b = TypeCheckerS {
  typeEnv = typeEnv s,
  scope = scope s,
  expectedReturnType = expectedReturnType s,
  insideLoop = b
}

emptyScope :: TypeCheckerS -> TypeCheckerS
emptyScope s = TypeCheckerS {
  typeEnv = typeEnv s,
  scope = S.empty,
  expectedReturnType = expectedReturnType s,
  insideLoop = insideLoop s
} 

localTypeEnv :: TypeCheckerS -> TypeCheckerState () -> TypeCheckerState ()
localTypeEnv changedEnv action = do
  backup <- get 
  put changedEnv
  action
  put backup

-- --------------EXCEPTIONS---------------------------------------------------------------------------------
data TypeCheckerException  =  InvalidTypeExpectedException BNFC'Position Type Type
                            | InvalidTypeException BNFC'Position Type
                            | InvalidFunctionApplicationException BNFC'Position Ident Type
                            | InvalidMethodApplicationException BNFC'Position Ident Type
                            | InvalidNumberOfParametersException BNFC'Position
                            | InvalidReturnTypeException BNFC'Position Type Type
                            | NoReturnException BNFC'Position Ident
                            | InvalidCastException BNFC'Position Type
                            | UndefinedVariableException BNFC'Position Ident
                            | UndefinedObjectFieldException BNFC'Position Ident
                            | UndefinedFunctionException BNFC'Position Ident
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
                            | WildCardException BNFC'Position



instance Show TypeCheckerException where

  show (InvalidTypeExpectedException position type1 type2) =
    "Invalid TYPE of " ++ pretty type1 ++ " at " ++ pretty position ++ "; EXPECTED " ++ pretty type2

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
 
  show (InvalidCastException position t) = 
    "invalid CAST to type " ++ pretty t ++ " at " ++ pretty position
  
  show (UndefinedVariableException position ident) =
    "UNDEFINED VARIABLE '" ++ pretty ident ++ "' at " ++ pretty position

  show (UndefinedObjectFieldException position ident) =
    "UNDEFINED OBJECT FIELD '" ++ pretty ident ++ "' at " ++ pretty position
  
  show (UndefinedFunctionException position ident) =
    "UNDEFINED FUNCTION USAGE '" ++ pretty ident ++ "' at " ++ pretty position

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

  show (NoMainException) = 
    "NO MAIN FUNCTION FOUND"

  show (ArgumentInMainException position) =
    "MAIN FUNCTION CAN'T RECEIVE ARGUMENTS at " ++ pretty position 

  show (MainReturnTypeException position type1) =
    "main function should RETURN INT but returns " ++ pretty type1 ++ " at " ++ pretty position

  show (ObjectFieldException position) =
    "invalid OBJECT FIELD USAGE at " ++ pretty position 

  show (ObjectFieldGetException position) =
    "invalid OBJECT FIELD GET at " ++ pretty position ++ "; last expression should be object attribute"

  show (WildCardException position) =
    "Static Error: Unknown problem at " ++ pretty position
