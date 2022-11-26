module Typechecker.Data where

import Syntax.AbsLattepp ( BNFC'Position, Ident, Arg, Type, Block )
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
                            | InvalidNumberOfParametersException BNFC'Position
                            | InvalidReturnException BNFC'Position
                            | UndefinedException BNFC'Position Ident
                            | VoidNotAllowedException BNFC'Position
                            | RedeclarationException BNFC'Position Ident
                            | FunctionNotDefinedException BNFC'Position Ident
                            | WildCardException BNFC'Position



instance Show TypeCheckerException where

  show (InvalidTypeExpectedException position type1 type2) =
    "Static Error: Invalid TYPE of " ++ pretty type1 ++ " at " ++ pretty position ++ "; EXPECTED " ++ pretty type2

  show (InvalidTypeException position type1) =
    "Static Error: Invalid TYPE of " ++ pretty type1 ++ " at " ++ pretty position

  show (InvalidFunctionApplicationException position ident t) =
    "Static Error: Invalid FUNCTION APPLICATION at " ++ pretty position ++ "; '" ++ pretty ident ++ "' is of type " ++ pretty t

  show (InvalidNumberOfParametersException position) =
    "Static Error: Invalid NUMBER OF PARAMETERS at " ++ pretty position

  show (InvalidReturnException position) =
    "Static Error: RETURN statement OUTSIDE of a function definition block at " ++ pretty position

  show (UndefinedException position ident) =
    "Static Error: UNDEFINED IDENTIFIER '" ++ pretty ident ++ "' at " ++ pretty position

  show (VoidNotAllowedException position) =
    "Static Error: VOID type NOT ALLOWED outside function return type at " ++ pretty position

  show (RedeclarationException position ident) =
    "Static Error: REDECLARATION of variable '" ++ pretty ident ++ "' at " ++ pretty position

  show (FunctionNotDefinedException position ident) =
    "Static Error: FUNCTION '" ++ pretty ident ++ "' MUST BE INITIALIZED at " ++ pretty position
    
  show (WildCardException position) =
    "Static Error: Unknown problem at " ++ pretty position