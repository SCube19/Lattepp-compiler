module Typechecker.TypeCheckerData where

import Syntax.AbsLattepp ( BNFC'Position, Ident, Arg, Type, Block )
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Trans.Except (ExceptT, Except)
import Control.Monad.Trans.State (StateT, get, modify, put, gets)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)

-- type TypeCheckerState = StateT TypeCheckerS (ExceptT String IO)

-- data TypeCheckerS = TypeCheckerS {
--   typeEnv :: M.Map Ident (Type, Bool),
--   scope :: S.Set Ident,
--   expectedReturnType :: Maybe Type,
--   insideLoop :: Bool
-- } deriving (Show)

-- initTypeCheckerS :: TypeCheckerS
-- initTypeCheckerS = TypeCheckerS {
--   typeEnv = M.empty,
--   scope = S.empty,
--   expectedReturnType = Nothing,
--   insideLoop = False
-- }

-- getType :: TypeCheckerS -> Ident -> Maybe (Type, Bool)
-- getType s ident = M.lookup ident (typeEnv s)

-- setTypes :: TypeCheckerS -> [Ident] -> [(Type, Bool)] -> TypeCheckerS
-- setTypes s _ [] = s
-- setTypes s [] _ = s
-- setTypes s (i:is) (t:ts) = setTypes (setType s i t) is ts

-- setType :: TypeCheckerS -> Ident -> (Type, Bool) -> TypeCheckerS
-- setType s ident t = TypeCheckerS {
--       typeEnv = M.insert ident t (typeEnv s),
--       scope = S.insert ident (scope s),
--       expectedReturnType = expectedReturnType s,
--       insideLoop = insideLoop s
--     }

-- setExpectedReturnType :: TypeCheckerS -> Maybe Type -> TypeCheckerS
-- setExpectedReturnType s r = TypeCheckerS {
--   typeEnv = typeEnv s,
--   scope = scope s,
--   expectedReturnType = r,
--   insideLoop = insideLoop s
-- }

-- setInsideLoop :: TypeCheckerS -> Bool -> TypeCheckerS
-- setInsideLoop s b = TypeCheckerS {
--   typeEnv = typeEnv s,
--   scope = scope s,
--   expectedReturnType = expectedReturnType s,
--   insideLoop = b
-- }

-- emptyScope :: TypeCheckerS -> TypeCheckerS
-- emptyScope s = TypeCheckerS {
--   typeEnv = typeEnv s,
--   scope = S.empty,
--   expectedReturnType = expectedReturnType s,
--   insideLoop = insideLoop s
-- } 

-- localTypeEnv :: TypeCheckerS -> TypeCheckerState () -> TypeCheckerState ()
-- localTypeEnv changedEnv action = do
--   backup <- get 
--   put changedEnv
--   action
--   put backup

-- --------------EXCEPTIONS---------------------------------------------------------------------------------
-- data TypeCheckerException  =  InvalidTypeExpectedException BNFC'Position Type Type
--                             | InvalidTypeException BNFC'Position Type
--                             | InvalidFunctionApplicationException BNFC'Position Ident Type
--                             | InvalidNumberOfParametersException BNFC'Position
--                             | InvalidReturnException BNFC'Position
--                             | InvalidExitException BNFC'Position
--                             | InvalidSkipException BNFC'Position
--                             | UndefinedException BNFC'Position Ident
--                             | VoidNotAllowedException BNFC'Position
--                             | RedeclarationException BNFC'Position Ident
--                             | ConstReferenceException BNFC'Position
--                             | ReferenceNotVariableException BNFC'Position
--                             | ImmutableNotInitializedException BNFC'Position Ident
--                             | ImmutableCannotBeChangedException BNFC'Position Ident
--                             | FunctionNotDefinedException BNFC'Position Ident
--                             | WildCardException BNFC'Position



-- instance Show TypeCheckerException where

--   show (InvalidTypeExpectedException position type1 type2) =
--     "Static Error: Invalid TYPE of " ++ prettyType type1 ++ " at " ++ prettyPosition position ++ "; EXPECTED " ++ prettyType type2

--   show (InvalidTypeException position type1) =
--     "Static Error: Invalid TYPE of " ++ prettyType type1 ++ " at " ++ prettyPosition position

--   show (InvalidFunctionApplicationException position ident t) =
--     "Static Error: Invalid FUNCTION APPLICATION at " ++ prettyPosition position ++ "; '" ++ prettyIdent ident ++ "' is of type " ++ prettyType t

--   show (InvalidNumberOfParametersException position) =
--     "Static Error: Invalid NUMBER OF PARAMETERS at " ++ prettyPosition position

--   show (InvalidReturnException position) =
--     "Static Error: RETURN statement OUTSIDE of a function definition block at " ++ prettyPosition position

--   show (InvalidExitException position) =
--     "Static Error: EXIT statement OUTSIDE of a loop block at " ++ prettyPosition position

--   show (InvalidSkipException position) =
--     "Static Error: SKIP statement OUTSIDE of a loop block at " ++ prettyPosition position

--   show (UndefinedException position ident) =
--     "Static Error: UNDEFINED IDENTIFIER '" ++ prettyIdent ident ++ "' at " ++ prettyPosition position

--   show (VoidNotAllowedException position) =
--     "Static Error: VOID type NOT ALLOWED outside function return type at " ++ prettyPosition position

--   show (RedeclarationException position ident) =
--     "Static Error: REDECLARATION of variable '" ++ prettyIdent ident ++ "' at " ++ prettyPosition position

--   show (ConstReferenceException position) =
--     "Static Error: cannot pass IMMUTABLE variable as REFERENCE parameter at " ++ prettyPosition position

--   show (ReferenceNotVariableException position) =
--     "Static Error: REFERENCE parameter MUST BE VARIABLE at " ++ prettyPosition position

--   show (ImmutableNotInitializedException position ident) =
--     "Static Error: IMMUTABLE value '" ++ prettyIdent ident ++ "' MUST BE INITIALIZED at " ++ prettyPosition position

--   show (ImmutableCannotBeChangedException position ident) =
--     "Static Error: IMMUTABLE value '" ++ prettyIdent ident ++ "' CANNOT BE ASSIGNED new value at " ++ prettyPosition position

--   show (FunctionNotDefinedException position ident) =
--     "Static Error: FUNCTION '" ++ prettyIdent ident ++ "' MUST BE INITIALIZED at " ++ prettyPosition position
    
--   show (WildCardException position) =
--     "Static Error: Unknown problem at " ++ prettyPosition position