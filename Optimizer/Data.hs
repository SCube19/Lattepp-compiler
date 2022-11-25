module Optimizer.Data where
import qualified Data.Map as M
import Control.Monad.Trans.State (StateT, get, put)
import Control.Monad.Trans.Except (ExceptT)
import Syntax.AbsLattepp

type OptimizerState = StateT OptimizerS (ExceptT String IO)

newtype OptimizerS = OptimizerS {
  consts :: M.Map Ident (PrimType, Value)
} 

data Value =  IntV Integer
            | BoolV Bool
            | StrV String

instance Show Value where
  show (IntV val) = show val
  show (BoolV val) = show val
  show (StrV val) = show val

castBool :: Value -> Bool
castBool (IntV v) = v > 0
castBool (BoolV v) = v
castBool _ = False

castInteger :: Value -> Integer
castInteger (IntV v) = v
castInteger (BoolV v) = if v then 1 else 0
castInteger _ = 0

castString :: Value -> String
castString (StrV v) = v
castString _ = ""

initOptimizerS :: OptimizerS
initOptimizerS = OptimizerS {
    consts = M.empty
}

putConst :: OptimizerS -> Ident -> (PrimType, Value) -> OptimizerS
putConst s ident x@(t, val) = OptimizerS {
    consts = M.insert ident x (consts s)
}

localOptimizerEnv :: OptimizerS -> OptimizerState a -> OptimizerState a
localOptimizerEnv changedEnv action = do
  backup <- get 
  put changedEnv
  result <- action
  put backup
  return result