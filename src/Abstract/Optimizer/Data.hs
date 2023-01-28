module Abstract.Optimizer.Data where
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.State  (StateT, get, put)
import qualified Data.Map                   as M
import           Syntax.AbsLattepp

type OptimizerState = StateT OptimizerS (ExceptT String IO)

newtype OptimizerS = OptimizerS {
  --todo remove consts use for better value propagation
  consts :: M.Map Ident (PrimType, Value)
}

data Value =  IntV Integer
            | BoolV Bool
            | StrV String

instance Show Value where
  show (IntV val)  = show val
  show (BoolV val) = show val
  show (StrV val)  = show val

castBool :: Value -> Bool
castBool (IntV v)  = v > 0
castBool (BoolV v) = v
castBool _         = False

castInteger :: Value -> Integer
castInteger (IntV v)  = v
castInteger (BoolV v) = if v then 1 else 0
castInteger _         = 0

castString :: Value -> String
castString (StrV v) = v
castString _        = ""


