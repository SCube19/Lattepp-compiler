module Quadruples.Optimizer.Gcse.Data where
import           Control.Monad.Except (ExceptT)
import           Control.Monad.State  (StateT)

type GcseState = StateT GcseS (ExceptT String IO)
data GcseS = GcseS {
}

initGcseS :: GcseS
initGcseS = GcseS {

}
