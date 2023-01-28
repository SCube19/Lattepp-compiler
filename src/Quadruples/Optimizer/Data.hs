module Quadruples.Optimizer.Data where
import           Control.Monad.Except (ExceptT)
import           Control.Monad.State  (StateT, gets, modify)
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe)
import           Quadruples.Data      (Quadruple, Register)


type OptimizerState = StateT OptimizerS (ExceptT String IO)
data OptimizerS = OptimizerS {
    quadAcc :: [Quadruple],
    copy    :: M.Map Register Register,
    fusage  :: M.Map Register Int,
    lusage  :: M.Map Register Int
}

initOptimizerS :: OptimizerS
initOptimizerS = OptimizerS {
    quadAcc = [],
    copy = M.empty,
    fusage = M.empty,
    lusage = M.empty
}

resetAcc :: OptimizerState ()
resetAcc = modify (\s -> OptimizerS {
    quadAcc = [],
    copy = copy s,
    fusage = fusage s,
    lusage = lusage s
})

addAcc :: Quadruple -> OptimizerState ()
addAcc q = modify (\s -> OptimizerS {
    quadAcc = q : quadAcc s,
    copy = copy s,
    fusage = fusage s,
    lusage = lusage s
})

makeCopy :: Register -> Register -> OptimizerState ()
makeCopy r1 r2 = modify (\s -> OptimizerS {
    quadAcc = quadAcc s,
    copy = M.insert r1 r2 (copy s),
    fusage = fusage s,
    lusage = lusage s
})

insertFirstUsage :: Register -> Int -> OptimizerState ()
insertFirstUsage r i = modify (\s -> OptimizerS {
        quadAcc = quadAcc s,
        copy = copy s,
        fusage = M.insertWith min r i (fusage s),
        lusage = lusage s
    })

insertLastUsage :: Register -> Int -> OptimizerState ()
insertLastUsage r i = modify (\s -> OptimizerS {
        quadAcc = quadAcc s,
        copy = copy s,
        fusage = fusage s,
        lusage = M.insertWith max r i (lusage s)
    })

mapCopy :: Register -> M.Map Register Register -> Register
mapCopy r copies = fromMaybe r (M.lookup r copies)
