module Compiler.Quadruples.Data where
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Except (ExceptT)
import qualified Data.Map                   as M

type QuadruplesState = StateT QuadrupleS (ExceptT String IO)
data QuadrupleS = QuadrupleS {
    qprogram :: QProgram
}

newtype Register = Register Integer

instance Show Register where
    show (Register r) = "r" ++ show r

data QProgram = QProgram {
    classes :: [QClass],
    funcs :: [Qfun],
    strings :: M.Map QLabel String
}

data Quadruple =
    Add Register Register Register |
    



