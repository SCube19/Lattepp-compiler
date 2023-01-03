module Compiler.Quadruples.Quadruples where

import Compiler.Quadruples.Data
import Syntax.AbsLattepp (Program)
import Control.Monad.Trans.Except (ExceptT)


quadruplize :: Program -> QuadruplesState QProgram
quadruplize p = return $ qprogram initQuadruplesS