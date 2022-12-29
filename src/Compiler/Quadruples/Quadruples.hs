module Compiler.Quadruples.Quadruples where

import Compiler.Quadruples.Data
import Syntax.AbsLattepp (Program)


quadruplize :: Program -> QuadruplesState QProgram
quadruplize p = return $ ""