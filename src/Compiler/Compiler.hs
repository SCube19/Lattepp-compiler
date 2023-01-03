module Compiler.Compiler where
import           Control.Monad.Trans.Except     (ExceptT)
import           Syntax.AbsLattepp              (Program)
import           Compiler.Quadruples.Quadruples (quadruplize)
import           Compiler.Quadruples.Data       (QProgram, initQuadruplesS)
import           Compiler.Data
import Control.Monad.Trans.State (evalStateT)

compile :: Program -> ExceptT String IO String
compile p =  do
    quads <- evalStateT (quadruplize p) initQuadruplesS
    evalStateT (compileQuads quads) initCompilerS

compileQuads :: QProgram -> CompilerState String
compileQuads p = return ""

