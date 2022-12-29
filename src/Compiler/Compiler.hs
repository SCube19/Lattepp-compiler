module Compiler.Compiler where
import           Control.Monad.Trans.Except     (ExceptT)
import           Syntax.AbsLattepp              (Program)
import           Compiler.Quadruples.Quadruples (quadruplize)

compile :: Program -> ExceptT String IO String
compile p = return $ quadruplize p
