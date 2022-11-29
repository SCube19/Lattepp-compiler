module Compiler.Compiler where
import           Control.Monad.Trans.Except (ExceptT)
import           Syntax.AbsLattepp          (Program)


compile :: Program -> ExceptT String IO String
compile p = return "movl %rsp, %rbp"
