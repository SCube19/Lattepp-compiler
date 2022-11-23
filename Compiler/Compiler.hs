module Compiler.Compiler where
import Syntax.AbsLattepp (Program)
import Control.Monad.Trans.Except (ExceptT)


compile :: Program -> ExceptT String IO String
compile p = return "movl %rsp, %rbp"