module Compiler.Data where
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Except (ExceptT)

type CompilerState = StateT CompilerS (ExceptT String IO)

newtype CompilerS = CompilerS {
    tmp :: String
}

initCompilerS :: CompilerS
initCompilerS = CompilerS {
    tmp = ""
}