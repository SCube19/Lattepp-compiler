module Compiler.Quadruples.Predata where
import           Control.Monad.Except (ExceptT)
import           Control.Monad.State  (StateT)
import qualified Data.Map             as M
import           Syntax.AbsLattepp    (Ident, Type, Type')

-------------------------------------- PREPROCESS -------------------------------------------------

type PreprocessState = StateT PreprocessS (ExceptT String IO)
data PreprocessS = PreprocessS {
    prefuncs   :: M.Map String FuncDefPre,
    preclasses :: M.Map String ClassDefPre
}

initPreprocessS :: PreprocessS
initPreprocessS = PreprocessS {
    prefuncs = M.empty,
    preclasses = M.empty
}

data FuncDefPre = FuncDefPre {
    args :: [Type],
    ret  :: Type
}

data ClassDefPre = ClassDefPre {
  ident   :: String,
  attrs   :: M.Map String Type,
  methods :: M.Map String FuncDefPre,
  super   :: Maybe ClassDefPre,
  size    :: Int
}

