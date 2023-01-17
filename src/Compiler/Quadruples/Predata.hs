{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Compiler.Quadruples.Predata where
import           Control.Monad.Except (ExceptT)
import           Control.Monad.State  (StateT, modify)
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe)
import           Syntax.AbsLattepp    (BNFC'Position, Ident (Ident), Type,
                                       Type')
import           Typechecker.Data     (TypeCheckerS)
import qualified Typechecker.Data     as Tc

-------------------------------------- PREPROCESS -------------------------------------------------

type PreprocessState = StateT PreprocessS (ExceptT String IO)
data PreprocessS = PreprocessS {
    prefuncs   :: PreFuncDef,
    preclasses :: M.Map String ClassDefPre
}

initPreprocessS :: PreprocessS
initPreprocessS = PreprocessS {
    prefuncs = M.empty,
    preclasses = M.empty
}

type PreFuncDef = M.Map Ident (Type, [Type])
data ClassDefPre = ClassDefPre {
  ident   :: String,
  attrs   :: M.Map String (Type, Int),
  methods :: M.Map String ((Type, [Type]), Int),
  super   :: Maybe String,
  size    :: Int
}

tcClassesToPre :: M.Map Ident ((Maybe Ident, BNFC'Position, [Ident]), Tc.ClassDefS) -> M.Map String ClassDefPre
tcClassesToPre cs =
    M.fromList (map (\(Ident k, ((s, _, _), def)) -> (k, _tcClassDefToPre def k s)) (M.toList cs))

_tcClassDefToPre :: Tc.ClassDefS -> String -> Maybe Ident -> ClassDefPre
_tcClassDefToPre c i msuper =
    ClassDefPre {
        ident = i,
        attrs   = M.fromList $ map (\(Ident k, v) -> (k, (v, 0))) (M.toList (Tc.attrs c)),
        methods = M.fromList $ map (\(Ident k, v) -> (k, (v, 0))) (M.toList (Tc.methods c)),
        super   = case msuper of
                    Nothing        -> Nothing
                    Just (Ident i) -> Just i,
        size    = 0
    }

gatherFromTc :: TypeCheckerS -> PreprocessState ()
gatherFromTc tcs = modify (const
  PreprocessS
    {prefuncs = Tc.funEnv tcs,
     preclasses = tcClassesToPre (Tc.classEnv tcs)})

instance Show ClassDefPre where
    show c = show $ ident c ++ "\n" ++ show (attrs c) ++ "\n" ++ show (methods c) ++ "\n" ++ show (super c) ++ "\n" ++ show (size c) ++ "\n---------------------------------"
