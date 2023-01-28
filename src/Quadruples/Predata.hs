{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Quadruples.Predata where
import           Abstract.Typechecker.Data (TypeCheckerS)
import qualified Abstract.Typechecker.Data as Tc
import qualified Control.Applicative
import           Control.Monad.Except      (ExceptT)
import           Control.Monad.State       (StateT, modify)
import           Data.Function             (on)
import           Data.List                 (sortBy)
import qualified Data.Map                  as M
import           Data.Maybe                (fromMaybe)
import qualified Data.Set                  as S
import           Syntax.AbsLattepp         (BNFC'Position, Ident (Ident), Type,
                                            Type')
import           Utils                     (Pretty (pretty))

-------------------------------------- PREPROCESS -------------------------------------------------

type PreprocessState = StateT PreprocessS (ExceptT String IO)
newtype PreprocessS = PreprocessS {
    preclasses :: M.Map String (Int, ClassDefPre)
}

initPreprocessS :: PreprocessS
initPreprocessS = PreprocessS {
    preclasses = M.empty
}

data ClassDefPre = ClassDefPre {
  ident   :: String,
  attrs   :: M.Map String (Type, Int),
  methods :: M.Map String ((Type, [Type]), Int),
  super   :: Maybe ClassDefPre,
  size    :: Int
}

hasMethod :: Ident -> Bool
hasMethod i = True

tcClassesToPre :: [(Ident, ((Maybe Ident, BNFC'Position, Int, [Ident]), Tc.ClassDefS))] -> M.Map String (Int, ClassDefPre) -> M.Map String (Int, ClassDefPre)
tcClassesToPre [] preCs = preCs
tcClassesToPre ((Ident k, ((s, _, ord, _), def)):cs) preCs =
    tcClassesToPre cs (M.insert k (ord, _tcClassDefToPre def k s preCs) preCs)

_tcClassDefToPre :: Tc.ClassDefS -> String -> Maybe Ident -> M.Map String (Int, ClassDefPre) -> ClassDefPre
_tcClassDefToPre c i msuper cs =
    let superClass = case msuper of
          Nothing         -> Nothing
          Just (Ident id) -> M.lookup id cs in
    let superClassSize = maybe 0 size (case superClass of
                                            Nothing     -> Nothing
                                            Just (_, x) -> Just x) in
    let (ats, finalOffset) = addOffsetsToAttrs (M.toList (Tc.attrs c)) superClassSize in
    ClassDefPre {
        ident = i,
        attrs   = M.fromList ats,
        methods = M.fromList $ addOffsetsToMethods (M.toList (Tc.methods c)) S.empty 0,
        super   = case superClass of
                        Nothing     -> Nothing
                        Just (_, x) -> Just x,
        size    = finalOffset
    }

addOffsetsToAttrs :: [(Ident, Type)] -> Int -> ([(String, (Type, Int))], Int)
addOffsetsToAttrs [] offset                        = ([], offset)
addOffsetsToAttrs ((Ident name, t):as) offset =
    if name == "self" then
        addOffsetsToAttrs as offset
    else
        let (ats, newOffset) = addOffsetsToAttrs as (offset + 4) in
        ((name, (t, offset)):ats, newOffset)

addOffsetsToMethods :: [(Ident, (Type, [Type]))] -> S.Set String -> Int -> [(String, ((Type, [Type]), Int))]
addOffsetsToMethods [] _ _                       = []
addOffsetsToMethods ((Ident name, ts):as) superMethods offset =
    if not $ S.member name superMethods then
        (name, (ts, offset)) : addOffsetsToMethods as (S.insert name superMethods) (offset + 4)
    else
        addOffsetsToMethods as superMethods offset

getOrd :: (Ident, ((Maybe Ident, BNFC'Position, Int, [Ident]), Tc.ClassDefS)) -> Int
getOrd (_, ((_, _, ord, _), _)) = ord

convertFromTc :: TypeCheckerS -> PreprocessState ()
convertFromTc tcs = modify $ const
  PreprocessS
    {preclasses = tcClassesToPre (sortBy (compare `on` getOrd) (M.toList (Tc.classEnv tcs))) M.empty}

instance Show ClassDefPre where
    show c = ident c ++ "\n" ++ show (map (\(name, (t, off)) -> (name, pretty t, off)) (M.toList (attrs c))) ++ "\n"
            ++ show (map (\(name, ((r, args), off)) -> (name, pretty r, map pretty args, off)) (M.toList (methods c))) ++ "\n" ++ maybe "Nothing SUPER" ident (super c)
            ++ "\nsize: " ++ show (size c) ++ "\n---------------------------------\n"
