{-# LANGUAGE TupleSections #-}
module Quadruples.Optimizer.Data where
import           Control.Monad.Except (ExceptT)
import           Control.Monad.State  (StateT, gets, modify)
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe)
import           Quadruples.Data      (QIndex (QIndex), Quadruple, Register)
import           Syntax.AbsLattepp    (Type)


type OptimizerState = StateT OptimizerS (ExceptT String IO)
data OptimizerS = OptimizerS {
    quadAcc             :: [Quadruple],
    copy                :: M.Map QIndex QIndex,
    fusage              :: M.Map Register Int,
    lusage              :: M.Map Register Int,
    indexfusage         :: M.Map QIndex Int,
    indexlusage         :: M.Map QIndex Int,
    localRemoved        :: [Int],
    indexRemovalMapping :: M.Map Int Int
}

initOptimizerS :: OptimizerS
initOptimizerS = OptimizerS {
    quadAcc = [],
    copy = M.empty,
    fusage = M.empty,
    lusage = M.empty,
    indexfusage = M.empty,
    indexlusage = M.empty,
    localRemoved = [],
    indexRemovalMapping = M.empty
}

resetAcc :: OptimizerState ()
resetAcc = modify (\s -> OptimizerS {
    quadAcc = [],
    copy = copy s,
    fusage = fusage s,
    lusage = lusage s,
    indexfusage = indexfusage s,
    indexlusage = indexlusage s,
    localRemoved = localRemoved s,
    indexRemovalMapping = indexRemovalMapping s
})

addAcc :: Quadruple -> OptimizerState ()
addAcc q = modify (\s -> OptimizerS {
    quadAcc = q : quadAcc s,
    copy = copy s,
    fusage = fusage s,
    lusage = lusage s,
    indexfusage = indexfusage s,
    indexlusage = indexlusage s,
    localRemoved = localRemoved s,
    indexRemovalMapping = indexRemovalMapping s
})

makeCopy :: QIndex -> QIndex -> OptimizerState ()
makeCopy i1 i2 = modify (\s -> OptimizerS {
    quadAcc = quadAcc s,
    copy = M.insert i1 i2 (copy s),
    fusage = fusage s,
    lusage = lusage s,
    indexfusage = indexfusage s,
    indexlusage = indexlusage s,
    localRemoved = localRemoved s,
    indexRemovalMapping = indexRemovalMapping s
})

insertFirstUsage :: Register -> Int -> OptimizerState ()
insertFirstUsage r i = modify (\s -> OptimizerS {
        quadAcc = quadAcc s,
        copy = copy s,
        fusage = M.insertWith min r i (fusage s),
        lusage = lusage s,
        indexfusage = indexfusage s,
        indexlusage = indexlusage s,
        localRemoved = localRemoved s,
        indexRemovalMapping = indexRemovalMapping s
    })

insertLastUsage :: Register -> Int -> OptimizerState ()
insertLastUsage r i = modify (\s -> OptimizerS {
        quadAcc = quadAcc s,
        copy = copy s,
        fusage = fusage s,
        lusage = M.insertWith max r i (lusage s),
        indexfusage = indexfusage s,
        indexlusage = indexlusage s,
        localRemoved = localRemoved s,
        indexRemovalMapping = indexRemovalMapping s
    })

insertFirstIndexUsage :: QIndex -> Int -> OptimizerState ()
insertFirstIndexUsage r i = modify (\s -> OptimizerS {
        quadAcc = quadAcc s,
        copy = copy s,
        fusage = fusage s,
        lusage = lusage s,
        indexfusage = M.insertWith min r i (indexfusage s),
        indexlusage = indexlusage s,
        localRemoved = localRemoved s,
        indexRemovalMapping = indexRemovalMapping s
    })

insertLastIndexUsage :: QIndex -> Int -> OptimizerState ()
insertLastIndexUsage r i = modify (\s -> OptimizerS {
        quadAcc = quadAcc s,
        copy = copy s,
        fusage = fusage s,
        lusage = lusage s,
        indexfusage = indexfusage s,
        indexlusage = M.insertWith max r i (indexlusage s),
        localRemoved = localRemoved s,
        indexRemovalMapping = indexRemovalMapping s
    })

mapIndex :: QIndex -> M.Map QIndex QIndex -> QIndex
mapIndex r copies = fromMaybe r (M.lookup r copies)

resetCopy :: OptimizerState ()
resetCopy = modify (\s -> OptimizerS {
    quadAcc = quadAcc s,
    copy = M.empty,
    fusage = fusage s,
    lusage = lusage s,
    indexfusage = indexfusage s,
    indexlusage = indexlusage s,
    localRemoved = localRemoved s,
    indexRemovalMapping = indexRemovalMapping s
})

removeIndex :: Int -> OptimizerState ()
removeIndex i = modify (\s -> OptimizerS {
    quadAcc = quadAcc s,
    copy = copy s,
    fusage = fusage s,
    lusage = lusage s,
    indexfusage = indexfusage s,
    indexlusage = indexlusage s,
    localRemoved = i : localRemoved s,
    indexRemovalMapping = indexRemovalMapping s
})

mapIntIndex :: QIndex -> M.Map Int Int -> QIndex
mapIntIndex (QIndex i t) m = case M.lookup i m of
    Nothing -> QIndex i t
    Just n  -> QIndex n t

initRemovalMappng :: Int -> OptimizerState ()
initRemovalMappng size = modify (\s -> OptimizerS {
    quadAcc = quadAcc s,
    copy = copy s,
    fusage = fusage s,
    lusage = lusage s,
    indexfusage = indexfusage s,
    indexlusage = indexlusage s,
    localRemoved = localRemoved s,
    indexRemovalMapping = M.fromList $ map (\a -> (a, a)) [0..size-1]
})

decreaseRemovalMapping :: Int -> OptimizerState ()
decreaseRemovalMapping i1 = modify (\s -> OptimizerS {
    quadAcc = quadAcc s,
    copy = copy s,
    fusage = fusage s,
    lusage = lusage s,
    indexfusage = indexfusage s,
    indexlusage = indexlusage s,
    localRemoved = localRemoved s,
    indexRemovalMapping = M.insertWith (flip (-)) i1 1 (indexRemovalMapping s)
})
