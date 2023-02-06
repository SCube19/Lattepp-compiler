module Quadruples.Optimizer.Optimizer where
import           Compiler.Data                  (AsmOperand (ValOp),
                                                 AsmValue (VInt))
import           Control.Monad.Except           (MonadIO (liftIO), runExcept,
                                                 runExceptT, when)
import           Control.Monad.State            (MonadTrans (lift),
                                                 StateT (runStateT), evalState,
                                                 evalStateT, gets, modify,
                                                 unless)
import           Data.List                      (sort)
import qualified Data.Map                       as M
import qualified Data.Set                       as S
import           GHC.TopHandler                 (runIO)
import           Quadruples.Data
import           Quadruples.Optimizer.Data      (OptimizerS (copy, fusage, indexRemovalMapping, indexfusage, indexlusage, localRemoved, lusage, quadAcc),
                                                 OptimizerState, addAcc,
                                                 decreaseRemovalMapping,
                                                 initOptimizerS,
                                                 initRemovalMapping,
                                                 insertFirstIndexUsage,
                                                 insertFirstUsage,
                                                 insertLastIndexUsage,
                                                 insertLastUsage, makeCopy,
                                                 mapIndex, mapIntIndex,
                                                 removeIndex, resetAcc,
                                                 resetCopy)
import           Quadruples.Optimizer.Lcse.Data (initLcseS)
import           Quadruples.Optimizer.Lcse.Lcse (lcse)
import           Quadruples.Optimizer.Utils     (basicBlockBoundry,
                                                 extractIndex)
import           Utils                          (rawInt)

optimizeQProgram :: QProgram -> OptimizerState QProgram
optimizeQProgram p = do
    res <- mapM optimizeFuncs (M.toList $ funcs p)
    return $ QProgram (classes p) (M.fromList res) (strings p)

optimizeFuncs :: (String, QFun) -> OptimizerState (String, QFun)
optimizeFuncs (name, fun) = do
    modify (const initOptimizerS)
    optimizeQuads (body fun) (localcount fun)
    res <- gets quadAcc
    lr <- gets localRemoved
    return (name, QFun {
        fident = fident fun,
        localcount = localcount fun - length lr,
        ret = ret fun,
        body = reverse res,
        offset = offset fun})

optimizeQuads :: [Quadruple] -> Int -> OptimizerState ()
optimizeQuads qs size = do
    qs1 <- optRes $ copyPropagation qs
    liveRangesIndexes qs1 0
    qs2 <- optRes $ deadIndexes qs1
    qs3 <- optRes $ fillHoles qs2 size
    qs4 <- lift $ evalStateT (lcse qs3) initLcseS
    liveRanges qs4 0
    deadRegisters qs4

optRes :: OptimizerState () -> OptimizerState [Quadruple]
optRes action = do
    action
    res <- gets quadAcc
    resetAcc
    return $ reverse res

copyPropagation :: [Quadruple] -> OptimizerState ()
copyPropagation [] = return ()

copyPropagation ((Load index1 r1):(Store r2 index2):qs) = do
    copies <- gets copy
    let getCp = flip mapIndex copies
    if r1 == r2 then do
        makeCopy index2 (getCp index1)
    else do
        makeCopy index2 index2
    addAcc (Load (getCp index1) r1)
    addAcc (Store r2 index2)
    copyPropagation qs

copyPropagation (q:qs) = do
    if basicBlockBoundry q then do
        resetCopy
        addAcc q
        copyPropagation qs
    else do
        copies <- gets copy
        let getCp = flip mapIndex copies
        case q of
            Inc qi -> do
                addAcc (Inc qi)
                makeCopy qi qi
            Dec qi -> do
                addAcc (Dec qi)
                makeCopy qi qi
            Load qi reg -> addAcc (Load (getCp qi) reg)
            Store reg qi@(QIndex i t) -> do
                addAcc (Store reg qi)
                makeCopy qi qi
            q -> addAcc q
        copyPropagation qs

deadRegisters :: [Quadruple] -> OptimizerState ()
deadRegisters []     = return ()
deadRegisters (q:qs) = do
    f <- gets fusage
    l <- gets lusage
    let result = extractResult q
    case result of
      Nothing -> do
        addAcc q
        deadRegisters qs
      Just reg ->
        case M.lookup reg f of
            Nothing -> undefined
            Just n  ->
                case M.lookup reg l of
                    Nothing -> undefined
                    Just m -> do
                        when (n /= m || isCall q) (addAcc q)
                        deadRegisters qs


liveRanges :: [Quadruple] -> Int -> OptimizerState ()
liveRanges [] _ = return ()
liveRanges (q:qs) i = do
    let all = extractAll q
    _liveRanges all i
    liveRanges qs (i + 1)

_liveRanges :: [Register] -> Int -> OptimizerState ()
_liveRanges rs i = do
    mapM_ (\r -> do
        insertFirstUsage r i
        insertLastUsage r i) rs


deadIndexes :: [Quadruple] -> OptimizerState ()
deadIndexes [] = return ()
deadIndexes (q:qs) = do
    f <- gets indexfusage
    l <- gets indexlusage
    let index = extractIndex q
    case index of
      Nothing -> do
        addAcc q
        deadIndexes qs
      Just ind@(QIndex i _) ->
        case M.lookup ind f of
            Nothing -> undefined
            Just n ->
                case M.lookup ind l of
                    Nothing -> undefined
                    Just m -> do
                        if n /= m then
                            addAcc q
                        else
                            removeIndex i
                        deadIndexes qs


liveRangesIndexes :: [Quadruple] -> Int -> OptimizerState ()
liveRangesIndexes [] _ = return ()
liveRangesIndexes (q:qs) i = do
    let index = extractIndex q
    case index of
      Nothing -> return ()
      Just qi -> _liveRangesIndex qi i
    liveRangesIndexes qs (i + 1)

_liveRangesIndex :: QIndex -> Int -> OptimizerState ()
_liveRangesIndex qi i = do
    insertFirstIndexUsage qi i
    insertLastIndexUsage qi i


fillHoles :: [Quadruple] -> Int -> OptimizerState ()
fillHoles qs size = do
    lr <- gets localRemoved
    initRemovalMapping size
    createRemovalMapping (sort lr) size
    mapIndexes qs


createRemovalMapping :: [Int] -> Int -> OptimizerState ()
createRemovalMapping [] _ = return ()
createRemovalMapping (i:is) size = do
    _createRemovalMapping i size
    createRemovalMapping is size

_createRemovalMapping :: Int -> Int -> OptimizerState ()
_createRemovalMapping curr size = do
    unless (curr == size) (do
        decreaseRemovalMapping curr
        _createRemovalMapping (curr + 1) size)

mapIndexes :: [Quadruple] -> OptimizerState ()
mapIndexes [] = return ()
mapIndexes (q:qs) = do
    mapping <- gets indexRemovalMapping
    let mind = flip mapIntIndex mapping
    case q of
        Inc qi       -> addAcc $ Inc (mind qi)
        Dec qi       -> addAcc $ Dec (mind qi)
        Load qi r    -> addAcc $ Load (mind qi) r
        LoadArg i qi -> addAcc $ LoadArg i (mind qi)
        Store r qi   -> addAcc $ Store r (mind qi)
        _            -> addAcc q
    mapIndexes qs

isCall :: Quadruple -> Bool
isCall Call {}      = True
isCall VCall {}     = True
isCall VoidVCall {} = True
isCall VoidCall {}  =  True
isCall _            = False
