module Compiler.Allocator.Allocator where
import           Compiler.Allocator.Data (AllocatorS (allocSize, allocation, fusage, integers, lusage, memoryPool, usedStackOffset),
                                          AllocatorState, addToPool, allocate,
                                          getStackOffset, insertFirstUsage,
                                          insertLastUsage, shrinkPool)
import           Compiler.Data           (AsmMem (RegOff),
                                          AsmOperand (MemOp, ValOp),
                                          AsmRegister (RBP), AsmValue (VInt))
import           Control.Monad.RWS       (MonadIO (liftIO), MonadState (get),
                                          gets, when)
import           Data.List               (nub)
import qualified Data.Map                as M
import qualified Data.Set                as S
import           Quadruples.Data         (Quadruple (MovV), Register,
                                          extractAll, extractResult, qvalueInt)

allocMemory :: [Quadruple] -> AllocatorState (M.Map Register AsmOperand, Int)
allocMemory qs = do
    defineIntervals qs 0
    makeAllocation qs 0
    alloc <- gets allocation
    size <- gets allocSize
    return (alloc, size)


defineIntervals :: [Quadruple] -> Int -> AllocatorState ()
defineIntervals [] _ = return ()
defineIntervals (q:qs) i = do
    ints <- gets integers
    let all = filter (\x -> not $ S.member x ints) (extractAll q)
    let result = extractResult q
    case result of
      Nothing -> _defineIntervals all  i
      Just reg ->
        case q of
            MovV v _ -> do
                    allocate reg (ValOp (VInt $ qvalueInt v))
                    _defineIntervals all i
            _ -> _defineIntervals all i
    defineIntervals qs (i + 1)

_defineIntervals :: [Register] -> Int -> AllocatorState ()
_defineIntervals rs i = do
    mapM_ (\r -> do
        insertFirstUsage r i
        insertLastUsage r i) rs

makeAllocation :: [Quadruple] -> Int -> AllocatorState ()
makeAllocation [] _ = return ()
makeAllocation (q:qs) i = do
    ints <- gets integers
    let all = nub $ filter (\x -> not $ S.member x ints) (extractAll q)
    fu <- gets fusage
    lu <- gets lusage
    mapM_ (\r -> do
        st1 <- get
        case M.lookup r fu of
            Nothing -> return ()
            Just n  -> do
                when (i == n) (do
                when (null (memoryPool st1)) (do
                    o <- gets usedStackOffset
                    off <- getStackOffset
                    addToPool $ MemOp $ RegOff RBP off)
                st3 <- get
                allocate r (head $ memoryPool st3)
                shrinkPool
                    )
        st2 <- get
        case M.lookup r lu of
          Nothing -> return ()
          Just n  -> when (i == n) (do
                maybe undefined addToPool (M.lookup r (allocation st2)))
            ) all
    makeAllocation qs (i + 1)
