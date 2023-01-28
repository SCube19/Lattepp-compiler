module Compiler.Allocator.Data where
import           Compiler.Data
import           Control.Monad.Except (ExceptT)
import           Control.Monad.State  (StateT, gets, modify)
import qualified Data.Map             as M
import qualified Data.Set             as S
import           Quadruples.Data      (Register)


type AllocatorState = StateT AllocatorS (ExceptT String IO)

data AllocatorS = AllocatorS {
    allocation      :: M.Map Register AsmOperand,
    integers        :: S.Set Register,
    usedStackOffset :: Offset,
    allocSize       :: Int,
    memoryPool      :: [AsmOperand],
    fusage          :: M.Map Register Int,
    lusage          :: M.Map Register Int
}

initRegPool :: [AsmOperand]
initRegPool = [RegOp RBX, RegOp R12, RegOp R13, RegOp R14, RegOp R15]

initAllocatorS :: Int -> AllocatorS
initAllocatorS init = AllocatorS {
    allocation = M.empty,
    integers = S.empty,
    usedStackOffset = -8 * (1 + init),
    allocSize = 0,
    memoryPool = initRegPool,
    fusage = M.empty,
    lusage = M.empty
}

getStackOffset :: AllocatorState Offset
getStackOffset = do
    off <- gets usedStackOffset
    modify (\s -> AllocatorS {
        allocation = allocation s,
        integers = integers s,
        usedStackOffset = usedStackOffset s - 8,
        allocSize = allocSize s + 1,
        memoryPool = memoryPool s,
        fusage = fusage s,
        lusage = lusage s
    })
    return off

allocate :: Register -> AsmOperand -> AllocatorState ()
allocate reg mem = modify (\s -> AllocatorS {
    allocation = M.insert reg mem (allocation s),
    integers = case mem of
                ValOp _ -> S.insert reg (integers s)
                RegOp _ -> integers s
                MemOp _ -> integers s,
    allocSize = allocSize s,
    usedStackOffset = usedStackOffset s,
    memoryPool = memoryPool s,
    fusage = fusage s,
    lusage = lusage s
})

insertFirstUsage :: Register -> Int -> AllocatorState ()
insertFirstUsage r i = modify (\s -> AllocatorS {
        allocation = allocation s,
        integers = integers s,
        usedStackOffset = usedStackOffset s,
        allocSize = allocSize s,
        memoryPool = memoryPool s,
        fusage = M.insertWith min r i (fusage s),
        lusage = lusage s
    })

insertLastUsage :: Register -> Int -> AllocatorState ()
insertLastUsage r i = modify (\s -> AllocatorS {
        allocation = allocation s,
        integers = integers s,
        usedStackOffset = usedStackOffset s,
        allocSize = allocSize s,
        memoryPool = memoryPool s,
        fusage = fusage s,
        lusage = M.insertWith max r i (lusage s)
    })

addToPool :: AsmOperand -> AllocatorState ()
addToPool op = modify (\s -> AllocatorS {
    allocation = allocation s,
    integers = integers s,
    usedStackOffset = usedStackOffset s,
    allocSize = allocSize s,
    memoryPool = op : memoryPool s,
    fusage = fusage s,
    lusage = lusage s
})

shrinkPool :: AllocatorState ()
shrinkPool = modify (\s ->
    let (m:ms) = memoryPool s in
        AllocatorS {
        allocation = allocation s,
        integers = integers s,
        usedStackOffset = usedStackOffset s,
        allocSize = allocSize s,
        memoryPool = ms,
        fusage = fusage s,
        lusage = lusage s
    })

