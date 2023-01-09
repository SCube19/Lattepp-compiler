module Compiler.Data where
import           Compiler.Quadruples.Data   (QLabel (QLabel), Register)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.State  (StateT, gets, modify)
import           Data.List                  (intercalate)
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import           Utils                      (noheadtail)

type CompilerState = StateT CompilerS (ExceptT String IO)

data CompilerS = CompilerS {
    instructions :: [AsmInstr],
    mapping      :: MemoryAllocation,
    usedMem      :: Int
}

initCompilerS :: CompilerS
initCompilerS = CompilerS {
    instructions = [],
    usedMem = 0,
    mapping = M.empty
}

addInstr :: AsmInstr -> CompilerState ()
addInstr i = modify (\s -> CompilerS {
    instructions = instructions s ++ [i],
    mapping = mapping s,
    usedMem = usedMem s
})

setAllocation :: MemoryAllocation -> Int -> CompilerState ()
setAllocation al mem = modify (\s -> CompilerS {
    instructions = instructions s,
    usedMem = mem,
    mapping = al
})

resetAllocation :: CompilerState ()
resetAllocation = modify (\s -> CompilerS {
    instructions = instructions s,
    usedMem = 0,
    mapping = M.empty
})

newtype AsmLabel = AsmLabel String

labelref :: QLabel -> String
labelref (QLabel l) = "L" ++ show l

qtoasmlbl :: QLabel -> AsmLabel
qtoasmlbl (QLabel l) = AsmLabel $ "L" ++ show l

type Offset = Int

data AsmValue = VInt Int | VLabel AsmLabel

data AsmRegister =
    EAX |
    EBX |
    ECX |
    EDX |
    EBP |
    ESP

data AsmMem =
    RegOff AsmRegister Offset |
    LblOff AsmLabel Offset |
    IndirMem AsmRegister Offset AsmRegister Offset

data AsmOperand =
    ValOp AsmValue |
    RegOp AsmRegister |
    MemOp AsmMem

isAsmValue :: AsmOperand -> Bool
isAsmValue (ValOp _) = True
isAsmValue (RegOp _) = False
isAsmValue (MemOp _) = False

data CallTarget =
    CReg AsmRegister |
    CLabel AsmLabel

data OpSize =
    Byte |
    Word |
    DWord |
    QWord

data AsmInstr =
    Add OpSize AsmOperand AsmOperand |
    Sub OpSize AsmOperand AsmOperand |
    Mul OpSize AsmOperand AsmOperand |
    Div OpSize AsmOperand |
    Call CallTarget |
    Cdq |
    Cmp OpSize AsmOperand AsmOperand |
    ILabel AsmLabel |
    Jmp AsmLabel |
    Je AsmLabel |
    Jne AsmLabel |
    Jl AsmLabel |
    Jle AsmLabel |
    Jg AsmLabel |
    Jge AsmLabel |
    Inc OpSize AsmOperand |
    Dec OpSize AsmOperand |
    Lea OpSize AsmOperand AsmOperand |
    Leave |
    Mov OpSize AsmOperand AsmOperand |
    Neg OpSize AsmOperand |
    Not OpSize AsmOperand |
    Push OpSize AsmOperand |
    Ret |
    Clear OpSize AsmOperand |
    Section String |
    Extern String |
    Text String |
    Global String |
    DataString String String |
    DataBuffer String Int

instance Show AsmLabel where
    show (AsmLabel l) = l

instance Show AsmValue where
    show (VInt v)              = show v
    show (VLabel (AsmLabel l)) = filter (/= ':') l

instance Show AsmRegister where
    show EAX = "eax"
    show EBX = "ebx"
    show ECX = "ecx"
    show EDX = "edx"
    show EBP = "ebp"
    show ESP = "esp"

instance Show AsmMem where
    show (RegOff r off) = "[" ++ show r ++ "+" ++ show off ++ "]"
    show (LblOff (AsmLabel l) off) = "[[" ++ l ++ "]" ++ "+" ++ show off ++ "]"
    show (IndirMem r1 off1 r2 off2) = "[" ++ show r1 ++ "+" ++ show off1 ++ "+" ++ show r2 ++ "*" ++ show r2 ++ "]"

instance Show AsmOperand where
    show (ValOp x) = show x
    show (RegOp x) = show x
    show (MemOp x) = show x

instance Show CallTarget where
    show (CReg r)   = show r
    show (CLabel l) = show l

instance Show OpSize where
    show Byte  = "BYTE"
    show Word  = "WORD"
    show DWord = "DWORD"
    show QWord = "QWORD"

instance Show AsmInstr where
    show (Add s o1 o2) = "\tadd " ++ show s ++ " " ++ show o1 ++ ", " ++ show o2 ++ "\n"
    show (Sub s o1 o2) = "\tsub " ++ show s ++ " " ++ show o1 ++ ", " ++ show o2 ++ "\n"
    show (Mul s o1 o2) = "\timul " ++ show s ++ " " ++ show o1 ++ ", " ++ show o2 ++ "\n"
    show (Div s o1)    = "\tidiv " ++ show s ++ " " ++ show o1 ++ "\n"
    show (Call trgt)   = "\tcall " ++ show trgt ++ "\n"
    show Cdq           = "\tcdq\n"
    show (Cmp s o1 o2) = "\tcmp " ++ show s ++ " " ++ show o1 ++ ", " ++ show o2 ++ "\n"
    show (ILabel l1)   = show l1 ++ ":\n"
    show (Jmp l1)      = "\tjmp " ++ show l1 ++ "\n"
    show (Je l1)       = "\tje " ++ show l1 ++ "\n"
    show (Jne l1)      = "\tjne " ++ show l1 ++ "\n"
    show (Jl l1)       = "\tjl " ++ show l1 ++ "\n"
    show (Jle l1)      = "\tjle " ++ show l1 ++ "\n"
    show (Jg l1)       = "\tjg " ++ show l1 ++ "\n"
    show (Jge l1)      = "\tjge " ++ show l1 ++ "\n"
    show (Inc s o1)      = "\tinc " ++ show s ++ " " ++ show o1 ++ "\n"
    show (Dec s o1)      = "\tdec " ++ show s ++ " " ++ show o1 ++ "\n"
    show (Lea s o1 o2) = "\tlea " ++ show s ++ " " ++ show o1 ++ ", " ++ show o2 ++ "\n"
    show Leave         = "\tleave\n"
    show (Mov s o1 o2) = "\tmov " ++ show s ++ " " ++ show o1 ++ ", " ++ show o2 ++ "\n"
    show (Neg s o1)    = "\tneg " ++ show s ++ " " ++ show o1 ++ "\n"
    show (Not s o1)    = "\tnot " ++ show s ++ " " ++ show o1 ++ "\n"
    show (Push s o1)   = "\tpush " ++ show s ++ " " ++ show o1 ++ "\n"
    show Ret           = "\tret\n"
    show (Clear s o1)  = "\txor " ++ show s ++ " " ++ show o1 ++ ", " ++ show o1 ++ "\n"
    show (Section s)   = "section ." ++ s ++ "\n"
    show (Extern s)    = "extern " ++ s ++ "\n"
    show (Text s)      = "text " ++ s ++ "\n"
    show (Global s)    = "global " ++ s ++ "\n"
    show (DataString l s) = "\t" ++ l ++ " db `" ++ noheadtail (show s) ++ "`, 0\n"
    show (DataBuffer l size) = "\t" ++ l ++
        if size `mod` 4 == 0 then " dd " else if even size then " dw " else " db " ++
        if size `mod` 4 == 0 then intercalate "," (replicate (div size 4) "0")
        else if even size then intercalate "," (replicate (div size 2) "0")
        else intercalate "," (replicate size "0") ++ "\n"





type AllocatorState = StateT AllocatorS (ExceptT String IO)

data AllocatorS = AllocatorS {
    allocation      :: MemoryAllocation,
    integers        :: S.Set Register,
    usedStackOffset :: Offset,
    allocSize       :: Int,
    memoryPool      :: [AsmOperand],
    fusage          :: M.Map Register Int,
    lusage          :: M.Map Register Int
}

type MemoryAllocation = M.Map Register AsmOperand

initAllocatorS :: Int -> AllocatorS
initAllocatorS init = AllocatorS {
    allocation = M.empty,
    integers = S.empty,
    usedStackOffset = -4 * (1 + init),
    allocSize = 0,
    memoryPool = [],
    fusage = M.empty,
    lusage = M.empty
}

getStackOffset :: AllocatorState Offset
getStackOffset = do
    off <- gets usedStackOffset
    modify (\s -> AllocatorS {
        allocation = allocation s,
        integers = integers s,
        usedStackOffset = usedStackOffset s - 4,
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

