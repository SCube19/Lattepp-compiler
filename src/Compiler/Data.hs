module Compiler.Data where
import           Compiler.Quadruples.Data   (Register)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.State  (StateT, gets, modify)
import           Data.List                  (intercalate)
import qualified Data.Map                   as M

type CompilerState = StateT CompilerS (ExceptT String IO)

newtype CompilerS = CompilerS {
    instructions    :: [AsmInstr]
}

initCompilerS :: CompilerS
initCompilerS = CompilerS {
    instructions = []
}

addInstr :: AsmInstr -> CompilerState ()
addInstr i = modify (\s -> CompilerS {
    instructions = instructions s ++ [i]
})

newtype AsmLabel = AsmLabel String

newtype Offset = Offset Int

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
    Je AsmLabel |
    Jne AsmLabel |
    Jl AsmLabel |
    Jle AsmLabel |
    Jg AsmLabel |
    Jge AsmLabel |
    Inc AsmOperand |
    Dec AsmOperand |
    Lea OpSize AsmOperand AsmOperand |
    Leave |
    Mov OpSize AsmOperand AsmOperand |
    Neg OpSize AsmOperand |
    Not OpSize AsmOperand |
    Push OpSize AsmOperand |
    Ret |
    Section String |
    Extern String |
    Text String |
    Global String |
    DataString String String |
    DataBuffer String Int

instance Show Offset where
    show (Offset i) = show i

instance Show AsmLabel where
    show (AsmLabel l) = l

instance Show AsmValue where
    show (VInt v)   = show v
    show (VLabel l) = show l

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
    show Cdq           = "\tcdq"
    show (Cmp s o1 o2)   = "\tcmp " ++ show s ++ " " ++ show o1 ++ ", " ++ show o2 ++ "\n"
    show (ILabel l1)   = show l1 ++ ":\n"
    show (Je l1)       = "\tje " ++ show l1 ++ "\n"
    show (Jne l1)      = "\njne " ++ show l1 ++ "\n"
    show (Jl l1)       = "\njl " ++ show l1 ++ "\n"
    show (Jle l1)      = "\njle " ++ show l1 ++ "\n"
    show (Jg l1)       = "\njg " ++ show l1 ++ "\n"
    show (Jge l1)      = "\njge " ++ show l1 ++ "\n"
    show (Inc o1)      = "\ninc " ++ show o1 ++ "\n"
    show (Dec o1)      = "\ndec " ++ show o1 ++ "\n"
    show (Lea s o1 o2) = "\nlea " ++ show s ++ " " ++ show o1 ++ ", " ++ show o2 ++ "\n"
    show Leave         = "\nleave"
    show (Mov s o1 o2) = "\nmov " ++ show s ++ " " ++ show o1 ++ ", " ++ show o2 ++ "\n"
    show (Neg s o1)    = "\nneg " ++ show s ++ " " ++ show o1 ++ "\n"
    show (Not s o1)    = "\nnot " ++ show s ++ " " ++ show o1 ++ "\n"
    show (Push s o1)   = "\npush " ++ show s ++ " " ++ show o1 ++ "\n"
    show Ret           = "\nret"
    show (Section s)   = "section ." ++ s ++ "\n"
    show (Extern s)    = "extern " ++ s ++ "\n"
    show (Text s)      = "text " ++ s ++ "\n"
    show (Global s)    = "global " ++ s ++ "\n"
    show (DataString l s) = l ++ " db '" ++ s ++ "', 0\n"
    show (DataBuffer l size) = l ++
        if size `mod` 4 == 0 then " dd " else if even size then " dw " else " db " ++
        if size `mod` 4 == 0 then intercalate "," (replicate (div size 4) "0")
        else if even size then intercalate "," (replicate (div size 2) "0")
        else intercalate "," (replicate size "0") ++ "\n"





type AllocatorState = StateT AllocatorS (ExceptT String IO)

data AllocatorS = AllocatorS {
    allocation      :: MemoryAllocation,
    usedStackOffset :: Int,
    memoryPool      :: [AsmMem],
    usage           :: M.Map Register (Int, Int)
}

type MemoryAllocation = M.Map Register AsmMem

initAllocatorS :: Int -> AllocatorS
initAllocatorS init = AllocatorS {
    allocation = M.empty,
    usedStackOffset = -4 * (1 + init),
    memoryPool = [],
    usage = M.empty
}

getStackOffset :: AllocatorState Int
getStackOffset = do
    off <- gets usedStackOffset
    modify (\s -> AllocatorS {
        allocation = allocation s,
        usedStackOffset = usedStackOffset s - 4,
        memoryPool = memoryPool s,
        usage = usage s
    })
    return off

allocate :: Register -> AsmMem -> AllocatorState ()
allocate reg mem = modify (\s -> AllocatorS {
    allocation = M.insert reg mem (allocation s),
    usedStackOffset = usedStackOffset s - 4,
    memoryPool = memoryPool s,
    usage = usage s
})
