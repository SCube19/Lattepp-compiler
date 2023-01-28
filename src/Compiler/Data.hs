module Compiler.Data where
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.State  (StateT, gets, modify)
import           Data.List                  (intercalate)
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import           Quadruples.Data            (QClass, QLabel (QLabel), Register)
import           Utils                      (noheadtail)

type CompilerState = StateT CompilerS (ExceptT String IO)

data CompilerS = CompilerS {
    instructions :: [AsmInstr],
    mapping      :: M.Map Register AsmOperand,
    usedMem      :: Int,
    classes      :: M.Map String QClass
}

initCompilerS :: M.Map String QClass -> CompilerS
initCompilerS cs = CompilerS {
    instructions = [],
    usedMem = 0,
    mapping = M.empty,
    classes = cs
}

addInstr :: AsmInstr -> CompilerState ()
addInstr i = modify (\s -> CompilerS {
    instructions = instructions s ++ [i],
    mapping = mapping s,
    usedMem = usedMem s,
    classes = classes s
})

setAllocation :: M.Map Register AsmOperand -> Int -> CompilerState ()
setAllocation al mem = modify (\s -> CompilerS {
    instructions = instructions s,
    usedMem = mem,
    mapping = al,
    classes = classes s
})

resetAllocation :: CompilerState ()
resetAllocation = modify (\s -> CompilerS {
    instructions = instructions s,
    usedMem = 0,
    mapping = M.empty,
    classes = classes s
})

updateMapping :: Register -> AsmOperand -> CompilerState ()
updateMapping reg op = modify (\s -> CompilerS {
    instructions = instructions s,
    mapping = M.insert reg op (mapping s),
    usedMem = usedMem s,
    classes = classes s
})


newtype AsmLabel = AsmLabel String deriving Eq

labelref :: QLabel -> String
labelref (QLabel l) = "L" ++ show l

qtoasmlbl :: QLabel -> AsmLabel
qtoasmlbl (QLabel l) = AsmLabel $ "L" ++ show l

type Offset = Int

data AsmValue = VInt Int | VLabel AsmLabel deriving Eq

data AsmRegister =
    RAX |
    RBX |
    RCX |
    RDX |
    RBP |
    RSP |
    RSI |
    RDI |
    R8  |
    R9  |
    R10 |
    R11 |
    R12 |
    R13 |
    R14 |
    R15 deriving Eq


data AsmMem =
    RegOff AsmRegister Offset |
    LblOff AsmLabel Offset |
    IndirMem AsmRegister Offset AsmRegister Offset deriving Eq

data AsmOperand =
    ValOp AsmValue |
    RegOp AsmRegister |
    MemOp AsmMem deriving Eq

isAsmValue :: AsmOperand -> Bool
isAsmValue (ValOp _) = True
isAsmValue (RegOp _) = False
isAsmValue (MemOp _) = False

isAsmReg :: AsmOperand -> Bool
isAsmReg (ValOp _) = False
isAsmReg (RegOp _) = True
isAsmReg (MemOp _) = False

data CallTarget =
    CReg AsmRegister |
    CLabel AsmLabel deriving Eq

data OpSize =
    Byte |
    Word |
    DWord |
    QWord deriving Eq

data AsmInstr =
    Add OpSize AsmOperand AsmOperand |
    Sub OpSize AsmOperand AsmOperand |
    Mul OpSize AsmOperand AsmOperand |
    Div OpSize AsmOperand |
    Call CallTarget |
    Cdq |
    Cmp OpSize AsmOperand AsmOperand |
    ILabel AsmLabel |
    And OpSize AsmOperand AsmOperand |
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
    Enter Int|
    Mov OpSize AsmOperand AsmOperand |
    Neg OpSize AsmOperand |
    Not OpSize AsmOperand |
    Push OpSize AsmOperand |
    Pop OpSize AsmOperand |
    Ret |
    Clear OpSize AsmOperand |
    Section String |
    Extern String |
    Text String |
    Global String |
    DataString String String |
    DataBuffer String Int deriving Eq

instance Show AsmLabel where
    show (AsmLabel l) = l

instance Show AsmValue where
    show (VInt v)              = show v
    show (VLabel (AsmLabel l)) = filter (/= ':') l

instance Show AsmRegister where
    show RAX = "rax"
    show RBX = "rbx"
    show RCX = "rcx"
    show RDX = "rdx"
    show RBP = "rbp"
    show RSP = "rsp"
    show RSI = "rsi"
    show RDI = "rdi"
    show R8  = "r8"
    show R9  = "r9"
    show R10 = "r10"
    show R11 = "r11"
    show R12 = "r12"
    show R13 = "r13"
    show R14 = "r14"
    show R15 = "r15"

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
    show Cdq           = "\tcqo\n"
    show (Cmp s o1 o2) = "\tcmp " ++ show s ++ " " ++ show o1 ++ ", " ++ show o2 ++ "\n"
    show (ILabel l1)   = show l1 ++ ":\n"
    show (And s o1 o2)= "\tand " ++ show s ++ " " ++ show o1 ++ ", " ++ show o2 ++ "\n"
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
    show (Enter size)  = "\tenter " ++ show size ++ ", 0\n"
    show (Mov s o1 o2) = "\tmov " ++ show s ++ " " ++ show o1 ++ ", " ++ show o2 ++ "\n"
    show (Neg s o1)    = "\tneg " ++ show s ++ " " ++ show o1 ++ "\n"
    show (Not s o1)    = "\tnot " ++ show s ++ " " ++ show o1 ++ "\n"
    show (Push s o1)   = "\tpush " ++ show s ++ " " ++ show o1 ++ "\n"
    show (Pop s o1)    = "\tpop " ++ show s ++ " " ++ show o1 ++ "\n"
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


