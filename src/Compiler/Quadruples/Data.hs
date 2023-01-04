module Compiler.Quadruples.Data where
import qualified Compiler.Quadruples.Predata as Pre
import           Control.Monad.Trans.Except  (ExceptT)
import           Control.Monad.Trans.State   (StateT)
import           Data.List                   (intercalate)
import qualified Data.Map                    as M
import           Data.Maybe                  (fromJust, fromMaybe, isNothing)
import           Syntax.AbsLattepp           (Ident, Program, Type,
                                              Type' (ObjectType))

type QuadruplesState = StateT QuadrupleS (ExceptT String IO)
data QuadrupleS = QuadrupleS {
    preprocessing :: Pre.PreprocessS,
    qprogram      :: QProgram,
    nextReg       :: Register,
    nextLabel     :: QLabel
}

initQuadruplesS :: QuadrupleS
initQuadruplesS = QuadrupleS {
    preprocessing = Pre.initPreprocessS,
    qprogram = QProgram {
        classes = [],
        funcs = [],
        strings = M.empty
    },
    nextReg = Register 1,
    nextLabel = QLabel 1
}

setPreprocessing :: QuadrupleS -> Pre.PreprocessS -> QuadrupleS
setPreprocessing s prep = QuadrupleS {
    preprocessing = prep,
    qprogram = qprogram s,
    nextReg = nextReg s,
    nextLabel = nextLabel s
}

addClass :: QuadrupleS -> QClass -> QuadrupleS
addClass s c = QuadrupleS {
    preprocessing = preprocessing s,
    qprogram = _addClass (qprogram s) c,
    nextReg = nextReg s,
    nextLabel = nextLabel s
}

_addClass :: QProgram -> QClass -> QProgram
_addClass p c = QProgram {
    classes = c : classes p,
    funcs = funcs p,
    strings = strings p
}

newtype Register = Register Integer

data QProgram = QProgram {
    classes :: [QClass],
    funcs   :: [QFun],
    strings :: M.Map QLabel String
}

data QFun = QFun {
    fident     :: String,
    localcount :: Int,
    body       :: [Quadruple]
}

data QClass = QClass {
    cident  :: String,
    fields  :: [QCField],
    methods :: [String],
    super   :: Maybe QClass
}

classDefPreToQClass :: Pre.ClassDefPre -> QClass
classDefPreToQClass c = QClass {
    cident = Pre.ident c,
    fields = preFieldsToQFields (Pre.attrs c),
    methods = map fst (M.toList $ Pre.methods c),
    super = case Pre.super c of
      Nothing -> Nothing
      Just c2 -> Just $ classDefPreToQClass c2
}

data QCField = QCField {
    fieldName :: String,
    fieldSize :: Int
}

preFieldsToQFields :: M.Map String Type -> [QCField]
preFieldsToQFields fs = map (\(f, s) -> QCField {fieldName = f, fieldSize = 4}) (M.toList fs)

newtype QLabel = QLabel Int

newtype QIndex = QIndex Int

newtype Offset = Offset Int

data QValue = QBool Bool | QInt Int

qvalueInt :: QValue -> Int
qvalueInt (QBool b) = if b then 1 else 0
qvalueInt (QInt i)  = i

qvalueBool :: QValue -> Bool
qvalueBool (QBool b) = b
qvalueBool (QInt i)  = i /= 0
data Quadruple =
    Add Register Register Register |
    Sub Register Register Register |
    Div Register Register Register |
    Mul Register Register Register |
    Mod Register Register Register |
    Cmp Register Register |
    Jmp QLabel |
    Je QLabel QLabel |
    Jge QLabel QLabel |
    Jg QLabel QLabel |
    Jle QLabel QLabel |
    Jl QLabel QLabel |
    Neg Register Register | -- consider one register
    Not Register Register | -- consider one register
    Concat Register Register Register |
    MovV QValue Register |
    Mov Register Register |
    Ret Register |
    Vret |
    Label QLabel |
    Load QIndex Register |
    LoadArg QIndex QIndex |
    LoadIndir Register Offset Register Offset Register |
    LoadLbl QLabel Register |
    Store QIndex Register |
    StoreIndir Register Offset Register Offset Register |
    Alloc QIndex |
    Call QLabel [Register] Register |
    VCall QLabel [Register] Register Offset Register |
    Vtab Register Type



instance Show QLabel where
    show (QLabel i) = "L" ++ show i

instance Show QIndex where
    show (QIndex i) = "i" ++ show i

instance Show Offset where
    show (Offset o) = show o

instance Show QValue where
    show (QBool b) = show b
    show (QInt i)  = show i

instance Show Register where
    show (Register r) = "r" ++ show r

instance Show QProgram where
    show q = concat $ map show (M.toList $ strings q) ++ map show (classes q) ++ map show (funcs q)

instance Show QClass where
    show c = "class " ++ cident c ++ if isNothing (super c) then "" else ("$" ++ cident (fromMaybe c (super c))) ++ "\n" ++ concat (map show (fields c) ++ map show (methods c)) ++ "endclass"

instance Show QCField where
    show field = fieldName field ++ "^" ++ show (fieldSize field) ++ "\n"

instance Show QFun where
    show f = fident f ++ ":\n" ++ concatMap show (body f)

instance Show Quadruple where
    show (Add r1 r2 result) = show result ++ "=" ++ show r1 ++ "+" ++ show r2
    show (Sub r1 r2 result) = show result ++ "=" ++ show r1 ++ "-" ++ show r2
    show (Div r1 r2 result) = show result ++ "=" ++ show r1 ++ "/" ++ show r2
    show (Mul r1 r2 result) = show result ++ "=" ++ show r1 ++ "*" ++ show r2
    show (Mod r1 r2 result) = show result ++ "=" ++ show r1 ++ "%" ++ show r2
    show (Cmp r1 r2) = "cmp " ++ show r1 ++ ", " ++ show r2
    show (Jmp label) = "jmp " ++ show label
    show (Je l1 l2) = "je " ++ show l1 ++ " else " ++ show l2
    show (Jge l1 l2) = "jge " ++ show l1 ++ " else " ++ show l2
    show (Jg l1 l2) = "jg " ++ show l1 ++ " else " ++ show l2
    show (Jle l1 l2) = "jle " ++ show l1 ++ " else " ++ show l2
    show (Jl l1 l2) = "jl " ++ show l1 ++ " else " ++ show l2
    show (Neg r1 result) = show result ++ "=" ++ " -" ++ show r1
    show (Not r1 result) = show result ++ "=" ++ " !" ++ show r1
    show (Concat r1 r2 result) = show result ++ "=concat(" ++ show r1 ++ ", " ++ show r2 ++ ")"
    show (MovV val r1) = "mov " ++ show r1 ++ ", " ++ show val
    show (Mov r1 r2) = "mov " ++ show r2 ++ ", " ++ show r1
    show (Ret r1) = "ret " ++ show r1
    show Vret = "ret"
    show (Label label) = show label
    show (Load index r1) = "load " ++ show r1 ++ ", " ++ show index
    show (LoadArg i1 i2) = "arg " ++ show i1 ++ ", " ++ show i2
    show (LoadIndir addr offset1 offsetreg offset2 result) = show result ++ "= ptr [" ++ show addr ++ "+" ++ show offset1 ++ "+" ++ show offsetreg ++ "*" ++ show offset2 ++ "]"
    show (LoadLbl label r1) = show r1 ++ "= ptr " ++ show label
    show (Store index r1) = "store " ++ show r1 ++ ", " ++ show index
    show (StoreIndir addr offset1 offsetreg offset2 value) = "store " ++ "[" ++ show addr ++ "+" ++ show offset1 ++ "+" ++ show offsetreg ++ "*" ++ show offset2 ++ "], " ++ show value
    show (Alloc index) = "alloc " ++ show index
    show (Call label args result) = show result ++ "=call " ++ show label ++ "(" ++ intercalate "," (map show args) ++ ")"
    show (VCall label args result _ _) = show result ++ "=vcall " ++ show label ++ "(" ++ intercalate "," (map show args)++ ")"
    show (Vtab r1 type1) = "vt " ++ show type1 ++ ", " ++ show r1




