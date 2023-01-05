module Compiler.Quadruples.Data where
import qualified Compiler.Quadruples.Predata as Pre
import           Control.Monad.Trans.Except  (ExceptT)
import           Control.Monad.Trans.State   (StateT, get, gets, modify, put)
import           Data.List                   (intercalate)
import qualified Data.Map                    as M
import           Data.Maybe                  (fromJust, fromMaybe, isNothing)
import           Debug.Trace                 (trace)
import           Syntax.AbsLattepp           (Ident, Program, Type,
                                              Type' (ObjectType))

type QuadruplesState = StateT QuadrupleS (ExceptT String IO)
data QuadrupleS = QuadrupleS {
    preprocessing :: Pre.PreprocessS,
    qprogram      :: QProgram,
    maxLocals     :: Int,
    currentFun    :: Maybe String,
    nextReg       :: Register,
    nextLabel     :: QLabel,
    localStore    :: LocalStore
}

data LocalStore = LocalStore {
    freeMem  :: [QIndex],
    varToMem :: M.Map Ident QIndex
}

initLocalStore :: Int -> LocalStore
initLocalStore size = LocalStore {
    freeMem = map QIndex [0..size],
    varToMem = M.empty
}

store :: Ident -> QuadruplesState QIndex
store i = do
    s <- gets localStore
    let (newS, mem) = _store s i
    modify (`setStore` newS)
    return mem

free :: Ident -> QuadruplesState ()
free i = do
    s <- gets localStore
    modify (`setStore` _free s i)

_store :: LocalStore -> Ident -> (LocalStore, QIndex)
_store s i =
    case freeMem s of
        [] -> undefined
        (x:xs) ->
            (LocalStore {
            freeMem = xs,
            varToMem = M.insert i x (varToMem s)
    }, x)

_free :: LocalStore -> Ident -> LocalStore
_free s i =
    let v = fromMaybe undefined (M.lookup i (varToMem s)) in
    LocalStore {
    freeMem = v : freeMem s,
    varToMem = M.delete i (varToMem s)
}

initQuadruplesS :: QuadrupleS
initQuadruplesS = QuadrupleS {
    preprocessing = Pre.initPreprocessS,
    qprogram = QProgram {
        classes = M.empty,
        funcs = M.empty,
        strings = M.empty
    },
    maxLocals = 0,
    currentFun = Nothing,
    nextReg = Register 0,
    nextLabel = QLabel 0,
    localStore = LocalStore {freeMem = [], varToMem = M.empty}
}

setStore :: QuadrupleS -> LocalStore -> QuadrupleS
setStore s ls = QuadrupleS {
    preprocessing = preprocessing s,
    qprogram = qprogram s,
    maxLocals = maxLocals s,
    currentFun = currentFun s,
    nextReg = nextReg s,
    nextLabel = nextLabel s,
    localStore = ls
}

setPreprocessing :: QuadrupleS -> Pre.PreprocessS -> QuadrupleS
setPreprocessing s prep = QuadrupleS {
    preprocessing = prep,
    qprogram = qprogram s,
    maxLocals = maxLocals s,
    currentFun = currentFun s,
    nextReg = nextReg s,
    nextLabel = nextLabel s,
    localStore = localStore s
}

setMaxLocals :: QuadrupleS -> Int -> QuadrupleS
setMaxLocals s l = QuadrupleS {
    preprocessing = preprocessing s,
    qprogram = qprogram s,
    maxLocals = max (maxLocals s) l,
    currentFun = currentFun s,
    nextReg = nextReg s,
    nextLabel = nextLabel s,
    localStore = localStore s
}

resetMaxLocals :: QuadrupleS -> QuadrupleS
resetMaxLocals s = QuadrupleS {
    preprocessing = preprocessing s,
    qprogram = qprogram s,
    maxLocals = 0,
    currentFun = currentFun s,
    nextReg = nextReg s,
    nextLabel = nextLabel s,
    localStore = localStore s
}

addFun :: QuadrupleS -> QFun -> QuadrupleS
addFun s f = QuadrupleS {
    preprocessing = preprocessing s,
    qprogram = _addFun (qprogram s) f,
    maxLocals = maxLocals s,
    currentFun = Just $ fident f,
    nextReg = nextReg s,
    nextLabel = nextLabel s,
    localStore = localStore s
}

_addFun :: QProgram -> QFun -> QProgram
_addFun p f = QProgram {
    classes = classes p,
    funcs = M.insert (fident f) f (funcs p),
    strings = strings p
}

addClass :: QuadrupleS -> QClass -> QuadrupleS
addClass s c = QuadrupleS {
    preprocessing = preprocessing s,
    qprogram = _addClass (qprogram s) c,
    maxLocals = maxLocals s,
    currentFun = currentFun s,
    nextReg = nextReg s,
    nextLabel = nextLabel s,
    localStore = localStore s
}

_addClass :: QProgram -> QClass -> QProgram
_addClass p c = QProgram {
    classes = M.insert (cident c) c (classes p),
    funcs = funcs p,
    strings = strings p
}

addQuad :: Quadruple -> QuadruplesState ()
addQuad q = modify (`_addQuad` q)

_addQuad :: QuadrupleS -> Quadruple -> QuadrupleS
_addQuad s q = QuadrupleS {
    preprocessing = preprocessing s,
    qprogram = __addQuad (qprogram s) (currentFun s) q,
    maxLocals = maxLocals s,
    currentFun = currentFun s,
    nextReg = nextReg s,
    nextLabel = nextLabel s,
    localStore = localStore s
}

__addQuad :: QProgram -> Maybe String -> Quadruple -> QProgram
__addQuad p mf q =
    let f = fromMaybe undefined mf in
    let lkpf = fromMaybe undefined $ M.lookup f (funcs p) in
    QProgram {
    classes = classes p,
    funcs = M.insert f (QFun {
        fident     = fident lkpf,
        localcount = localcount lkpf,
        body       = body lkpf ++ [q]}) (funcs p),
    strings = strings p
}

getLabel :: QuadruplesState QLabel
getLabel = do
    st <- get
    let label = nextLabel st
    put $ QuadrupleS {
        preprocessing = preprocessing st,
        qprogram = qprogram st,
        maxLocals = maxLocals st,
        currentFun = currentFun st,
        nextReg = nextReg st,
        nextLabel = _incLabel (nextLabel st),
        localStore = localStore st
    }
    return label

getRegister :: QuadruplesState Register
getRegister = do
    st <- get
    let reg = nextReg st
    put $ QuadrupleS {
        preprocessing = preprocessing st,
        qprogram = qprogram st,
        maxLocals = maxLocals st,
        currentFun = currentFun st,
        nextReg = _incReg (nextReg st),
        nextLabel = nextLabel st,
        localStore = localStore st
    }
    return reg


_incLabel :: QLabel -> QLabel
_incLabel (QLabel i) = QLabel (i + 1)

_incReg :: Register -> Register
_incReg (Register i) = Register (i + 1)


storeEnv :: QuadrupleS -> QuadruplesState a -> QuadruplesState a
storeEnv changedEnv action = do
  backup <- gets localStore
  result <- action
  modify (`setStore` backup)
  return result


newtype Register = Register Int

data QProgram = QProgram {
    classes :: M.Map String QClass,
    funcs   :: M.Map String QFun,
    strings :: M.Map String String
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
    Inc QIndex Register |
    Dec QIndex Register |
    Ret Register |
    Vret |
    Label QLabel |
    FLabel String |
    Load QIndex Register |
    LoadArg QIndex |
    LoadIndir Register Offset Register Offset Register |
    LoadLbl QLabel Register |
    Store Register QIndex |
    StoreIndir Register Offset Register Offset Register |
    Alloc QIndex |
    Call QLabel [Register] Register |
    VCall QLabel [Register] Register Offset Register |
    Vtab Register Type



instance Show QLabel where
    show (QLabel i) = "L" ++ show i ++ ":"

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
    show q = concat $ map show (M.toList $ strings q) ++ map (show . snd) (M.toList $ classes q) ++ map (show . snd) (M.toList $ funcs q)

instance Show QClass where
    show c = "class " ++ cident c ++ if isNothing (super c) then "" else ("$" ++ cident (fromMaybe c (super c))) ++ "\n" ++ concat (map show (fields c) ++ map show (methods c)) ++ "endclass"

instance Show QCField where
    show field = fieldName field ++ "^" ++ show (fieldSize field) ++ "\n"

instance Show QFun where
    show f = fident f ++ ":\n" ++ concatMap show (body f)

instance Show Quadruple where
    show (Add r1 r2 result) = show result ++ "=" ++ show r1 ++ "+" ++ show r2 ++ "\n"
    show (Sub r1 r2 result) = show result ++ "=" ++ show r1 ++ "-" ++ show r2 ++ "\n"
    show (Div r1 r2 result) = show result ++ "=" ++ show r1 ++ "/" ++ show r2 ++ "\n"
    show (Mul r1 r2 result) = show result ++ "=" ++ show r1 ++ "*" ++ show r2 ++ "\n"
    show (Mod r1 r2 result) = show result ++ "=" ++ show r1 ++ "%" ++ show r2 ++ "\n"
    show (Cmp r1 r2) = "cmp " ++ show r1 ++ ", " ++ show r2 ++ "\n"
    show (Jmp label) = "jmp " ++ show label ++ "\n"
    show (Je l1 l2) = "je " ++ show l1 ++ " else " ++ show l2 ++ "\n"
    show (Jge l1 l2) = "jge " ++ show l1 ++ " else " ++ show l2 ++ "\n"
    show (Jg l1 l2) = "jg " ++ show l1 ++ " else " ++ show l2 ++ "\n"
    show (Jle l1 l2) = "jle " ++ show l1 ++ " else " ++ show l2 ++ "\n"
    show (Jl l1 l2) = "jl " ++ show l1 ++ " else " ++ show l2 ++ "\n"
    show (Neg r1 result) = show result ++ "=" ++ " -" ++ show r1 ++ "\n"
    show (Not r1 result) = show result ++ "=" ++ " !" ++ show r1 ++ "\n"
    show (Concat r1 r2 result) = show result ++ "=concat(" ++ show r1 ++ ", " ++ show r2 ++ ")" ++ "\n"
    show (MovV val r1) = "mov " ++ show r1 ++ ", " ++ show val ++ "\n"
    show (Mov r1 r2) = "mov " ++ show r2 ++ ", " ++ show r1 ++ "\n"
    show (Inc i1 r1) = "inc " ++ show i1 ++ ", " ++ show r1 ++ "\n"
    show (Dec i1 r1) = "dec " ++ show i1 ++ ", " ++ show r1 ++ "\n"
    show (Ret r1) = "ret " ++ show r1 ++ "\n"
    show Vret = "ret" ++ "\n"
    show (Label label) = show label ++ "\n"
    show (FLabel label) = "F" ++ show label ++ "\n"
    show (Load index r1) = "load " ++ show r1 ++ ", " ++ show index ++ "\n"
    show (LoadArg i1) = "arg " ++ show i1 ++ "\n"
    show (LoadIndir addr offset1 offsetreg offset2 result) = show result ++ "= ptr [" ++ show addr ++ "+" ++ show offset1 ++ "+" ++ show offsetreg ++ "*" ++ show offset2 ++ "]" ++ "\n"
    show (LoadLbl label r1) = show r1 ++ "= ptr " ++ show label ++ "\n"
    show (Store r1 index) = "store " ++ show index ++ ", " ++ show r1 ++ "\n"
    show (StoreIndir addr offset1 offsetreg offset2 value) = "store " ++ "[" ++ show addr ++ "+" ++ show offset1 ++ "+" ++ show offsetreg ++ "*" ++ show offset2 ++ "], " ++ show value ++ "\n"
    show (Alloc index) = "alloc " ++ show index ++ "\n"
    show (Call label args result) = show result ++ "=call " ++ show label ++ "(" ++ intercalate "," (map show args) ++ ")" ++ "\n"
    show (VCall label args result _ _) = show result ++ "=vcall " ++ show label ++ "(" ++ intercalate "," (map show args)++ ")" ++ "\n"
    show (Vtab r1 type1) = "vt " ++ show type1 ++ ", " ++ show r1 ++ "\n"


