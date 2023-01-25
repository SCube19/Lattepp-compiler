module Compiler.Quadruples.Data where
import           Compiler.Quadruples.Predata (PreprocessS)
import qualified Compiler.Quadruples.Predata as Pre
import qualified Control.Applicative
import           Control.Monad.Trans.Except  (ExceptT)
import           Control.Monad.Trans.State   (StateT, get, gets, modify, put)
import           Data.Function               (on)
import           Data.List                   (intercalate, sortBy)
import qualified Data.Map                    as M
import           Data.Maybe                  (fromJust, fromMaybe, isNothing)
import           Debug.Trace                 (trace)
import           Syntax.AbsLattepp           (Block' (Block), Ident (Ident),
                                              PrimType' (Bool, Int, Str, Void),
                                              Program, TopDef, TopDef' (FnDef),
                                              Type,
                                              Type' (ObjectType, Primitive))
import           Utils                       (Raw (raw), rawStr, rawVoid)

type QuadruplesState = StateT QuadrupleS (ExceptT String IO)
data QuadrupleS = QuadrupleS {
    qprogram   :: QProgram,
    maxLocals  :: Int,
    currentFun :: Maybe String,
    nextReg    :: Int,
    nextLabel  :: QLabel,
    localStore :: LocalStore
}

data LocalStore = LocalStore {
    freeMem  :: [QIndex],
    varToMem :: M.Map Ident QIndex
}

initLocalStore :: Int -> LocalStore
initLocalStore size = LocalStore {
    freeMem = map (`QIndex` rawVoid) [0..size - 1],
    varToMem = M.empty
}

store :: Ident -> Type -> QuadruplesState QIndex
store i t = do
    s <- gets localStore
    let (newS, mem) = _store s i t
    setStore newS
    return mem

free :: Ident -> QuadruplesState ()
free i = do
    s <- gets localStore
    setStore (_free s i)

_store :: LocalStore -> Ident -> Type -> (LocalStore, QIndex)
_store s i t =
    case freeMem s of
        [] -> undefined
        (x@(QIndex ind str):xs) ->
            (LocalStore {
            freeMem = xs,
            varToMem = M.insert i (QIndex ind (raw t))(varToMem s)
    }, x)

_free :: LocalStore -> Ident -> LocalStore
_free s i =
    let v = fromMaybe undefined (M.lookup i (varToMem s)) in
    LocalStore {
    freeMem = v : freeMem s,
    varToMem = M.delete i (varToMem s)
}

predefinedFuncs :: M.Map String Type
predefinedFuncs = M.fromList [
    ("printInt", Primitive Nothing (Void Nothing) ),
    ("printString",  Primitive Nothing (Void Nothing) ),
    ("readInt", Primitive Nothing (Int Nothing) ),
    ("readString", Primitive Nothing (Str Nothing) ),
    ("error", Primitive Nothing (Void Nothing) ),
    ("__concat", Primitive Nothing (Str Nothing) ),
    ("__equals", Primitive Nothing (Bool Nothing) ),
    ("__notequals", Primitive Nothing (Bool Nothing))]

initQuadruplesS :: [TopDef] -> QuadrupleS
initQuadruplesS defs = QuadrupleS {
    qprogram = QProgram {
        classes = M.empty,
        funcs = M.fromList $ gatherFuncs defs [],
        strings = M.empty
    },
    maxLocals = 0,
    currentFun = Nothing,
    nextReg = 0,
    nextLabel = QLabel 0,
    localStore = LocalStore {freeMem = [], varToMem = M.empty}
}

gatherFuncs :: [TopDef] -> [(String, QFun)] -> [(String, QFun)]
gatherFuncs [] fs = fs
gatherFuncs (def:defs) fs = case def of
  FnDef _ t (Ident id) _ _ -> gatherFuncs defs ((id, QFun {fident = id, ret = t, localcount = 0, body = [], offset = 0}):fs)
  _ -> gatherFuncs defs fs

setStore ::  LocalStore -> QuadruplesState ()
setStore ls = modify (\s -> QuadrupleS {
    qprogram = qprogram s,
    maxLocals = maxLocals s,
    currentFun = currentFun s,
    nextReg = nextReg s,
    nextLabel = nextLabel s,
    localStore = ls
})

setMaxLocals :: QuadrupleS -> Int -> QuadrupleS
setMaxLocals s l = QuadrupleS {
    qprogram = qprogram s,
    maxLocals = max (maxLocals s) l,
    currentFun = currentFun s,
    nextReg = nextReg s,
    nextLabel = nextLabel s,
    localStore = localStore s
}

resetMaxLocals :: QuadrupleS -> QuadrupleS
resetMaxLocals s = QuadrupleS {
    qprogram = qprogram s,
    maxLocals = 0,
    currentFun = currentFun s,
    nextReg = nextReg s,
    nextLabel = nextLabel s,
    localStore = localStore s
}

addFun :: QFun -> QuadruplesState ()
addFun f = modify (\s -> QuadrupleS {
    qprogram = _addFun (qprogram s) f,
    maxLocals = maxLocals s,
    currentFun = Just $ fident f,
    nextReg = nextReg s,
    nextLabel = nextLabel s,
    localStore = localStore s
})

_addFun :: QProgram -> QFun -> QProgram
_addFun p f = QProgram {
    classes = classes p,
    funcs = M.insert (fident f) f (funcs p),
    strings = strings p
}

addClass :: QClass -> QuadruplesState ()
addClass c = modify (\s -> QuadrupleS {
    qprogram = _addClass (qprogram s) c,
    maxLocals = maxLocals s,
    currentFun = currentFun s,
    nextReg = nextReg s,
    nextLabel = nextLabel s,
    localStore = localStore s
})

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
        ret        = ret lkpf,
        localcount = localcount lkpf,
        body       = body lkpf ++ [q],
        offset     = offset lkpf}) (funcs p),
    strings = strings p
}

getLabel :: QuadruplesState QLabel
getLabel = do
    st <- get
    let label = nextLabel st
    put $ QuadrupleS {
        qprogram = qprogram st,
        maxLocals = maxLocals st,
        currentFun = currentFun st,
        nextReg = nextReg st,
        nextLabel = _incLabel (nextLabel st),
        localStore = localStore st
    }
    return label

getRegister :: Type -> QuadruplesState Register
getRegister t = do
    st <- get
    let reg = nextReg st
    put $ QuadrupleS {
        qprogram = qprogram st,
        maxLocals = maxLocals st,
        currentFun = currentFun st,
        nextReg = nextReg st + 1,
        nextLabel = nextLabel st,
        localStore = localStore st
    }
    return $ Register reg t


_incLabel :: QLabel -> QLabel
_incLabel (QLabel i) = QLabel (i + 1)

addString :: String -> QuadruplesState QLabel
addString s = do
    p <- gets qprogram
    case M.lookup s (strings p) of
      Nothing -> do label <- getLabel
                    modify (`_addString` (s, label))
                    return label
      Just mem -> return mem


_addString :: QuadrupleS -> (String, QLabel) -> QuadrupleS
_addString s x = QuadrupleS {
    qprogram = __addString (qprogram s) x,
    maxLocals = maxLocals s,
    currentFun = currentFun s,
    nextReg = nextReg s,
    nextLabel = nextLabel s,
    localStore = localStore s
}

__addString :: QProgram -> (String, QLabel) -> QProgram
__addString p (s, l) = QProgram {
    classes = classes p,
    funcs = funcs p,
    strings = M.insert s l (strings p)
}

storeEnv :: QuadrupleS -> QuadruplesState a -> QuadruplesState a
storeEnv changedEnv action = do
  backup <- gets localStore
  result <- action
  setStore backup
  return result


data Register = Register Int Type

instance Eq Register where
    (==) (Register a _) (Register b _) = a == b

instance Ord Register where
    compare (Register a _) (Register b _) = compare a b

regType :: Register -> Type
regType (Register _ t) = t


data QProgram = QProgram {
    classes :: M.Map String QClass,
    funcs   :: M.Map String QFun,
    strings :: M.Map String QLabel
}

data QFun = QFun {
    fident     :: String,
    ret        :: Type,
    localcount :: Int,
    body       :: [Quadruple],
    offset     :: Int
}

data QClass = QClass {
    cident  :: String,
    fields  :: M.Map String QCField,
    methods :: M.Map String QFun,
    super   :: Maybe QClass,
    vtable  :: Maybe QLabel
}

data QCField = QCField {
    fieldName   :: String,
    fieldOffset :: Int,
    fieldSize   :: Int
}

preFieldsToQFields :: M.Map String (Type, Int) -> M.Map String QCField
preFieldsToQFields fs = M.fromList $ map (\(f, (t, o)) -> (f, QCField {fieldName = f, fieldOffset = o, fieldSize = 4})) (M.toList fs)

preFuncsToQFun :: M.Map String ((Type, [Type]), Int) -> String -> M.Map String QFun
preFuncsToQFun funcs cName = M.fromList $ map (\(f, ((rType, args), o)) -> (f, QFun {
    fident     = cName ++ "__" ++ f,
    ret        = rType,
    localcount = length args,
    body       = [],
    offset     = o})) (M.toList funcs)

classDefPreToQClass :: Pre.ClassDefPre -> M.Map String QClass -> QClass
classDefPreToQClass c cs =
    let superClass = Pre.super c in
    QClass {
    cident = Pre.ident c,
    fields = preFieldsToQFields (Pre.attrs c),
    methods = preFuncsToQFun (Pre.methods c) (Pre.ident c),
    super = case superClass of
                Nothing -> Nothing
                Just s -> M.lookup (Pre.ident s) cs Control.Applicative.<|> undefined,
    vtable = Nothing
}

getOrd :: (String, (Int, Pre.ClassDefPre)) -> Int
getOrd (_, (ord, _)) = ord

convertPreprocessing :: PreprocessS -> QuadruplesState ()
convertPreprocessing ps = mapM_ (_convertPreprocessing . (\(_, (_, x)) -> x)) (sortBy (compare `on` getOrd) (M.toList $ Pre.preclasses ps))

_convertPreprocessing :: Pre.ClassDefPre -> QuadruplesState ()
_convertPreprocessing def = do
    q <- gets qprogram
    modify  (\s ->
        let qclass = classDefPreToQClass def (classes q) in
        QuadrupleS {
        qprogram   = QProgram {
            classes = M.insert (cident qclass) qclass (classes q),
            funcs   = funcs q,
            strings = strings q
        },
        maxLocals  = maxLocals s,
        currentFun = currentFun s,
        nextReg    = nextReg s,
        nextLabel  = nextLabel s,
        localStore = localStore s
        })

newtype QLabel = QLabel Int deriving (Eq, Ord)

data QIndex = QIndex Int Type

type Offset = Int

data QValue = QBool Bool | QInt Int

qinteger :: Integer -> QValue
qinteger i = QInt $ fromIntegral i

qvalueInt :: QValue -> Int
qvalueInt (QBool b) = if b then 2^32 - 1 else 0
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
    Je QLabel |
    Jne QLabel |
    Jge QLabel |
    Jg QLabel |
    Jle QLabel |
    Jl QLabel |
    PhiJe  QLabel QLabel Register |
    PhiJne QLabel QLabel Register |
    PhiJge QLabel QLabel Register |
    PhiJg  QLabel QLabel Register |
    PhiJle QLabel QLabel Register |
    PhiJl  QLabel QLabel Register |
    Neg Register Register |
    Not Register Register |
    MovV QValue Register |
    Mov Register Register |
    Inc QIndex |
    Dec QIndex |
    Ret Register |
    Vret |
    Label QLabel |
    Load QIndex Register |
    LoadArg QIndex |
    LoadIndir Register Offset Register Offset Register |
    LoadLbl QLabel Register |
    Store Register QIndex |
    StoreIndir Register Offset Register Offset Register |
    Alloc QIndex |
    Call String [Register] Register |
    VoidCall String [Register] |
    VCall String [Register] Register Offset Register |
    VoidVCall String [Register] Register Offset |
    Vtab Register Type


extractResult :: Quadruple -> Maybe Register
extractResult (Add _ _ r)            = Just r
extractResult (Sub _ _ r)            = Just r
extractResult (Div _ _ r)            = Just r
extractResult (Mul _ _ r)            = Just r
extractResult (Mod _ _ r)            = Just r
extractResult (Cmp _ _)              = Nothing
extractResult (Jmp _)                = Nothing
extractResult (Je _)                 = Nothing
extractResult (Jne _)                = Nothing
extractResult (Jge _)                = Nothing
extractResult (Jg _)                 = Nothing
extractResult (Jle _)                = Nothing
extractResult (Jl _)                 = Nothing
extractResult (PhiJe  _ _ r)         = Just r
extractResult (PhiJne _ _ r)         = Just r
extractResult (PhiJge _ _ r)         = Just r
extractResult (PhiJg  _ _ r)         = Just r
extractResult (PhiJle _ _ r)         = Just r
extractResult (PhiJl  _ _ r)         = Just r
extractResult (Neg _ r)              = Just r
extractResult (Not _ r)              = Just r
extractResult (MovV _ r)             = Just r
extractResult (Mov _ r)              = Just r
extractResult (Inc _)                = Nothing
extractResult (Dec _)                = Nothing
extractResult (Ret _)                = Nothing
extractResult Vret                   = Nothing
extractResult (Label _)              = Nothing
extractResult (Load _ r)             = Just r
extractResult (LoadArg _)            = Nothing
extractResult (LoadIndir _ _ _ _ r)  = Just r
extractResult (LoadLbl _ r)          = Just r
extractResult (Store _ _)            = Nothing
extractResult (StoreIndir _ _ _ _ r) = Just r
extractResult (Alloc _)              = Nothing
extractResult (Call _ _ r)           = Just r
extractResult (VoidCall _ _)         = Nothing
extractResult (VCall _ _ _ _ r)      = Just r
extractResult VoidVCall {}           = Nothing
extractResult (Vtab _ _)             = Nothing

extractAll :: Quadruple -> [Register]
extractAll (Add r1 r2 r3)            = [r1, r2, r3]
extractAll (Sub r1 r2 r3)            = [r1, r2, r3]
extractAll (Div r1 r2 r3)            = [r1, r2, r3]
extractAll (Mul r1 r2 r3)            = [r1, r2, r3]
extractAll (Mod r1 r2 r3)            = [r1, r2, r3]
extractAll (Cmp r1 r2)               = [r1, r2]
extractAll (Jmp _)                   = []
extractAll (Je _)                    = []
extractAll (Jne _)                   = []
extractAll (Jge _)                   = []
extractAll (Jg _)                    = []
extractAll (Jle _)                   = []
extractAll (Jl _)                    = []
extractAll (PhiJe  _ _ r1)           = [r1]
extractAll (PhiJne _ _ r1)           = [r1]
extractAll (PhiJge _ _ r1)           = [r1]
extractAll (PhiJg  _ _ r1)           = [r1]
extractAll (PhiJle _ _ r1)           = [r1]
extractAll (PhiJl  _ _ r1)           = [r1]
extractAll (Neg r1 r2)               = [r1, r2]
extractAll (Not r1 r2)               = [r1, r2]
extractAll (MovV _ r1)               = [r1]
extractAll (Mov r1 r2)               = [r1, r2]
extractAll (Inc _ )                  = []
extractAll (Dec _ )                  = []
extractAll (Ret r1)                  = [r1]
extractAll Vret                      = []
extractAll (Label _)                 = []
extractAll (Load _ r1)               = [r1]
extractAll (LoadArg _)               = []
extractAll (LoadIndir r1 _ r2 _ r3)  = [r1, r2, r3]
extractAll (LoadLbl _ r1)            = [r1]
extractAll (Store r1 _)              = [r1]
extractAll (StoreIndir r1 _ r2 _ r3) = [r1, r2, r3]
extractAll (Alloc _)                 = []
extractAll (Call _ rs r1)            = r1 : rs
extractAll (VoidCall _ rs)           = rs
extractAll (VCall _ rs r1 _ r2)      = r1 : r2 : rs
extractAll (VoidVCall _ rs r1 _)     = r1 :rs
extractAll (Vtab r1 _)               = [r1]

instance Show QLabel where
    show (QLabel i) = "L" ++ show i ++ ":"

instance Show QIndex where
    show (QIndex i _) = "i" ++ show i

instance Show QValue where
    show x@(QBool b) = show $ qvalueInt x
    show (QInt i)    = show i

instance Show Register where
    show (Register r _) = "r" ++ show r

instance Show QProgram where
    show q = concat $ map showConstString (M.toList $ strings q) ++ map (show . snd) (M.toList $ classes q) ++ map (show . snd) (M.toList $ funcs q)

showConstString :: (String, QLabel) -> String
showConstString (s, l) = show l ++ " " ++ show s ++ "\n"

instance Show QClass where
    show c = "class " ++ cident c ++ (if isNothing (super c) then "" else " $ " ++ maybe "" cident (super c)) ++ "\n" ++
                concat (map (show . snd) (M.toList $ fields c) ++
                map (show . snd) (M.toList $ methods c)) ++ "endclass\n"

instance Show QCField where
    show field = fieldName field ++ "^" ++ show (fieldSize field) ++ " > " ++ show (fieldOffset field) ++ "\n"

instance Show QFun where
    show f = fident f ++ ": > " ++ show (offset f) ++ " | lcount: "  ++ show (localcount f) ++ "\n" ++ concatMap show (body f)

instance Show Quadruple where
    show (Add r1 r2 result) = show result ++ " = " ++ show r1 ++ "+" ++ show r2 ++ "\n"
    show (Sub r1 r2 result) = show result ++ " = " ++ show r1 ++ "-" ++ show r2 ++ "\n"
    show (Div r1 r2 result) = show result ++ " = " ++ show r1 ++ "/" ++ show r2 ++ "\n"
    show (Mul r1 r2 result) = show result ++ " = " ++ show r1 ++ "*" ++ show r2 ++ "\n"
    show (Mod r1 r2 result) = show result ++ " = " ++ show r1 ++ "%" ++ show r2 ++ "\n"
    show (Cmp r1 r2) = "cmp " ++ show r1 ++ ", " ++ show r2 ++ "\n"
    show (Jmp label) = "jmp " ++ show label ++ "\n"
    show (Je l1) = "je " ++ show l1 ++ "\n"
    show (Jne l1) = "jne " ++ show l1 ++ "\n"
    show (Jge l1) = "jge " ++ show l1 ++ "\n"
    show (Jg l1) = "jg " ++ show l1 ++ "\n"
    show (Jle l1) = "jle " ++ show l1 ++ "\n"
    show (Jl l1) = "jl " ++ show l1 ++ "\n"
    show (PhiJe  l1 l2 r1) = show r1 ++ " = phi je " ++  show l1 ++  show l2 ++ "\n"
    show (PhiJne l1 l2 r1) = show r1 ++ " = phi jne " ++ show l1 ++ show l2 ++  "\n"
    show (PhiJge l1 l2 r1) = show r1 ++ " = phi jge " ++ show l1 ++ show l2 ++  "\n"
    show (PhiJg  l1 l2 r1) = show r1 ++ " = phi jg " ++  show l1 ++  show l2 ++ "\n"
    show (PhiJle l1 l2 r1) = show r1 ++ " = phi jle " ++ show l1 ++ show l2 ++  "\n"
    show (PhiJl  l1 l2 r1) = show r1 ++ " = phi jl " ++  show l1 ++  show l2 ++ "\n"
    show (Neg r1 result) = "neg " ++ show result ++ " " ++ show r1 ++ "\n"
    show (Not r1 result) = "not " ++ show result ++ " " ++ show r1 ++ "\n"
    show (MovV val r1) = "mov " ++ show r1 ++ ", " ++ show val ++ "\n"
    show (Mov r1 r2) = "mov " ++ show r2 ++ ", " ++ show r1 ++ "\n"
    show (Inc i1) = "inc " ++ show i1 ++ "\n"
    show (Dec i1) = "dec " ++ show i1 ++ "\n"
    show (Ret r1) = "ret " ++ show r1 ++ "\n"
    show Vret = "ret" ++ "\n"
    show (Label label) = show label ++ "\n"
    show (Load index r1) = "load " ++ show r1 ++ ", " ++ show index ++ "\n"
    show (LoadArg i1) = "arg " ++ show i1 ++ "\n"
    show (LoadIndir addr offset1 offsetreg offset2 result) = show result ++ "= ptr [" ++ show addr ++ "+" ++ show offset1 ++ "+" ++ show offsetreg ++ "*" ++ show offset2 ++ "]" ++ "\n"
    show (LoadLbl label r1) = show r1 ++ " = ptr " ++ show label ++ "\n"
    show (Store r1 index) = "store " ++ show index ++ ", " ++ show r1 ++ "\n"
    show (StoreIndir addr offset1 offsetreg offset2 value) = "store " ++ "[" ++ show addr ++ "+" ++ show offset1 ++ "+" ++ show offsetreg ++ "*" ++ show offset2 ++ "], " ++ show value ++ "\n"
    show (Alloc index) = "alloc " ++ show index ++ "\n"
    show (Call label args result) = show result ++ " = call " ++ label ++ "(" ++ intercalate "," (map show args) ++ ")" ++ "\n"
    show (VoidCall label args) = "voidcall " ++ label ++ "(" ++ intercalate "," (map show args) ++ ")" ++ "\n"
    show (VCall label args this _ result) = show result ++ " = " ++ show this ++ ".vcall " ++ label ++ "(" ++ intercalate "," (map show args)++ ")" ++ "\n"
    show (VoidVCall label args this _ ) = show this ++ ".vcall " ++ label ++ "(" ++ intercalate "," (map show args)++ ")" ++ "\n"
    show (Vtab r1 type1) = "vt " ++ show type1 ++ ", " ++ show r1 ++ "\n"



