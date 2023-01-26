module Compiler.Compiler where
import           Compiler.Data                  as X86
import           Compiler.Quadruples.Data       as Quad
import           Compiler.Quadruples.Quadruples (quadruplize)
import           Control.Monad                  (unless, when)
import           Control.Monad.Cont             (MonadIO (liftIO),
                                                 MonadTrans (lift))
import           Control.Monad.Trans.Except     (ExceptT)
import           Control.Monad.Trans.State      (evalStateT, get, gets)
import           Data.List                      (delete)
import qualified Data.Map                       as M
import           Data.Maybe                     (fromMaybe)
import qualified Data.Set                       as S
import           Syntax.AbsLattepp              (Ident (Ident), Program,
                                                 Program' (Program),
                                                 Type' (ObjectType))
import           Typechecker.Data               (TypeCheckerS)
import           Utils                          (align16)

compile :: Program -> TypeCheckerS -> ExceptT String IO String
compile p@(Program _ defs) tcEnv =  do
    quads <- evalStateT (quadruplize p tcEnv) (initQuadruplesS defs)
    asm <- evalStateT (compileQuads quads) (initCompilerS $ (Quad.classes quads))
    return $ concatMap show asm

compileQuads :: QProgram -> CompilerState [AsmInstr]
compileQuads p = do
    addInstr $ Section "data"
    mapM_ (\(s, l) -> addInstr $ DataString (labelref l) s) (M.toList $ strings p)
    addInstr $ Section "text"
    addInstr $ Global "main"
    addInstr $ Extern "printInt"
    addInstr $ Extern "printString"
    addInstr $ Extern "readInt"
    addInstr $ Extern "readString"
    addInstr $ Extern "error"
    addInstr $ Extern "__concat"
    addInstr $ Extern "__equals"
    addInstr $ Extern "__notequals"
    mapM_ compileQuadsFun (funcs p)
    gets instructions


compileQuadsFun :: QFun -> CompilerState ()
compileQuadsFun f = do
    resetAllocation
    (mapping, size) <- lift $ evalStateT (allocMemory (body f)) (initAllocatorS (localcount f))
    setAllocation mapping (size + localcount f)
    initialFunQuad (fident f)
    mapM_ compileQuad (body f)

compileQuad :: Quadruple -> CompilerState ()
compileQuad (Quad.Add r1 r2 res)                  = do
    regs <- gets mapping
    let asmres = fromMaybe undefined (M.lookup res regs)
    let asmr1 = fromMaybe undefined (M.lookup r1 regs)
    let asmr2 = fromMaybe undefined (M.lookup r2 regs)
    addInstr $ X86.Mov QWord (RegOp RAX) asmr1
    addInstr $ X86.Add QWord (RegOp RAX) asmr2
    addInstr $ X86.Mov QWord asmres (RegOp RAX)

compileQuad (Quad.Sub r1 r2 res)                  = do
    regs <- gets mapping
    let asmres = fromMaybe undefined (M.lookup res regs)
    let asmr1 = fromMaybe undefined (M.lookup r1 regs)
    let asmr2 = fromMaybe undefined (M.lookup r2 regs)
    addInstr $ X86.Mov QWord (RegOp RAX) asmr1
    addInstr $ X86.Sub QWord (RegOp RAX) asmr2
    addInstr $ X86.Mov QWord asmres (RegOp RAX)

compileQuad (Quad.Div r1 r2 res)                  = do
    regs <- gets mapping
    let asmres = fromMaybe undefined (M.lookup res regs)
    let asmr1 = fromMaybe undefined (M.lookup r1 regs)
    let asmr2 = fromMaybe undefined (M.lookup r2 regs)
    addInstr $ X86.Mov QWord (RegOp RAX) asmr1
    addInstr Cdq
    if isAsmValue asmr2 then do
        addInstr $ X86.Mov QWord (RegOp RCX) asmr2
        addInstr $ X86.Div QWord (RegOp RCX)
    else
        addInstr $ X86.Div QWord asmr2
    addInstr $ X86.Mov QWord asmres (RegOp RAX)

compileQuad (Quad.Mul r1 r2 res)                  = do
    regs <- gets mapping
    let asmres = fromMaybe undefined (M.lookup res regs)
    let asmr1 = fromMaybe undefined (M.lookup r1 regs)
    let asmr2 = fromMaybe undefined (M.lookup r2 regs)
    addInstr $ X86.Mov QWord (RegOp RAX) asmr1
    addInstr $ X86.Mul QWord (RegOp RAX) asmr2
    addInstr $ X86.Mov QWord asmres (RegOp RAX)

compileQuad (Quad.Mod r1 r2 res)                  = do
    regs <- gets mapping
    let asmres = fromMaybe undefined (M.lookup res regs)
    let asmr1 = fromMaybe undefined (M.lookup r1 regs)
    let asmr2 = fromMaybe undefined (M.lookup r2 regs)
    addInstr $ X86.Mov QWord (RegOp RAX) asmr1
    addInstr Cdq
    if isAsmValue asmr2 then do
        addInstr $ X86.Mov QWord (RegOp RCX) asmr2
        addInstr $ X86.Div QWord (RegOp RCX)
    else
        addInstr $ X86.Div QWord asmr2
    addInstr $ X86.Mov QWord asmres (RegOp RDX)

compileQuad (Quad.Cmp r1 r2)                      = do
    regs <- gets mapping
    let asmr1 = fromMaybe undefined (M.lookup r1 regs)
    let asmr2 = fromMaybe undefined (M.lookup r2 regs)
    addInstr $ X86.Mov QWord (RegOp RAX) asmr2
    if isAsmValue asmr1 then do
        addInstr $ X86.Mov QWord (RegOp RCX) asmr1
        addInstr $ X86.Cmp QWord (RegOp RCX) (RegOp RAX)
    else
        addInstr $ X86.Cmp QWord asmr1 (RegOp RAX)

compileQuad (Quad.Jmp l1)                         =
    addInstr $ X86.Jmp (AsmLabel $ labelref l1)

compileQuad (Quad.Je l1)                          =
    addInstr $ X86.Je (AsmLabel $ labelref l1)

compileQuad (Quad.Jne l1)                         =
    addInstr $ X86.Jne (AsmLabel $ labelref l1)

compileQuad (Quad.Jge l1)                         =
    addInstr $ X86.Jge (AsmLabel $ labelref l1)

compileQuad (Quad.Jg l1)                          =
    addInstr $ X86.Jg (AsmLabel $ labelref l1)

compileQuad (Quad.Jle l1)                         =
    addInstr $ X86.Jle (AsmLabel $ labelref l1)

compileQuad (Quad.Jl l1)                          =
    addInstr $ X86.Jl (AsmLabel $ labelref l1)

compileQuad (Quad.PhiJe l1 l2 res)                   = do
    regs <- gets mapping
    let asmres = fromMaybe undefined (M.lookup res regs)
    addInstr $ X86.Je (AsmLabel $ labelref l1)
    compilePhi asmres (qtoasmlbl l1) (qtoasmlbl l2)

compileQuad (Quad.PhiJne l1 l2 res)                  = do
    regs <- gets mapping
    let asmres = fromMaybe undefined (M.lookup res regs)
    addInstr $ X86.Jne (AsmLabel $ labelref l1)
    compilePhi asmres (qtoasmlbl l1) (qtoasmlbl l2)

compileQuad (Quad.PhiJge l1 l2 res)                  = do
    regs <- gets mapping
    let asmres = fromMaybe undefined (M.lookup res regs)
    addInstr $ X86.Jge (AsmLabel $ labelref l1)
    compilePhi asmres (qtoasmlbl l1) (qtoasmlbl l2)

compileQuad (Quad.PhiJg l1 l2 res)                   = do
    regs <- gets mapping
    let asmres = fromMaybe undefined (M.lookup res regs)
    addInstr $ X86.Jg (AsmLabel $ labelref l1)
    compilePhi asmres (qtoasmlbl l1) (qtoasmlbl l2)

compileQuad (Quad.PhiJle l1 l2 res)                  = do
    regs <- gets mapping
    let asmres = fromMaybe undefined (M.lookup res regs)
    addInstr $ X86.Jle (AsmLabel $ labelref l1)
    compilePhi asmres (qtoasmlbl l1) (qtoasmlbl l2)

compileQuad (Quad.PhiJl l1 l2 res)                   = do
    regs <- gets mapping
    let asmres = fromMaybe undefined (M.lookup res regs)
    addInstr $ X86.Jl (AsmLabel $ labelref l1)
    compilePhi asmres (qtoasmlbl l1) (qtoasmlbl l2)

compileQuad (Quad.Neg r1 res)                     = do
    regs <- gets mapping
    let asmres = fromMaybe undefined (M.lookup res regs)
    let asmr1 = fromMaybe undefined (M.lookup r1 regs)
    when (r1 /= res)
        (   do
            addInstr $ X86.Mov QWord (RegOp RAX) asmr1
            addInstr $ X86.Mov QWord asmres (RegOp RAX))
    addInstr $ X86.Neg QWord asmres

compileQuad (Quad.Not r1 res)                      = do
    regs <- gets mapping
    let asmres = fromMaybe undefined (M.lookup res regs)
    let asmr1 = fromMaybe undefined (M.lookup r1 regs)
    when (r1 /= res)
        (   do
            addInstr $ X86.Mov QWord (RegOp RAX) asmr1
            addInstr $ X86.Mov QWord asmres (RegOp RAX))
    addInstr $ X86.Not QWord asmres

compileQuad (Quad.MovV v r1)                      = do
    regs <- gets mapping
    let asmr1 = fromMaybe undefined (M.lookup r1 regs)
    unless (isAsmValue asmr1) $
        addInstr $ X86.Mov QWord asmr1 (ValOp (VInt $ qvalueInt v))

compileQuad (Quad.Mov r1 r2)                      = do
    regs <- gets mapping
    let asmr1 = fromMaybe undefined (M.lookup r1 regs)
    let asmr2 = fromMaybe undefined (M.lookup r2 regs)
    addInstr $ X86.Mov QWord asmr1 asmr2

compileQuad (Quad.Inc (QIndex i _))                   = do
    regs <- gets mapping
    addInstr $ X86.Inc QWord (MemOp (RegOff RBP (-8 * (i + 1))))

compileQuad (Quad.Dec (QIndex i _))                   = do
    regs <- gets mapping
    addInstr $ X86.Dec QWord (MemOp (RegOff RBP (-8 * (i + 1))))

compileQuad (Quad.Ret r1)                         = do
    regs <- gets mapping
    let asmr1 = fromMaybe undefined (M.lookup r1 regs)
    addInstr $ X86.Mov QWord (RegOp RAX) asmr1
    retPop
    addInstr X86.Ret

compileQuad Quad.Vret                             = do
    retPop
    addInstr X86.Ret

compileQuad (Quad.Label l1)                       =
    addInstr $ X86.ILabel $ qtoasmlbl l1

compileQuad (Quad.Load (QIndex i _) res)                 = do
    regs <- gets mapping
    let asmres = fromMaybe undefined (M.lookup res regs)
    addInstr $ X86.Mov QWord (RegOp RAX) (MemOp $ RegOff RBP (-8 * (i + 1)))
    addInstr $ X86.Mov QWord asmres (RegOp RAX)

compileQuad (Quad.LoadArg (QIndex i _))                  = do
    case i of
        0 -> addInstr $ X86.Mov QWord (MemOp $ RegOff RBP (-8 * (i + 1))) (RegOp RDI)
        1 -> addInstr $ X86.Mov QWord (MemOp $ RegOff RBP (-8 * (i + 1))) (RegOp RSI)
        2 -> addInstr $ X86.Mov QWord (MemOp $ RegOff RBP (-8 * (i + 1))) (RegOp RDX)
        3 -> addInstr $ X86.Mov QWord (MemOp $ RegOff RBP (-8 * (i + 1))) (RegOp RCX)
        4 -> addInstr $ X86.Mov QWord (MemOp $ RegOff RBP (-8 * (i + 1))) (RegOp R8)
        5 -> addInstr $ X86.Mov QWord (MemOp $ RegOff RBP (-8 * (i + 1))) (RegOp R9)
        _ -> do
             addInstr $ X86.Mov QWord (RegOp RAX) (MemOp $ RegOff RBP (8 * (i - 4)))
             addInstr $ X86.Mov QWord (MemOp $ RegOff RBP (-8 * (i + 1))) (RegOp RAX)

compileQuad (Quad.LoadIndir r1 off1 r2 off2 res)  = do
    regs <- gets mapping
    let asmres = fromMaybe undefined (M.lookup res regs)
    let asmr1 = fromMaybe undefined (M.lookup r1 regs)
    let asmr2 = fromMaybe undefined (M.lookup r2 regs)
    addInstr $ X86.Mov QWord (RegOp RAX) asmr1
    case asmr2 of
      ValOp (VInt 0) ->  addInstr $ X86.Mov QWord (RegOp RAX) (MemOp $ RegOff RAX off1)
      _ -> do
        addInstr $ X86.Mov QWord (RegOp RCX) asmr2
        addInstr $ X86.Mov QWord (RegOp RAX) (MemOp $ IndirMem RAX off1 RCX off2)
    addInstr $ X86.Mov QWord asmres (RegOp RAX)

compileQuad (Quad.LoadLbl l1 res)                 = do
    regs <- gets mapping
    let asmres = fromMaybe undefined (M.lookup res regs)
    addInstr $ X86.Mov QWord asmres (ValOp $ VLabel $ AsmLabel $ labelref l1)

compileQuad (Quad.Store r1 (QIndex i _))              = do
    regs <- gets mapping
    let asmr1 = fromMaybe undefined (M.lookup r1 regs)
    addInstr $ X86.Mov QWord (RegOp RAX) asmr1
    addInstr $ X86.Mov QWord (MemOp $ RegOff RBP (-8 * (i + 1))) (RegOp RAX)

compileQuad (Quad.StoreIndir r1 off1 r2 off2 val) = do
    regs <- gets mapping
    let asmval = fromMaybe undefined (M.lookup val regs)
    let asmr1 = fromMaybe undefined (M.lookup r1 regs)
    let asmr2 = fromMaybe undefined (M.lookup r2 regs)
    addInstr $ X86.Mov QWord (RegOp RCX) asmr1
    addInstr $ X86.Mov QWord (RegOp RAX) asmval
    case asmr2 of
      ValOp (VInt 0) -> addInstr $ X86.Mov QWord (MemOp $ RegOff RCX off1) (RegOp RAX)
      _ -> do
        addInstr $ X86.Mov QWord (RegOp RDX) asmr2
        addInstr $ X86.Mov QWord (MemOp $ IndirMem RCX off1 RDX off2) (RegOp RAX)

compileQuad (Quad.Alloc index)                    = return ()

compileQuad (Quad.Call name args res)             = do
    regs <- gets mapping
    let asmres = fromMaybe undefined (M.lookup res regs)
    call name args
    addInstr $ X86.Mov QWord asmres (RegOp RAX)

compileQuad (Quad.VoidCall name args)             = do
    call name args

compileQuad (Quad.VCall name args this off1 res) = do
    regs <- gets mapping
    let asmres = fromMaybe undefined (M.lookup res regs)
    let asmthis = fromMaybe undefined (M.lookup this regs)
    addInstr $ X86.Mov QWord (RegOp RAX) asmthis
    addInstr $ X86.Mov QWord (RegOp RAX) (MemOp $ RegOff RAX 0)
    addInstr $ X86.Mov QWord (RegOp RAX) (MemOp $ RegOff RAX off1)
    call name args

compileQuad (Quad.VoidVCall name args this off1) = do
    regs <- gets mapping
    let asmthis = fromMaybe undefined (M.lookup this regs)
    addInstr $ X86.Mov QWord (RegOp RAX) asmthis
    addInstr $ X86.Mov QWord (RegOp RAX) (MemOp $ RegOff RAX 0)
    addInstr $ X86.Mov QWord (RegOp RAX) (MemOp $ RegOff RAX off1)
    call name args

compileQuad (Quad.Vtab r1 t) = do
    classes <- gets X86.classes
    case t of
      ObjectType _ (Ident name) -> do
            case M.lookup name classes of
              Nothing -> return ()
              Just qc ->
                case Quad.vtable qc of
                    Nothing -> return ()
                    Just vl -> do
                        regs <- gets mapping
                        let asmr1 = fromMaybe undefined (M.lookup r1 regs)
                        addInstr $ X86.Mov QWord (RegOp RAX) asmr1
                        addInstr $ X86.Mov QWord (MemOp $ RegOff RAX 0) (ValOp $ VLabel (qtoasmlbl vl))
      _ -> return ()

compilePhi :: AsmOperand -> AsmLabel -> AsmLabel -> CompilerState ()
compilePhi op l afterL = do
    addInstr $ X86.Mov QWord op (ValOp $ VInt 0)
    addInstr $ X86.Jmp afterL
    addInstr $ X86.ILabel l
    addInstr $ X86.Mov QWord op (ValOp $ VInt (-1))
    addInstr $ X86.ILabel afterL

initialFunQuad :: String -> CompilerState ()
initialFunQuad l = do
    size <- gets usedMem
    addInstr $ X86.ILabel $ AsmLabel l
    let allocation = 8 + align16 (8 * size)
    addInstr $ Enter allocation
    addInstr $ Push QWord (RegOp RBX)
    addInstr $ Push QWord (RegOp R12)
    addInstr $ Push QWord (RegOp R13)
    addInstr $ Push QWord (RegOp R14)
    addInstr $ Push QWord (RegOp R15)


retPop :: CompilerState ()
retPop = do
    addInstr $ Pop QWord (RegOp R15)
    addInstr $ Pop QWord (RegOp R14)
    addInstr $ Pop QWord (RegOp R13)
    addInstr $ Pop QWord (RegOp R12)
    addInstr $ Pop QWord (RegOp RBX)
    addInstr Leave

call :: String -> [Register] -> CompilerState ()
call name args = do
    regs <- gets mapping
    newArgs <- passRegs args
    mapM_ (\r -> do
    let asmr = fromMaybe undefined (M.lookup r regs)
    addInstr $ Push QWord asmr
    ) (reverse newArgs)
    when (odd $ length newArgs) (addInstr $ X86.Sub QWord (RegOp RSP) (ValOp $ VInt 8))
    addInstr $ X86.Call (CLabel $ AsmLabel name)
    addInstr $ X86.Add QWord (RegOp RSP) (ValOp $ VInt (align16 $ 8 * length newArgs))

passRegs :: [Register] -> CompilerState [Register]
passRegs rs = _passRegs rs 1

_passRegs :: [Register] -> Int -> CompilerState [Register]
_passRegs [] _ = return []
_passRegs (r:rs) nr = do
    regs <- gets mapping
    let asmr = fromMaybe undefined (M.lookup r regs)
    case nr of
        1 -> do
            addInstr $ X86.Mov QWord (RegOp RDI) asmr
            _passRegs rs (nr + 1)
        2 -> do
            addInstr $ X86.Mov QWord (RegOp RSI) asmr
            _passRegs rs (nr + 1)
        3 -> do
            addInstr $ X86.Mov QWord (RegOp RDX) asmr
            _passRegs rs (nr + 1)
        4 -> do
            addInstr $ X86.Mov QWord (RegOp RCX) asmr
            _passRegs rs (nr + 1)
        5 -> do
            addInstr $ X86.Mov QWord (RegOp R8) asmr
            _passRegs rs (nr + 1)
        6 -> do
            addInstr $ X86.Mov QWord (RegOp R9) asmr
            _passRegs rs (nr + 1)
        _ -> return (r:rs)

----------------------------------------------------------------------------
allocMemory :: [Quadruple] -> AllocatorState (MemoryAllocation, Int)
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
      Nothing -> _defineIntervals all i
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
    let all = filter (\x -> not $ S.member x ints) (extractAll q)
    fu <- gets fusage
    lu <- gets lusage
    mapM_ (\r -> do
        st1 <- get
        case M.lookup r fu of
            Nothing -> return ()
            Just n  -> do
                when (i == n) (do
                when (null (memoryPool st1)) (do
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
