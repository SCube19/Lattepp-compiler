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
import           Syntax.AbsLattepp              (Program, Program' (Program))

compile :: Program -> ExceptT String IO String
compile p@(Program _ defs) =  do
    quads <- evalStateT (quadruplize p) (initQuadruplesS defs)
    asm <- evalStateT (compileQuads quads) initCompilerS
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
    addInstr $ X86.ILabel $ AsmLabel (fident f)
    addInstr $ Push DWord (RegOp EBP)
    addInstr $ X86.Mov DWord (RegOp EBP) (RegOp ESP)
    let alignment = 4 * ( size + (4 - (size + 2) `mod` 4) `mod` 4)
    addInstr $ X86.Sub DWord (RegOp ESP) (ValOp $ VInt alignment)
    mapM_ compileQuad (body f)

compileQuad :: Quadruple -> CompilerState ()
compileQuad (Quad.Add r1 r2 res)                  = do
    regs <- gets mapping
    let asmres = fromMaybe undefined (M.lookup res regs)
    let asmr1 = fromMaybe undefined (M.lookup r1 regs)
    let asmr2 = fromMaybe undefined (M.lookup r2 regs)
    addInstr $ X86.Mov DWord (RegOp EAX) asmr1
    addInstr $ X86.Add DWord (RegOp EAX) asmr2
    addInstr $ X86.Mov DWord asmres (RegOp EAX)

compileQuad (Quad.Sub r1 r2 res)                  = do
    regs <- gets mapping
    let asmres = fromMaybe undefined (M.lookup res regs)
    let asmr1 = fromMaybe undefined (M.lookup r1 regs)
    let asmr2 = fromMaybe undefined (M.lookup r2 regs)
    addInstr $ X86.Mov DWord (RegOp EAX) asmr1
    addInstr $ X86.Sub DWord (RegOp EAX) asmr2
    addInstr $ X86.Mov DWord asmres (RegOp EAX)

compileQuad (Quad.Div r1 r2 res)                  = do
    regs <- gets mapping
    let asmres = fromMaybe undefined (M.lookup res regs)
    let asmr1 = fromMaybe undefined (M.lookup r1 regs)
    let asmr2 = fromMaybe undefined (M.lookup r2 regs)
    addInstr $ X86.Mov DWord (RegOp EAX) asmr1
    addInstr Cdq
    if isAsmValue asmr2 then do
        addInstr $ X86.Mov DWord (RegOp EBX) asmr2
        addInstr $ X86.Div DWord (RegOp EBX)
    else
        addInstr $ X86.Div DWord asmr2
    addInstr $ X86.Mov DWord asmres (RegOp EAX)

compileQuad (Quad.Mul r1 r2 res)                  = do
    regs <- gets mapping
    let asmres = fromMaybe undefined (M.lookup res regs)
    let asmr1 = fromMaybe undefined (M.lookup r1 regs)
    let asmr2 = fromMaybe undefined (M.lookup r2 regs)
    addInstr $ X86.Mov DWord (RegOp EAX) asmr1
    addInstr $ X86.Mul DWord (RegOp EAX) asmr2
    addInstr $ X86.Mov DWord asmres (RegOp EAX)

compileQuad (Quad.Mod r1 r2 res)                  = do
    regs <- gets mapping
    let asmres = fromMaybe undefined (M.lookup res regs)
    let asmr1 = fromMaybe undefined (M.lookup r1 regs)
    let asmr2 = fromMaybe undefined (M.lookup r2 regs)
    addInstr $ X86.Mov DWord (RegOp EAX) asmr1
    addInstr Cdq
    if isAsmValue asmr2 then do
        addInstr $ X86.Mov DWord (RegOp EBX) asmr2
        addInstr $ X86.Div DWord (RegOp EBX)
    else
        addInstr $ X86.Div DWord asmr2
    addInstr $ X86.Mov DWord asmres (RegOp EDX)

compileQuad (Quad.Cmp r1 r2)                      = do
    regs <- gets mapping
    let asmr1 = fromMaybe undefined (M.lookup r1 regs)
    let asmr2 = fromMaybe undefined (M.lookup r2 regs)
    addInstr $ X86.Mov DWord (RegOp EAX) asmr2
    if isAsmValue asmr1 then do
        addInstr $ X86.Mov DWord (RegOp EBX) asmr1
        addInstr $ X86.Cmp DWord (RegOp EBX) (RegOp EAX)
    else
        addInstr $ X86.Cmp DWord asmr1 (RegOp EAX)

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

compileQuad (Quad.Neg r1)                      = do
    regs <- gets mapping
    let asmr1 = fromMaybe undefined (M.lookup r1 regs)
    addInstr $ X86.Neg DWord asmr1

compileQuad (Quad.Not r1)                      = do
    regs <- gets mapping
    let asmr1 = fromMaybe undefined (M.lookup r1 regs)
    addInstr $ X86.Not DWord asmr1

compileQuad (Quad.MovV v r1)                      = do
    regs <- gets mapping
    let asmr1 = fromMaybe undefined (M.lookup r1 regs)
    unless (isAsmValue asmr1) $
        addInstr $ X86.Mov DWord asmr1 (ValOp (VInt $ qvalueInt v))

compileQuad (Quad.Mov r1 r2)                      = do
    regs <- gets mapping
    let asmr1 = fromMaybe undefined (M.lookup r1 regs)
    let asmr2 = fromMaybe undefined (M.lookup r2 regs)
    addInstr $ X86.Mov DWord asmr1 asmr2

compileQuad (Quad.Inc (QIndex i _))                   = do
    regs <- gets mapping
    addInstr $ X86.Inc DWord (MemOp (RegOff EBP (-4 * (i + 1))))

compileQuad (Quad.Dec (QIndex i _))                   = do
    regs <- gets mapping
    addInstr $ X86.Dec DWord (MemOp (RegOff EBP (-4 * (i + 1))))

compileQuad (Quad.Ret r1)                         = do
    regs <- gets mapping
    let asmr1 = fromMaybe undefined (M.lookup r1 regs)
    addInstr $ X86.Mov DWord (RegOp EAX) asmr1
    addInstr Leave
    addInstr X86.Ret

compileQuad Quad.Vret                             = do
    addInstr Leave
    addInstr X86.Ret

compileQuad (Quad.Label l1)                       =
    addInstr $ X86.ILabel $ qtoasmlbl l1

compileQuad (Quad.Load (QIndex i _) res)                 = do
    regs <- gets mapping
    let asmres = fromMaybe undefined (M.lookup res regs)
    addInstr $ X86.Mov DWord (RegOp EAX) (MemOp $ RegOff EBP (-4 * (i + 1)))
    addInstr $ X86.Mov DWord asmres (RegOp EAX)

compileQuad (Quad.LoadArg (QIndex i _))                  = do
    addInstr $ X86.Mov DWord (RegOp EAX) (MemOp $ RegOff EBP (4 * (i + 2)))
    addInstr $ X86.Mov DWord (MemOp $ RegOff EBP (-4 * (i + 1))) (RegOp EAX)

compileQuad (Quad.LoadIndir r1 off1 r2 off2 res)  = return ()
compileQuad (Quad.LoadLbl l1 res)                 = do
    regs <- gets mapping
    let asmres = fromMaybe undefined (M.lookup res regs)
    addInstr $ X86.Mov DWord asmres (ValOp $ VLabel $ AsmLabel $ labelref l1)

compileQuad (Quad.Store r1 (QIndex i _))              = do
    regs <- gets mapping
    let asmr1 = fromMaybe undefined (M.lookup r1 regs)
    addInstr $ X86.Mov DWord (RegOp EAX) asmr1
    addInstr $ X86.Mov DWord (MemOp $ RegOff EBP (-4 * (i + 1))) (RegOp EAX)

compileQuad (Quad.StoreIndir r1 off1 r2 off2 res) = return ()
compileQuad (Quad.Alloc index)                    = return ()
compileQuad (Quad.Call name args res)             = do
    regs <- gets mapping
    let asmres = fromMaybe undefined (M.lookup res regs)
    let alignment = 4 * ((4 - (length args `mod` 4)) `mod` 4)
    addInstr $ X86.Sub DWord (RegOp ESP) (ValOp $ VInt alignment)
    mapM_ (\r -> do
        let asmr = fromMaybe undefined (M.lookup r regs)
        addInstr $ Push DWord asmr
        ) (reverse args)
    addInstr $ X86.Call (CLabel $ AsmLabel name)
    addInstr $ X86.Add DWord (RegOp ESP) (ValOp $ VInt (4 * length args))
    addInstr $ X86.Mov DWord asmres (RegOp EAX)

compileQuad (Quad.VoidCall name args)             = do
    regs <- gets mapping
    let alignment = 4 * ((4 - (length args `mod` 4)) `mod` 4)
    addInstr $ X86.Sub DWord (RegOp ESP) (ValOp $ VInt alignment)
    mapM_ (\r -> do
        let asmr = fromMaybe undefined (M.lookup r regs)
        addInstr $ Push DWord asmr
        ) (reverse args)
    addInstr $ X86.Call (CLabel $ AsmLabel name)
    addInstr $ X86.Add DWord (RegOp ESP) (ValOp $ VInt (4 * length args))

compileQuad (Quad.VCall name args r1 off1 res)    = return ()
compileQuad (Quad.Vtab r1 t)                      = return ()

----------------------------------------------------------------------------
allocMemory :: [Quadruple] -> AllocatorState (MemoryAllocation, Int)
allocMemory qs = do
    let consts = getConsts qs S.empty
    defineIntervals qs consts 0
    fu <- gets fusage
    lu <- gets lusage
    al <- gets allocation
    makeAllocation qs consts 0
    alloc <- gets allocation
    size <- gets allocSize
    return (alloc, size)


getConsts :: [Quadruple] -> S.Set Register -> S.Set Register
getConsts [] s = s
getConsts (q:qs) s =
    let mreg = extractResult q in
    case mreg of
        Nothing -> getConsts qs s
        Just reg ->
            if S.member reg s then
                getConsts qs (S.delete reg s)
            else
                getConsts qs (S.insert reg s)

defineIntervals :: [Quadruple] -> S.Set Register -> Int -> AllocatorState () --fix
defineIntervals [] _ _ = return ()
defineIntervals (q:qs) consts i = do
    ints <- gets integers
    let all = filter (\x -> not $ S.member x ints) (extractAll q)
    let result = extractResult q
    case result of
      Nothing -> _defineIntervals all i
      Just reg ->
        case q of
            MovV v _ ->
                if S.member reg consts then do
                    allocate reg (ValOp (VInt $ qvalueInt v))
                    _defineIntervals all i
                else _defineIntervals all i
            _ -> _defineIntervals all i
    defineIntervals qs consts (i + 1)

_defineIntervals :: [Register] -> Int -> AllocatorState ()
_defineIntervals rs i = do
    mapM_ (\r -> do
        insertFirstUsage r i
        insertLastUsage r i) rs

makeAllocation :: [Quadruple] -> S.Set Register -> Int -> AllocatorState ()
makeAllocation [] _ _ = return ()
makeAllocation (q:qs) consts i = do
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
                    addToPool $ MemOp $ RegOff EBP off)
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
    makeAllocation qs consts (i + 1)
