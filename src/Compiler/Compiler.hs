module Compiler.Compiler where
import           Compiler.Data
import           Compiler.Quadruples.Data       as Quad
import           Compiler.Quadruples.Quadruples (quadruplize)
import           Control.Monad.Trans.Except     (ExceptT)
import           Control.Monad.Trans.State      (evalStateT, gets)
import qualified Data.Map                       as M
import qualified Data.Set                       as S
import           Syntax.AbsLattepp              (Program)

compile :: Program -> ExceptT String IO String
compile p =  do
    quads <- evalStateT (quadruplize p) initQuadruplesS
    asm <- evalStateT (compileQuads quads) initCompilerS
    return $ show asm

compileQuads :: QProgram -> CompilerState [AsmInstr]
compileQuads p = do
    addInstr $ Section "data"
    mapM_ (\(s, l) -> addInstr $ DataString (show l) s) (M.toList $ strings p)
    addInstr $ Section "text"
    addInstr $ Global "main"
    addInstr $ Extern "printInt"
    addInstr $ Extern "printString"
    addInstr $ Extern "readInt"
    addInstr $ Extern "readString"
    addInstr $ Extern "error"
    addInstr $ Extern "1__concat"
    addInstr $ Extern "1__equals"
    mapM_ compileQuadsFun (funcs p)
    gets instructions


compileQuadsFun :: QFun -> CompilerState ()
compileQuadsFun f = do
    return ()


compileQuad :: Quadruple -> CompilerState ()
compileQuad (Quad.Add r1 r2 res)                  = return ()
compileQuad (Quad.Sub r1 r2 res)                  = return ()
compileQuad (Quad.Div r1 r2 res)                  = return ()
compileQuad (Quad.Mul r1 r2 res)                  = return ()
compileQuad (Quad.Mod r1 r2 res)                  = return ()
compileQuad (Quad.Cmp r1 r2)                      = return ()
compileQuad (Quad.Jmp l1)                         = return ()
compileQuad (Quad.Je l1)                          = return ()
compileQuad (Quad.Jne l1)                         = return ()
compileQuad (Quad.Jge l1)                         = return ()
compileQuad (Quad.Jg l1)                          = return ()
compileQuad (Quad.Jle l1)                         = return ()
compileQuad (Quad.Jl l1)                          = return ()
compileQuad (Quad.Neg r1)                         = return ()
compileQuad (Quad.Not r1)                         = return ()
compileQuad (Quad.MovV v r1)                      = return ()
compileQuad (Quad.Mov r1 r2)                      = return ()
compileQuad (Quad.Inc index r1)                   = return ()
compileQuad (Quad.Dec index r2)                   = return ()
compileQuad (Quad.Ret r1)                         = return ()
compileQuad Quad.Vret                             = return ()
compileQuad (Quad.Label l1)                       = return ()
compileQuad (Quad.FLabel s)                       = return ()
compileQuad (Quad.Load index res)                 = return ()
compileQuad (Quad.LoadArg index)                  = return ()
compileQuad (Quad.LoadIndir r1 off1 r2 off2 res)  = return ()
compileQuad (Quad.LoadLbl l1 res)                 = return ()
compileQuad (Quad.Store r1 resIndex)              = return ()
compileQuad (Quad.StoreIndir r1 off1 r2 off2 res) = return ()
compileQuad (Quad.Alloc index)                    = return ()
compileQuad (Quad.Call name args res)             = return ()
compileQuad (Quad.VCall name args r1 off1 res)    = return ()
compileQuad (Quad.Vtab r1 t)                      = return ()

----------------------------------------------------------------------------
allocMemory :: [Quadruple] -> AllocatorState MemoryAllocation
allocMemory qs = do
    let consts = getConsts qs S.empty
    gets allocation


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
