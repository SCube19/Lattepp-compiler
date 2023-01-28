module Quadruples.Optimizer.Optimizer where
import           Compiler.Data                  (AsmOperand (ValOp),
                                                 AsmValue (VInt))
import           Control.Monad.Except           (runExcept, runExceptT)
import           Control.Monad.State            (MonadTrans (lift),
                                                 StateT (runStateT), evalState,
                                                 evalStateT, gets, modify,
                                                 unless)
import qualified Data.Map                       as M
import qualified Data.Set                       as S
import           GHC.TopHandler                 (runIO)
import           Quadruples.Data
import           Quadruples.Optimizer.Data      (OptimizerS (copy, fusage, lusage, quadAcc),
                                                 OptimizerState, addAcc,
                                                 initOptimizerS,
                                                 insertFirstUsage,
                                                 insertLastUsage, makeCopy,
                                                 mapCopy, resetAcc)
import           Quadruples.Optimizer.Gcse.Data (initGcseS)
import           Quadruples.Optimizer.Gcse.Gcse (gcse)

optimizeQProgram :: QProgram -> OptimizerState QProgram
optimizeQProgram p = do
    res <- mapM optimizeFuncs (M.toList $ funcs p)
    return $ QProgram (classes p) (M.fromList res) (strings p)

optimizeFuncs :: (String, QFun) -> OptimizerState (String, QFun)
optimizeFuncs (name, fun) = do
    modify (const initOptimizerS)
    res <- optimizeQuads (body fun)
    return (name, QFun {
        fident = fident fun,
        localcount = localcount fun,
        ret = ret fun,
        body = res,
        offset = offset fun})

optimizeQuads :: [Quadruple] -> OptimizerState [Quadruple]
optimizeQuads qs = do return qs
    -- qs1 <- lift $ evalStateT (gcse qs) initGcseS
    -- copyPropagation qs1
    -- qs2 <- gets quadAcc
    -- resetAcc
    -- liveRanges qs2 0
    -- deadCode qs2
    -- gets quadAcc


copyPropagation :: [Quadruple] -> OptimizerState ()
copyPropagation [] = return ()

copyPropagation (q@(Mov r1 r2):qs) = do
    makeCopy r1 r2
    addAcc q
    copyPropagation qs

copyPropagation (q:qs) = do
    copies <- gets copy
    case q of
      Add r1 r2 res                -> addAcc $ Add (mapCopy r1 copies) (mapCopy r2 copies) res
      Sub r1 r2 res                -> addAcc $ Sub (mapCopy r1 copies) (mapCopy r2 copies) res
      Div r1 r2 res                -> addAcc $ Div (mapCopy r1 copies) (mapCopy r2 copies) res
      Mul r1 r2 res                -> addAcc $ Mul (mapCopy r1 copies) (mapCopy r2 copies) res
      Mod r1 r2 res                -> addAcc $ Mod (mapCopy r1 copies) (mapCopy r2 copies) res
      Cmp r1 r2                    -> addAcc $ Cmp (mapCopy r1 copies) (mapCopy r2 copies)
      Neg r1 res                   -> addAcc $ Neg (mapCopy r1 copies) res
      Not r1 res                   -> addAcc $ Not (mapCopy r1 copies) res
      Ret r1                       -> addAcc $ Ret (mapCopy r1 copies)
      LoadIndir r1 n r2 i res      -> addAcc $ LoadIndir (mapCopy r1 copies) n (mapCopy r2 copies) i res
      StoreIndir r1 n r2 i val     -> addAcc $ StoreIndir (mapCopy r1 copies) n (mapCopy r2 copies) i (mapCopy val copies)
      Call s args res              -> addAcc $ Call s (map (`mapCopy` copies) args) res
      VoidCall s args              -> addAcc $ VoidCall s (map (`mapCopy` copies) args)
      VCall s args r1 n res        -> addAcc $ VCall s (map (`mapCopy` copies) args) (mapCopy r1 copies) n res
      VoidVCall s args r1 n        -> addAcc $ VoidVCall s (map (`mapCopy` copies) args) (mapCopy r1 copies) n
      Vtab r1 ty                   -> addAcc $ Vtab (mapCopy r1 copies) ty
      x -> addAcc x
    copyPropagation qs

deadCode :: [Quadruple] -> OptimizerState ()
deadCode []     = return ()
deadCode (q:qs) = do
    f <- gets fusage
    l <- gets lusage
    let result = extractResult q
    case result of
      Nothing -> do
        addAcc q
        deadCode qs
      Just reg ->
        case M.lookup reg f of
            Nothing -> undefined
            Just n  ->
                case M.lookup reg l of
                    Nothing -> undefined
                    Just m -> do
                        unless (n == m) (addAcc q)
                        deadCode qs


liveRanges :: [Quadruple] -> Int -> OptimizerState ()
liveRanges [] _ = return ()
liveRanges (q:qs) i = do
    let all = extractAll q
    _liveRanges all i
    liveRanges qs (i + 1)

_liveRanges :: [Register] -> Int -> OptimizerState ()
_liveRanges rs i = do
    mapM_ (\r -> do
        insertFirstUsage r i
        insertLastUsage r i) rs
