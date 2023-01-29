module Quadruples.Optimizer.Lcse.Lcse where
import           Control.Monad.RWS              (MonadIO (liftIO), gets)
import           Quadruples.Data
import           Quadruples.Optimizer.Lcse.Data (LcseS (quadAcc, regMapping),
                                                 LcseState, addAcc,
                                                 addExpression, addMapping,
                                                 clearState, getExpression,
                                                 getMapping)
import           Quadruples.Optimizer.Utils     (basicBlockBoundry)
import           Utils                          (rawInt)

lcse :: [Quadruple] -> LcseState [Quadruple]
lcse [] = do
    qs <- gets quadAcc
    return $ reverse qs

lcse (q:qs) = do
    if basicBlockBoundry q then do
        clearState
        addAcc q
    else do
        mreg <- getExpression q
        case mreg of
          Nothing -> do
            q1 <- mapRegs q
            addExpression q1
            addAcc q1
          Just reg -> do
            let mres = extractResult q
            case mres of
              Nothing -> undefined
              Just res -> do
                addMapping res reg
                addAcc $ Mov reg res
    lcse qs


mapRegs :: Quadruple -> LcseState Quadruple
mapRegs q = do
  mapping <- gets regMapping
  let mapReg = flip getMapping mapping
  return $ case q of
    Add r1 r2 res           -> Add (mapReg r1) (mapReg r2) res
    Sub r1 r2 res           -> Sub (mapReg r1) (mapReg r2) res
    Div r1 r2 res           -> Div (mapReg r1) (mapReg r2) res
    Mul r1 r2 res           -> Mul (mapReg r1) (mapReg r2) res
    Mod r1 r2 res           -> Mod (mapReg r1) (mapReg r2) res
    Cmp r1 r2               -> Cmp (mapReg r1) (mapReg r2)
    Neg r1 res              -> Neg (mapReg r1) res
    Not r1 res              -> Not (mapReg r1) res
    Mov r1 res              -> Mov (mapReg r1) res
    Ret r1                  -> Ret (mapReg r1)
    LoadIndir r1 n r2 i res -> LoadIndir (mapReg r1) n (mapReg r2) i res
    Store r1 qi             -> Store (mapReg r1) qi
    StoreIndir r1 n r2 i r3 -> LoadIndir (mapReg r1) n (mapReg r2) i (mapReg r3)
    Call s regs res         -> Call s (map mapReg regs) res
    VoidCall s regs         -> VoidCall s (map mapReg regs)
    VCall s regs r1 n res   -> VCall s (map mapReg regs) (mapReg r1) n res
    VoidVCall s regs r1 n   -> VoidVCall s (map mapReg regs) (mapReg r1) n
    Vtab r1 ty              -> Vtab (mapReg r1) ty
    x                       -> x
