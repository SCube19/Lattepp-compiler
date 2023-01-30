module Quadruples.Quadruples where

import           Abstract.Typechecker.Data      (TypeCheckerS)
import           Control.Monad.State            (MonadIO (liftIO),
                                                 MonadState (get, put),
                                                 MonadTrans (lift), evalStateT,
                                                 gets, modify, unless, when)
import           Control.Monad.Trans.Except     (ExceptT)
import qualified Data.Map                       as M
import           Data.Maybe                     (fromMaybe, isNothing)
import           Quadruples.Data                as QD
import           Quadruples.Optimizer.Data      (initOptimizerS)
import           Quadruples.Optimizer.Optimizer (optimizeQProgram)
import           Quadruples.Predata
import           Quadruples.Preprocess          (preprocess)
import           Syntax.AbsLattepp              as Abs
import           Utils                          (Raw (raw), getArrayType,
                                                 isArrayType, rawBool, rawInt,
                                                 rawStr, rawVoid)

--usunąć indexy na redeklaracji robić nowy rejestr a nie load store
quadruplize :: Program -> TypeCheckerS -> QuadruplesState QProgram
quadruplize p@(Program _ defs) tcEnv = do
    preprocessing <- lift $ evalStateT (preprocess p tcEnv) initPreprocessS
    convertPreprocessing preprocessing
    mapM_ quadruplizeTopDef defs
    prog <- gets qprogram
    store <- gets localStore
    optimized <- lift $ evalStateT (optimizeQProgram prog) initOptimizerS
    liftIO (writeFile "quads.txt" (show prog))
    return optimized


quadruplizeTopDef :: TopDef -> QuadruplesState ()
quadruplizeTopDef (FnDef pos ret (Ident ident) args block)  = do
    l <- maxLocalsBlock block 0
    mx <- gets maxLocals
    let toReserve = mx + length args
    modify resetMaxLocals
    addFun $ QFun ident ret toReserve [] 0
    setStore (initLocalStore toReserve)
    loadArgs args
    quadruplizeBlock block
    when (raw ret == rawVoid) $ addQuad Vret

quadruplizeTopDef (ClassDef pos ident block) = quadruplizeClassBlock ident block

quadruplizeTopDef (ExtClassDef pos ident ext block) = quadruplizeClassBlock ident block

quadruplizeBlock :: Block -> QuadruplesState ()
quadruplizeBlock (Block pos stmts) = do
    st <- get
    storeEnv st (mapM_ quadruplizeStmt stmts)

quadruplizeClassBlock :: Ident -> ClassBlock -> QuadruplesState ()
quadruplizeClassBlock cName (ClassBlock pos stmts) = mapM_ (quadruplizeClassStmt cName) stmts

quadruplizeClassStmt :: Ident -> ClassStmt -> QuadruplesState ()
quadruplizeClassStmt _ (ClassEmpty pos)                       = return ()
quadruplizeClassStmt _ (ClassDecl pos t items)                = return ()
quadruplizeClassStmt c@(Ident cName) (ClassMethod pos ret (Ident i) args block) = do
    l <- maxLocalsBlock block 0
    mx <- gets maxLocals
    let selfargs = Arg Nothing (ObjectType Nothing c) (Ident "self") : args
    let toReserve = mx + length selfargs
    modify resetMaxLocals
    let compundName = cName ++ "__" ++ i
    addFun $ QFun compundName ret toReserve [] 0
    setStore (initLocalStore toReserve)
    loadArgs selfargs
    quadruplizeBlock block
    when (raw ret == rawVoid) $ addQuad Vret

quadruplizeExtIdent :: ExtIdent -> QuadruplesState ()
quadruplizeExtIdent (Id pos ident)           = return ()
quadruplizeExtIdent (ArrId pos ident expr)   = return ()
quadruplizeExtIdent (AttrId pos expr1 expr2) = return ()

quadruplizeStmt :: Stmt -> QuadruplesState ()
quadruplizeStmt (Empty pos)                     = return ()

quadruplizeStmt (BStmt pos block)               = quadruplizeBlock block

quadruplizeStmt (Decl pos t items)              = do
    mapM_ (quadruplizeItem t) items

quadruplizeStmt (Ass pos ident expr)            = do
    reg <- quadruplizeExpr expr
    st <- gets localStore
    case ident of
      Id _ id -> do
        case M.lookup id (varToMem st) of
                Nothing -> undefined
                Just mem -> do
                        addQuad $ Store reg mem
      ArrId _ id ex -> do
        case M.lookup id (varToMem st) of
          Nothing -> undefined
          Just arr -> do
            index <- quadruplizeExpr ex
            addr <- getRegister rawInt
            addQuad $ Load arr addr
            addQuad $ StoreIndir addr 8 (Just index) 8 (Just reg)
      AttrId _ ex ex' -> undefined

quadruplizeStmt (Incr pos ident)                = do
    st <- gets localStore
    case ident of
      Id _ id -> case M.lookup id (varToMem st) of
                    Nothing -> undefined
                    Just mem -> do
                        addQuad $ Inc mem
      ArrId _ id ex -> do
        case M.lookup id (varToMem st) of
          Nothing -> undefined
          Just arr -> do
            index <- quadruplizeExpr ex
            m1 <- getRegister rawInt
            res <- getRegister rawInt
            load <- getRegister rawInt
            addr <- getRegister rawInt
            addQuad $ Load arr addr
            addQuad $ LoadIndir addr 8 (Just index) 8 load
            addQuad $ MovV (QInt 1) m1
            addQuad $ Add load m1 res
            addQuad $ StoreIndir addr 8 (Just index) 8 (Just res)
      AttrId ma ex ex' -> undefined


quadruplizeStmt (Decr pos ident)                = do
    st <- gets localStore
    case ident of
      Id _ id -> case M.lookup id (varToMem st) of
                    Nothing -> undefined
                    Just mem -> do
                        addQuad $ Dec mem
      ArrId _ id ex -> do
        case M.lookup id (varToMem st) of
          Nothing -> undefined
          Just arr -> do
            index <- quadruplizeExpr ex
            m1 <- getRegister rawInt
            res <- getRegister rawInt
            load <- getRegister rawInt
            addr <- getRegister rawInt
            addQuad $ Load arr addr
            addQuad $ LoadIndir addr 8 (Just index) 8 load
            addQuad $ MovV (QInt 1) m1
            addQuad $ Sub load m1 res
            addQuad $ StoreIndir addr 8 (Just index) 8 (Just res)
      AttrId ma ex ex' -> undefined

quadruplizeStmt (Abs.Ret pos expr)              = do
    reg <- quadruplizeExpr expr
    addQuad $ QD.Ret reg

quadruplizeStmt (VRet pos)                      = addQuad Vret

quadruplizeStmt (Cond pos expr stmt)            = do
    falseLabel <- getLabel
    ifReg <- quadruplizeExpr expr
    zero <- getRegister rawInt
    addQuad $ MovV (QInt 0) zero
    addQuad $ Cmp ifReg zero
    addQuad $ Je falseLabel
    quadruplizeBlock (Block Nothing [stmt])
    addQuad $ Label falseLabel

quadruplizeStmt (CondElse pos expr stmt1 stmt2) = do
    elseLabel <- getLabel
    afterLabel <- getLabel
    ifReg <- quadruplizeExpr expr
    zero <- getRegister rawInt
    addQuad $ MovV (QInt 0) zero
    addQuad $ Cmp ifReg zero
    addQuad $ Je elseLabel
    quadruplizeBlock (Block Nothing [stmt1])
    addQuad $ Jmp afterLabel
    addQuad $ Label elseLabel
    quadruplizeBlock (Block Nothing [stmt2])
    addQuad $ Label afterLabel

quadruplizeStmt (While pos expr stmt)           = do
    bodyLabel <- getLabel
    condLabel <- getLabel
    zero <- getRegister rawInt
    addQuad $ Jmp condLabel
    addQuad $ Label bodyLabel
    quadruplizeBlock (Block Nothing [stmt])
    addQuad $ Label condLabel
    condReg <- quadruplizeExpr expr
    addQuad $ MovV (QInt 0) zero
    addQuad $ Cmp condReg zero
    addQuad $ Jne bodyLabel

quadruplizeStmt (For pos t ident1@(Ident i) ident2 stmt)  = do
    st <- get
    local <- gets localStore
    let arr = case ident2 of
          Id _ id         -> fromMaybe undefined (M.lookup id (varToMem local))
          AttrId _ ex ex' -> undefined
          _               -> undefined
    storeEnv st (do
        bodyLabel <- getLabel
        condLabel <- getLabel

        zero <- getRegister rawInt
        value <- getRegister t
        index <- getRegister rawInt
        indexCheck <- getRegister rawInt
        len <- getRegister rawInt
        addr1 <- getRegister rawInt
        addr2 <- getRegister rawInt

        x <- store ident1 t
        xiter <- store (Ident $ i ++ "__ITERATOR") rawInt

        addQuad $ MovV (QInt 0) zero
        addQuad $ Store zero xiter

        addQuad $ Jmp condLabel

        addQuad $ Label bodyLabel
        addQuad $ Load xiter index
        addQuad $ Load arr addr1
        addQuad $ LoadIndir addr1 8 (Just index) 8 value
        addQuad $ Store value x

        quadruplizeBlock (Block Nothing [stmt])

        addQuad $ Inc xiter

        addQuad $ Label condLabel
        addQuad $ Load xiter indexCheck
        addQuad $ Load arr addr2
        addQuad $ LoadIndir addr2 0 Nothing 0 len
        addQuad $ Cmp indexCheck len
        addQuad $ Jl bodyLabel)

quadruplizeStmt (SExp pos expr)                 = do
    quadruplizeExpr expr
    return ()

quadruplizeItem :: Type -> Item -> QuadruplesState ()
quadruplizeItem t (NoInit pos ident)    = do
    reg <- getRegister t
    addQuad $ MovV (QInt 0) reg
    case t of
      Primitive _ pt -> do
            mem <- store ident t
            addQuad $ Store reg mem
      ObjectType ma id -> return ()
      Array _ t1 -> do
            arr <- store ident t
            addQuad $ StoreIndir reg 0 Nothing 0 Nothing
            addQuad $ Store reg arr
      _ -> undefined

quadruplizeItem t (Init pos ident expr) = do
    reg <- quadruplizeExpr expr
    case t of
      Primitive ma pt -> do
        mem <- store ident t
        addQuad $ Store reg mem
      ObjectType ma id -> return ()
      Array _ t1 -> do
            arr <- store ident t
            addQuad $ Store reg arr
      _ -> undefined

quadruplizeExpr :: Expr -> QuadruplesState Register
quadruplizeExpr (ECast pos ident expr)    = do
    (Register reg _) <- quadruplizeExpr expr
    return $ Register reg (ObjectType Nothing ident)

quadruplizeExpr (ECastPrim pos t expr)    = do
    (Register reg _) <- quadruplizeExpr expr
    return $ Register reg (Primitive Nothing t)

quadruplizeExpr (ENewObject pos ident)    = do
    classSize <- getClassSize ident 0
    let vtableSize = classSize + 8
    sizeReg <- getRegister rawInt
    res <- getRegister (raw $ ObjectType Nothing ident)
    addQuad $ MovV (QInt vtableSize) sizeReg
    addQuad $ Call "__heap" [sizeReg] res
    addQuad $ Vtab res (regType res)
    return res

quadruplizeExpr (ENewArr pos t expr)      = do
    sizeReg <- quadruplizeExpr expr
    m1 <- getRegister rawInt
    realSize <- getRegister rawInt
    bytes <- getRegister rawInt
    addQuad $ MovV (QInt 1) m1
    addQuad $ Add sizeReg m1 realSize
    res <- getRegister (raw $ Array Nothing t)
    addQuad $ Call "__heap" [realSize] res
    addQuad $ StoreIndir res 0 Nothing 0 (Just sizeReg)
    return res

quadruplizeExpr (ENull pos)               = do
    reg <- getRegister rawVoid
    addQuad $ MovV (QInt 0) reg
    return reg

quadruplizeExpr (EObject pos expr1 expr2) = do
    left@(Register _ t) <- quadruplizeExpr expr1
    if isArrayType t then do
        l <- getRegister rawInt
        addQuad $ LoadIndir left 0 Nothing 0 l
        return l
    else
        return left

quadruplizeExpr (EArr pos ident expr)     = do
    s <- gets localStore
    case M.lookup ident (varToMem s) of
      Nothing -> undefined
      Just arr@(QIndex _ t) -> do
        index <- quadruplizeExpr expr
        addr <- getRegister rawInt
        res <- getRegister t
        addQuad $ Load arr addr
        addQuad $ LoadIndir addr 8 (Just index) 8 res
        return res

quadruplizeExpr (EVar pos ident)          = do
    s <- gets localStore
    f <- gets currentFun
    case M.lookup ident (varToMem s) of
      Nothing -> undefined
      Just mem@(QIndex i t) -> do
        reg <- getRegister t
        addQuad $ Load mem reg
        return reg

quadruplizeExpr (ELitInt pos integer)     = do
    reg <- getRegister rawInt
    addQuad $ MovV (qinteger integer) reg
    return reg

quadruplizeExpr (ELitTrue pos)            = do
    reg <- getRegister rawBool
    addQuad $ MovV (QBool True) reg
    return reg

quadruplizeExpr (ELitFalse pos)           = do
    reg <- getRegister rawBool
    addQuad $ MovV (QBool False) reg
    return reg

quadruplizeExpr (EString pos s)           = do
    reg <- getRegister rawStr
    label <- addString s
    addQuad $ LoadLbl label reg
    return reg

quadruplizeExpr app@(EApp pos (Ident ident) exprs) = do
    args <- mapM quadruplizeExpr exprs
    p <- gets qprogram
    vc <- isVoidCall app
    if vc then do
        addQuad $ VoidCall ident args
        return $ Register (-1) rawVoid
    else do
        case M.lookup ident (funcs p) of
            Nothing -> case M.lookup ident predefinedFuncs of
                Nothing -> undefined
                Just t -> do
                        reg <- getRegister t
                        addQuad $ Call ident args reg
                        return reg
            Just f -> do
                    reg <- getRegister $ raw (ret f)
                    addQuad $ Call ident args reg
                    return reg


quadruplizeExpr (Abs.Neg pos expr)        = do
    res <- getRegister rawInt
    reg <- quadruplizeExpr expr
    addQuad $ QD.Neg reg res
    return res

quadruplizeExpr (Abs.Not pos expr)        = do
    res <- getRegister rawBool
    reg <- quadruplizeExpr expr
    addQuad $ QD.Not reg res
    return res

quadruplizeExpr (EMul pos expr1 op expr2) = do
    reg <- getRegister rawInt
    r1 <- quadruplizeExpr expr1
    r2 <- quadruplizeExpr expr2
    case op of
      Times _   -> addQuad $ Mul r1 r2 reg
      Abs.Div _ -> addQuad $ QD.Div r1 r2 reg
      Abs.Mod _ -> addQuad $ QD.Mod r1 r2 reg
    return reg

quadruplizeExpr (EAdd pos expr1 op expr2) = do
    r1 <- quadruplizeExpr expr1
    r2 <- quadruplizeExpr expr2
    case op of
      Plus _ -> if regType r1 == rawStr then do
                    reg <- getRegister rawStr
                    addQuad $ Call "__concat" [r1, r2] reg
                    return reg
                else do
                    reg <- getRegister rawInt
                    addQuad $ QD.Add r1 r2 reg
                    return reg
      Minus _ -> do
        reg <- getRegister rawInt
        addQuad $ QD.Sub r1 r2 reg
        return reg

quadruplizeExpr (ERel pos expr1 op expr2) = do
    r1 <- quadruplizeExpr expr1
    r2 <- quadruplizeExpr expr2
    if regType r1 == rawStr then do
        reg <- getRegister rawStr
        quadruplizeStringRel reg r1 r2 op
    else do
        reg <- getRegister rawInt
        phiLabel <- getLabel
        afterLabel <- getLabel
        addQuad $ Cmp r1 r2
        case op of
            LTH _ -> addQuad $ PhiJl  phiLabel afterLabel reg
            LE _  -> addQuad $ PhiJle phiLabel afterLabel reg
            GTH _ -> addQuad $ PhiJg  phiLabel afterLabel reg
            GE _  -> addQuad $ PhiJge phiLabel afterLabel reg
            EQU _ -> addQuad $ PhiJe  phiLabel afterLabel reg
            NE _  -> addQuad $ PhiJne phiLabel afterLabel reg
        return reg

quadruplizeExpr (EAnd pos expr1 expr2)    = do
    reg <- getRegister rawBool
    phiLabel <- getLabel
    onPhiLabel <- getLabel
    afterLabel <- getLabel
    left <- quadruplizeExpr expr1
    f <- getRegister rawBool
    addQuad $ MovV (QBool False) f
    addQuad $ Cmp left f
    addQuad $ Je onPhiLabel
    right <- quadruplizeExpr expr2
    f2 <- getRegister rawBool
    addQuad $ MovV (QBool False) f2
    addQuad $ Cmp right f2
    addQuad $ Jne onPhiLabel
    addQuad $ Label onPhiLabel
    addQuad $ PhiJne phiLabel afterLabel reg
    return reg


quadruplizeExpr (EOr pos expr1 expr2)     = do
    reg <- getRegister rawBool
    onPhiLabel <- getLabel
    phiLabel <- getLabel
    afterLabel <- getLabel
    left <- quadruplizeExpr expr1
    f <- getRegister rawBool
    addQuad $ MovV (QBool False) f
    addQuad $ Cmp left f
    addQuad $ Jne onPhiLabel
    right <- quadruplizeExpr expr2
    f2 <- getRegister rawBool
    addQuad $ MovV (QBool False) f2
    addQuad $ Cmp right f2
    addQuad $ Label onPhiLabel
    addQuad $ PhiJne phiLabel afterLabel reg
    return reg


quadruplizeStringRel :: Register -> Register -> Register -> RelOp -> QuadruplesState Register
quadruplizeStringRel reg r1 r2 op = do
    case op of
        EQU ma -> addQuad $ Call "__equals" [r1, r2] reg
        NE ma -> do
                 addQuad $ Call "__notequals" [r1, r2] reg
        _ -> undefined
    return reg

loadArgs :: [Arg] -> QuadruplesState ()
loadArgs [] = return ()
loadArgs ((Arg _ t ident):as) = do
    mem@(QIndex i _) <- store ident t
    addQuad $ LoadArg i mem
    loadArgs as

isVoidCall :: Expr -> QuadruplesState Bool
isVoidCall q = case q of
    EApp _ (Ident i) _ -> do
        p <- gets qprogram
        case M.lookup i (funcs p) of
          Nothing -> case M.lookup i predefinedFuncs of
            Nothing -> undefined
            Just t  -> return $ raw t == rawVoid
          Just f -> return $ raw (ret f) == rawVoid

    _ -> undefined
--------------------------------------------------------------------------------------


maxLocalsBlock :: Block -> Int -> QuadruplesState Int
maxLocalsBlock (Block _ []) l = do
    return 0
maxLocalsBlock (Block pos (s:ss)) l = do
    locals <- maxLocalsStmt s l
    modify (`setMaxLocals` (l + locals))
    maxLocalsBlock (Block pos ss) (l + locals)

maxLocalsStmt :: Stmt -> Int -> QuadruplesState Int
maxLocalsStmt (Empty pos) _                     = return 0
maxLocalsStmt (BStmt pos block) l               = maxLocalsBlock block l
maxLocalsStmt (Decl pos t items) _              = return $ length items
maxLocalsStmt (Ass pos ident expr) _            = return 0
maxLocalsStmt (Incr pos ident) _                = return 0
maxLocalsStmt (Decr pos ident) _                = return 0
maxLocalsStmt (Abs.Ret pos expr) _              = return 0
maxLocalsStmt (VRet pos) _                      = return 0
maxLocalsStmt (Cond pos expr stmt) l            = maxLocalsBlock (Block Nothing [stmt]) l
maxLocalsStmt (CondElse pos expr stmt1 stmt2) l = do
    locals1 <- maxLocalsBlock (Block Nothing [stmt1]) l
    locals2 <- maxLocalsBlock (Block Nothing [stmt2]) l
    return 0
maxLocalsStmt (While pos expr stmt) l           = maxLocalsBlock (Block Nothing [stmt]) l
maxLocalsStmt (For pos t ident1 ident2 stmt) l  = do
    modify (`setMaxLocals` (l + 2))
    maxLocalsBlock (Block Nothing [stmt]) (l + 2)
maxLocalsStmt (SExp pos expr) _                 = return 0
