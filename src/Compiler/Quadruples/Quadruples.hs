module Compiler.Quadruples.Quadruples where

import           Compiler.Quadruples.Data
import           Compiler.Quadruples.Predata    (initPreprocessS)
import           Compiler.Quadruples.Preprocess (preprocess)
import           Control.Monad.State            (MonadIO (liftIO),
                                                 MonadState (get, put),
                                                 MonadTrans (lift), evalStateT,
                                                 gets, modify, when)
import           Control.Monad.Trans.Except     (ExceptT)
import qualified Data.Map                       as M
import           Syntax.AbsLattepp              as Abs
import           Utils                          (Raw (raw), rawStr, rawVoid)

quadruplize :: Program -> QuadruplesState QProgram
quadruplize p@(Program _ defs) = do
    preprocessed <- lift $ evalStateT (preprocess p) initPreprocessS
    modify (`setPreprocessing` preprocessed)
    mapM_ quadruplizeTopDef defs
    prog <- gets qprogram
    --liftIO $ print $ prog
    gets qprogram


quadruplizeTopDef :: TopDef -> QuadruplesState ()
quadruplizeTopDef (FnDef pos ret (Ident ident) args block)  = do
    l <- maxLocalsBlock block 0
    mx <- gets maxLocals
    let toReserve = mx+ length args
    modify resetMaxLocals
    modify (`addFun` QFun ident ret toReserve [])
    modify (`setStore` initLocalStore toReserve)
    loadArgs args
    quadruplizeBlock block
    when (raw ret == rawVoid) $ addQuad Vret

quadruplizeTopDef (ClassDef pos ident block)        = return () -- todo
quadruplizeTopDef (ExtClassDef pos ident ext block) = return () -- todo

quadruplizeBlock :: Block -> QuadruplesState ()
quadruplizeBlock (Block pos stmts) = do
    st <- get
    storeEnv st (mapM_ quadruplizeStmt stmts)

quadruplizeClassBlock :: ClassBlock -> QuadruplesState ()
quadruplizeClassBlock (ClassBlock pos stmts) = return ()

quadruplizeClassStmt :: ClassStmt -> QuadruplesState ()
quadruplizeClassStmt (ClassEmpty pos)                       = return ()
quadruplizeClassStmt (ClassDecl pos t items)                = return ()
quadruplizeClassStmt (ClassMethod pos ret ident args block) = return ()

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
      ArrId _ id ex -> undefined
      AttrId _ ex ex' -> undefined

quadruplizeStmt (Incr pos ident)                = do
    st <- gets localStore
    case ident of
      Id _ id -> case M.lookup id (varToMem st) of
                    Nothing -> undefined
                    Just mem -> do
                        addQuad $ Inc mem
      ArrId ma id ex -> undefined
      AttrId ma ex ex' -> undefined


quadruplizeStmt (Decr pos ident)                = do
    st <- gets localStore
    case ident of
      Id _ id -> case M.lookup id (varToMem st) of
                    Nothing -> undefined
                    Just mem -> do
                        addQuad $ Dec mem
      ArrId ma id ex -> undefined
      AttrId ma ex ex' -> undefined

quadruplizeStmt (Abs.Ret pos expr)              = do
    reg <- quadruplizeExpr expr
    addQuad $ Compiler.Quadruples.Data.Ret reg

quadruplizeStmt (VRet pos)                      = addQuad Vret

quadruplizeStmt (Cond pos expr stmt)            = do
    falseLabel <- getLabel
    ifReg <- quadruplizeExpr expr
    zero <- getRegister
    addQuad $ MovV (QInt 0) zero
    addQuad $ Cmp ifReg zero
    addQuad $ Je falseLabel
    quadruplizeBlock (Block Nothing [stmt])
    addQuad $ Label falseLabel

quadruplizeStmt (CondElse pos expr stmt1 stmt2) = do
    elseLabel <- getLabel
    afterLabel <- getLabel
    ifReg <- quadruplizeExpr expr
    zero <- getRegister
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
    zero <- getRegister
    addQuad $ Jmp condLabel
    addQuad $ Label bodyLabel
    quadruplizeBlock (Block Nothing [stmt])
    addQuad $ Label condLabel
    condReg <- quadruplizeExpr expr
    addQuad $ MovV (QInt 0) zero
    addQuad $ Cmp condReg zero
    addQuad $ Jne bodyLabel

quadruplizeStmt (For pos t ident1 ident2 stmt)  = return ()

quadruplizeStmt (SExp pos expr)                 = do
    quadruplizeExpr expr
    return ()

quadruplizeItem :: Type -> Item -> QuadruplesState ()
quadruplizeItem t (NoInit pos ident)    = do
    reg <- getRegister
    addQuad $ MovV (QInt 0) reg
    mem <- store ident t
    addQuad $ Store reg mem

quadruplizeItem t (Init pos ident expr) = do
    reg <- quadruplizeExpr expr
    mem <- store ident t
    addQuad $ Store reg mem


quadruplizeExpr :: Expr -> QuadruplesState Register
quadruplizeExpr (ECast pos ident expr)    = return $ Register 0 False

quadruplizeExpr (ECastPrim pos t expr)    = return $ Register 0 False

quadruplizeExpr (ENewObject pos ident)    = return $ Register 0 False

quadruplizeExpr (ENewArr pos t expr)      = return $ Register 0 False

quadruplizeExpr (ENull pos)               = do
    reg <- getRegister
    addQuad $ MovV (QInt 0) reg
    return reg

quadruplizeExpr (EObject pos expr1 expr2) = return $ Register 0 False

quadruplizeExpr (EArr pos ident expr)     = return $ Register 0 False

quadruplizeExpr (EVar pos ident)          = do
    s <- gets localStore
    case M.lookup ident (varToMem s) of
      Nothing -> undefined
      Just mem@(QIndex i str) -> do
        reg <- getRegister
        addQuad $ Load mem reg
        return $ if str then stringReg reg else reg

quadruplizeExpr (ELitInt pos integer)     = do
    reg <- getRegister
    addQuad $ MovV (qinteger integer) reg
    return reg

quadruplizeExpr (ELitTrue pos)            = do
    reg <- getRegister
    addQuad $ MovV (QBool True) reg
    return reg

quadruplizeExpr (ELitFalse pos)           = do
    reg <- getRegister
    addQuad $ MovV (QBool False) reg
    return reg

quadruplizeExpr (EString pos s)           = do
    reg <- getRegister
    label <- addString s
    let strReg = stringReg reg
    addQuad $ LoadLbl label strReg
    return strReg

quadruplizeExpr (EApp pos (Ident ident) exprs)    = do
    reg <- getRegister
    args <- mapM quadruplizeExpr exprs
    p <- gets qprogram
    --liftIO $ print $ funcs p
    case M.lookup ident (funcs p) of
      Nothing -> case M.lookup ident predefinedFuncs of
        Nothing -> undefined
        Just t -> do
            if raw t == rawVoid then
                addQuad $ VoidCall ident args
            else
                addQuad $ Call ident args reg
            if raw t == rawStr then return $ stringReg reg else return reg
      Just f -> do
        if raw (ret f) == rawVoid then
            addQuad $ VoidCall ident args
        else
            addQuad $ Call ident args reg
        if raw (ret f) == rawStr then return $ stringReg reg else return reg


quadruplizeExpr (Abs.Neg pos expr)        = do
    reg <- quadruplizeExpr expr
    addQuad $ Compiler.Quadruples.Data.Neg reg
    return reg

quadruplizeExpr (Abs.Not pos expr)        = do
    reg <- quadruplizeExpr expr
    addQuad $ Compiler.Quadruples.Data.Not reg
    return reg

quadruplizeExpr (EMul pos expr1 op expr2) = do
    reg <- getRegister
    r1 <- quadruplizeExpr expr1
    r2 <- quadruplizeExpr expr2
    case op of
      Times _   -> addQuad $ Mul r1 r2 reg
      Abs.Div _ -> addQuad $ Compiler.Quadruples.Data.Div r1 r2 reg
      Abs.Mod _ -> addQuad $ Compiler.Quadruples.Data.Mod r1 r2 reg
    return reg

quadruplizeExpr (EAdd pos expr1 op expr2) = do
    reg <- getRegister
    r1 <- quadruplizeExpr expr1
    r2 <- quadruplizeExpr expr2
    case op of
      Plus _ -> if isStringReg r1 then do
                    addQuad $ Call "__concat" [r1, r2] reg --defining function with number at the begining protects us from name conflicts | typechecking allows us to check only one register
                else
                    addQuad $ Compiler.Quadruples.Data.Add r1 r2 reg
      Minus _ -> addQuad $ Compiler.Quadruples.Data.Sub r1 r2 reg
    return $ if isStringReg r1 then stringReg reg else reg

quadruplizeExpr (ERel pos expr1 op expr2) = do
    reg <- getRegister
    r1 <- quadruplizeExpr expr1
    r2 <- quadruplizeExpr expr2
    if isStringReg r1 then do
        quadruplizeStringRel reg r1 r2 op
    else do
        truey <- getLabel
        afterLabel <- getLabel
        addQuad $ Cmp r1 r2
        case op of
            LTH _ -> addQuad $ Jl truey
            LE _  -> addQuad $ Jle truey
            GTH _ -> addQuad $ Jg truey
            GE _  -> addQuad $ Jge truey
            EQU _ -> addQuad $ Je truey
            NE _  -> addQuad $ Jne truey
        addQuad $ MovV (QBool False) reg
        addQuad $ Jmp afterLabel
        addQuad $ Label truey
        addQuad $ MovV (QBool True) reg
        addQuad $ Label afterLabel
        return reg

quadruplizeExpr (EAnd pos expr1 expr2)    = do
    reg <- getRegister
    falsy <- getLabel
    truey <- getLabel
    afterLabel <- getLabel
    left <- quadruplizeExpr expr1
    f <- getRegister
    addQuad $ MovV (QBool False) f
    addQuad $ Cmp left f
    addQuad $ Je falsy
    right <- quadruplizeExpr expr2
    f2 <- getRegister
    addQuad $ MovV (QBool False) f2
    addQuad $ Cmp right f2
    addQuad $ Jne truey
    addQuad $ Label falsy
    addQuad $ MovV (QBool False) reg
    addQuad $ Jmp afterLabel
    addQuad $ Label truey
    addQuad $ MovV (QBool True) reg
    addQuad $ Label afterLabel
    return reg


quadruplizeExpr (EOr pos expr1 expr2)     = do
    reg <- getRegister
    falsy <- getLabel
    truey <- getLabel
    afterLabel <- getLabel
    left <- quadruplizeExpr expr1
    f <- getRegister
    addQuad $ MovV (QBool False) f
    addQuad $ Cmp left f
    addQuad $ Jne truey
    right <- quadruplizeExpr expr2
    f2 <- getRegister
    addQuad $ MovV (QBool False) f2
    addQuad $ Cmp right f2
    addQuad $ Jne truey
    addQuad $ Label falsy
    addQuad $ MovV (QBool False) reg
    addQuad $ Jmp afterLabel
    addQuad $ Label truey
    addQuad $ MovV (QBool True) reg
    addQuad $ Label afterLabel
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
    mem <- store ident t
    addQuad $ LoadArg mem --due to how store works arg index and mem index are the same
    loadArgs as

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

