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
import           Utils                          (Raw (raw), rawVoid)


quadruplize :: Program -> QuadruplesState QProgram
quadruplize p@(Program _ defs) = do
    preprocessed <- lift $ evalStateT (preprocess p) initPreprocessS
    modify (`setPreprocessing` preprocessed)
    mapM_ quadruplizeTopDef defs
    prog <- gets qprogram
    liftIO $ print prog
    gets qprogram


quadruplizeTopDef :: TopDef -> QuadruplesState ()
quadruplizeTopDef (FnDef pos ret (Ident ident) args block)  = do
    l <- maxLocalsBlock block 0
    mx <- gets maxLocals
    let toReserve = mx+ length args
    liftIO $ print toReserve
    modify resetMaxLocals
    modify (`addFun` QFun ident toReserve []) --possibly add FLabel
    mapM_ (addQuad . (Alloc . QIndex)) [0..toReserve]
    modify (`setStore` initLocalStore toReserve)
    loadArgs args
    start <- getLabel
    addQuad $ Label start
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
    mapM_ quadruplizeItem items

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
    res <- getRegister
    st <- gets localStore
    case ident of
      Id _ id -> case M.lookup id (varToMem st) of
                    Nothing -> undefined
                    Just mem -> do
                        addQuad $ Inc mem res
      ArrId ma id ex -> undefined
      AttrId ma ex ex' -> undefined


quadruplizeStmt (Decr pos ident)                = do
    res <- getRegister
    st <- gets localStore
    case ident of
      Id _ id -> case M.lookup id (varToMem st) of
                    Nothing -> undefined
                    Just mem -> do
                        addQuad $ Dec mem res
      ArrId ma id ex -> undefined
      AttrId ma ex ex' -> undefined

quadruplizeStmt (Abs.Ret pos expr)              = do
    reg <- quadruplizeExpr expr
    addQuad $ Compiler.Quadruples.Data.Ret reg

quadruplizeStmt (VRet pos)                      = addQuad Vret

quadruplizeStmt (Cond pos expr stmt)            = do
    trueLabel <- getLabel
    falseLabel <- getLabel
    ifReg <- quadruplizeExpr expr
    zero <- getRegister
    addQuad $ MovV (QInt 0) zero
    addQuad $ Cmp ifReg zero
    addQuad $ Je falseLabel trueLabel
    addQuad $ Label trueLabel
    quadruplizeBlock (Block Nothing [stmt])
    addQuad $ Label falseLabel

quadruplizeStmt (CondElse pos expr stmt1 stmt2) = do
    ifLabel <- getLabel
    elseLabel <- getLabel
    afterLabel <- getLabel
    ifReg <- quadruplizeExpr expr
    zero <- getRegister
    addQuad $ MovV (QInt 0) zero
    addQuad $ Cmp ifReg zero
    addQuad $ Je elseLabel ifLabel
    addQuad $ Label ifLabel
    quadruplizeBlock (Block Nothing [stmt1])
    addQuad $ Jmp afterLabel
    addQuad $ Label elseLabel
    quadruplizeBlock (Block Nothing [stmt2])
    addQuad $ Label afterLabel



quadruplizeStmt (While pos expr stmt)           = do
    bodyLabel <- getLabel
    condLabel <- getLabel
    afterLabel <- getLabel
    zero <- getRegister
    addQuad $ Jmp condLabel
    addQuad $ Label bodyLabel
    quadruplizeBlock (Block Nothing [stmt])
    addQuad $ Label condLabel
    condReg <- quadruplizeExpr expr
    addQuad $ MovV (QInt 0) zero
    addQuad $ Cmp condReg zero
    addQuad $ Je afterLabel bodyLabel
    addQuad $ Label afterLabel



quadruplizeStmt (For pos t ident1 ident2 stmt)  = return ()

quadruplizeStmt (SExp pos expr)                 = do
    quadruplizeExpr expr
    return ()

quadruplizeItem :: Item -> QuadruplesState ()
quadruplizeItem (NoInit pos ident)    = do
    reg <- getRegister
    addQuad $ MovV (QInt 0) reg
    mem <- store ident
    addQuad $ Store reg mem

quadruplizeItem (Init pos ident expr) = do
    reg <- quadruplizeExpr expr
    mem <- store ident
    addQuad $ Store reg mem


quadruplizeExpr :: Expr -> QuadruplesState Register
quadruplizeExpr (ECast pos ident expr)    = return $ Register 0
quadruplizeExpr (ECastPrim pos t expr)    = return $ Register 0
quadruplizeExpr (ENewObject pos ident)    = return $ Register 0
quadruplizeExpr (ENewArr pos t expr)      = return $ Register 0
quadruplizeExpr (ENull pos)               = return $ Register 0
quadruplizeExpr (EObject pos expr1 expr2) = return $ Register 0
quadruplizeExpr (EArr pos ident expr)     = return $ Register 0
quadruplizeExpr (EVar pos ident)          = return $ Register 0
quadruplizeExpr (ELitInt pos integer)     = return $ Register 0
quadruplizeExpr (ELitTrue pos)            = return $ Register 0
quadruplizeExpr (ELitFalse pos)           = return $ Register 0
quadruplizeExpr (EString pos s)           = return $ Register 0
quadruplizeExpr (EApp pos ident exprs)    = return $ Register 0
quadruplizeExpr (Abs.Neg pos expr)        = return $ Register 0
quadruplizeExpr (Abs.Not pos expr)        = return $ Register 0
quadruplizeExpr (EMul pos expr1 op expr2) = return $ Register 0
quadruplizeExpr (EAdd pos expr1 op expr2) = return $ Register 0
quadruplizeExpr (ERel pos expr1 op expr2) = return $ Register 0
quadruplizeExpr (EAnd pos expr1 expr2)    = return $ Register 0
quadruplizeExpr (EOr pos expr1 expr2)     = return $ Register 0



loadArgs :: [Arg] -> QuadruplesState ()
loadArgs [] = return ()
loadArgs ((Arg _ _ ident):as) = do
    mem <- store ident
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

