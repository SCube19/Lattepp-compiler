module Compiler.Quadruples.Quadruples where

import           Compiler.Quadruples.Data
import           Compiler.Quadruples.Predata    (initPreprocessS)
import           Compiler.Quadruples.Preprocess (preprocess)
import           Control.Monad.State            (MonadIO (liftIO),
                                                 MonadState (get, put),
                                                 MonadTrans (lift), evalStateT,
                                                 gets, modify, when)
import           Control.Monad.Trans.Except     (ExceptT)
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
    mapM_ ((\local -> modify (`addQuad` local)) . (Alloc . QIndex)) [0..toReserve]
    modify (`setStore` initLocalStore toReserve)
    loadArgs args
    start <- getLabel
    modify (`addQuad` Label start)
    quadruplizeBlock block
    when (raw ret == rawVoid) $ modify (`addQuad` Vret)

quadruplizeTopDef (ClassDef pos ident block)        = return () -- todo
quadruplizeTopDef (ExtClassDef pos ident ext block) = return () -- todo

quadruplizeBlock :: Block -> QuadruplesState ()
quadruplizeBlock (Block pos stmts) = return ()

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
quadruplizeStmt (BStmt pos block)               = return ()
quadruplizeStmt (Decl pos t items)              = return ()
quadruplizeStmt (Ass pos ident expr)            = return ()
quadruplizeStmt (Incr pos ident)                = return ()
quadruplizeStmt (Decr pos ident)                = return ()
quadruplizeStmt (Abs.Ret pos expr)              = return ()
quadruplizeStmt (VRet pos)                      = return ()
quadruplizeStmt (Cond pos expr stmt)            = return ()
quadruplizeStmt (CondElse pos expr stmt1 stmt2) = return ()
quadruplizeStmt (While pos expr stmt)           = return ()
quadruplizeStmt (For pos t ident1 ident2 stmt)  = return ()
quadruplizeStmt (SExp pos expr)                 = return ()

quadruplizeItem :: Type -> Item -> QuadruplesState ()
quadruplizeItem t (NoInit pos ident)    = return ()
quadruplizeItem t (Init pos ident expr) = return ()

quadruplizeExpr :: Expr -> QuadruplesState ()
quadruplizeExpr (ECast pos ident expr)    = return ()
quadruplizeExpr (ECastPrim pos t expr)    = return ()
quadruplizeExpr (ENewObject pos ident)    = return ()
quadruplizeExpr (ENewArr pos t expr)      = return ()
quadruplizeExpr (ENull pos)               = return ()
quadruplizeExpr (EObject pos expr1 expr2) = return ()
quadruplizeExpr (EArr pos ident expr)     = return ()
quadruplizeExpr (EVar pos ident)          = return ()
quadruplizeExpr (ELitInt pos integer)     = return ()
quadruplizeExpr (ELitTrue pos)            = return ()
quadruplizeExpr (ELitFalse pos)           = return ()
quadruplizeExpr (EString pos s)           = return ()
quadruplizeExpr (EApp pos ident exprs)    = return ()
quadruplizeExpr (Abs.Neg pos expr)        = return ()
quadruplizeExpr (Abs.Not pos expr)        = return ()
quadruplizeExpr (EMul pos expr1 op expr2) = return ()
quadruplizeExpr (EAdd pos expr1 op expr2) = return ()
quadruplizeExpr (ERel pos expr1 op expr2) = return ()
quadruplizeExpr (EAnd pos expr1 expr2)    = return ()
quadruplizeExpr (EOr pos expr1 expr2)     = return ()



loadArgs :: [Arg] -> QuadruplesState ()
loadArgs [] = return ()
loadArgs ((Arg _ _ ident):as) = do
    curr <- gets localStore
    let (s, mem) = store curr ident
    modify (`setStore` s)
    modify (`addQuad` LoadArg (QIndex mem)) --due to how store works arg index and mem index are the same
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

