module Optimizer.Optimizer where

import Syntax.AbsLattepp
import Optimizer.Utils
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Optimizer.Data (OptimizerState, OptimizerS (consts), initOptimizerS, localOptimizerEnv, castInteger, castString, castBool, putConst, Value (IntV, StrV, BoolV))
import Control.Monad.Trans.State (StateT(runStateT), evalStateT, get, gets, put)
import qualified Data.Map as M
import Utils (rawBool, rawInt, rawStr, Raw (raw), rawExtIdent)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Debug.Trace

optimize :: Program -> ExceptT String IO Program
optimize (Program pos defs) = do
    newDefs <- evalStateT (mapM optimizeTopDef defs) initOptimizerS
    return $ Program pos newDefs

optimizeTopDef :: TopDef -> OptimizerState TopDef
optimizeTopDef (FnDef pos ret ident args block) = do
    newBlock <- localOptimizerEnv initOptimizerS (optimizeBlock block)
    return $ FnDef pos ret ident args newBlock

optimizeTopDef (ClassDef pos ident block) = do
    newBlock <- optimizeClassBlock block
    return $ ClassDef pos ident newBlock

optimizeTopDef (ExtClassDef pos ident ext block) = do
    newBlock <- optimizeClassBlock block
    return $ ExtClassDef pos ident ext newBlock

optimizeBlock :: Block -> OptimizerState Block
optimizeBlock (Block pos stmts) = do
    st <- get
    newStmts <- localOptimizerEnv st (mapM optimizeStmt stmts)
    return $ Block pos newStmts

optimizeClassBlock :: ClassBlock -> OptimizerState ClassBlock
optimizeClassBlock (ClassBlock pos stmts) = do
    newStmts <- mapM optimizeClassStmt stmts
    return $ ClassBlock pos newStmts

optimizeClassStmt :: ClassStmt -> OptimizerState ClassStmt
optimizeClassStmt (ClassEmpty pos) = return $ ClassEmpty pos

optimizeClassStmt (ClassDecl pos t items) = do
    return $ ClassDecl pos t items

optimizeClassStmt (ClassMethod pos ret ident args block) = do
    newBlock <- optimizeBlock block
    return $ ClassMethod pos ret ident args newBlock

optimizeExtIdent :: ExtIdent -> OptimizerState ExtIdent
optimizeExtIdent (Id pos ident) = return $ Id pos ident

optimizeExtIdent (ArrId pos ident expr) = do
    newExpr <- optimizeExpr expr
    return $ ArrId pos ident newExpr

optimizeExtIdent (AttrId pos expr1 expr2) = do
    newExpr1 <- case expr1 of
            EVar _ _ -> return expr1
            _ -> optimizeExpr expr1
    newExpr2 <- case expr2 of
            EVar _ _ -> return expr2
            _ -> optimizeExpr expr2
    return $ AttrId pos newExpr1 newExpr2

optimizeStmt :: Stmt -> OptimizerState Stmt
optimizeStmt (Empty pos) = return $ Empty pos

optimizeStmt (BStmt pos block) = do
    newBlock <- optimizeBlock block
    return $ BStmt pos newBlock

optimizeStmt (Decl pos t items) = do
    newItems <- mapM (optimizeItem t) items
    return $ Decl pos t newItems

optimizeStmt (Ass pos ident expr) = do
    newExpr <- optimizeExpr expr
    newIdent <- optimizeExtIdent ident
    st <- get
    case newIdent of
      Id _ id -> case newExpr of
        ELitInt _ val -> put $ putConst st id (Int Nothing, IntV val)
        ELitTrue _ -> put $ putConst st id (Bool Nothing, BoolV True)
        ELitFalse _ -> put $ putConst st id (Bool Nothing, BoolV False)
        EString _ s -> put $ putConst st id (Str Nothing, StrV s)
        _ -> return ()
      ArrId {} -> return ()
      AttrId {} -> return ()
    newSt <- get
    return $ Ass pos ident newExpr

optimizeStmt (Incr pos ident) = do
    newIdent <- optimizeExtIdent ident
    return $ Incr pos newIdent

optimizeStmt (Decr pos ident) = do
    newIdent <- optimizeExtIdent ident
    return $ Decr pos newIdent

optimizeStmt (Ret pos expr) = do
    newExpr <- optimizeExpr expr
    return $ Ret pos expr

optimizeStmt (VRet pos) = return $ VRet pos

optimizeStmt (Cond pos expr stmt) = do
    newExpr <- optimizeExpr expr
    st <- get
    newStmt <- localOptimizerEnv st (optimizeStmt stmt)
    return $ Cond pos newExpr newStmt

optimizeStmt (CondElse pos expr stmt1 stmt2) = do
    newExpr <- optimizeExpr expr
    st <- get
    newStmt1 <- localOptimizerEnv st (optimizeStmt stmt1)
    newStmt2 <- localOptimizerEnv st (optimizeStmt stmt2)
    return $ CondElse pos newExpr newStmt1 newStmt2

optimizeStmt (While pos expr stmt) = do
    newExpr <- optimizeExpr expr
    st <- get
    newStmt <- localOptimizerEnv st (optimizeStmt stmt)
    return $ While pos newExpr newStmt

optimizeStmt (For pos t ident1 ident2 stmt) = do
    st <- get
    newStmt <- localOptimizerEnv st (optimizeStmt stmt)
    newIdent <- optimizeExtIdent ident2
    return $ For pos t ident1 newIdent newStmt

optimizeStmt (SExp pos expr) = do
    newExpr <- optimizeExpr expr
    return $ SExp pos newExpr

optimizeItem :: Type -> Item -> OptimizerState Item
optimizeItem  t (NoInit pos ident) = do
    st <- get
    case t of
      Primitive _ pt -> do
        put $ putConst st ident (raw pt, case pt of
                                                Int _ -> IntV 0
                                                Str _ -> StrV ""
                                                Bool _ -> BoolV False
                                                Void _ -> undefined)
        newSt <- get
        return $ NoInit pos ident
      _ -> return $ NoInit pos ident

optimizeItem t (Init pos ident expr) = do
    newExpr <- optimizeExpr expr
    st <- get
    case newExpr of
        ELitInt _ val -> put $ putConst st ident (Int Nothing, IntV val)
        ELitTrue _ -> put $ putConst st ident (Bool Nothing, BoolV True)
        ELitFalse _ -> put $ putConst st ident (Bool Nothing, BoolV False)
        EString _ s -> put $ putConst st ident (Str Nothing, StrV s)
        _ -> return ()
    newSt <- get
    return $ Init pos ident newExpr

optimizeExpr :: Expr -> OptimizerState Expr
optimizeExpr(ECast pos ident expr) = do
    newExpr <- optimizeExpr expr
    return $ ECast pos ident newExpr

optimizeExpr(ECastPrim pos t expr) = do
    newExpr <- optimizeExpr expr
    return $ ECastPrim pos t newExpr

optimizeExpr(ENewObject pos ident) = return $ ENewObject pos ident

optimizeExpr(ENewArr pos t expr) = do
    newExpr <- optimizeExpr expr
    return $ ENewArr pos t newExpr

optimizeExpr(ENull pos) = return $ ENull pos

optimizeExpr(EObject pos expr1 expr2) = do
    newExpr1 <- optimizeExpr expr1
    newExpr2 <- optimizeExpr expr2
    return $ EObject pos newExpr1 newExpr2

optimizeExpr(EArr pos ident expr) = do
    newExpr <- optimizeExpr expr
    return $ EArr pos ident newExpr

optimizeExpr(EVar pos ident) = do
    c <- gets consts
    case M.lookup ident c of
      Nothing -> return $ EVar pos ident
      Just (t, val) -> case t of
        Int _ -> return $ ELitInt pos (castInteger val)
        Str _ -> return $ EString pos (castString val)
        Bool _ -> return $ if castBool val then ELitTrue pos else ELitFalse pos
        _ -> undefined

optimizeExpr(ELitInt pos integer) = return $ ELitInt pos integer

optimizeExpr(ELitTrue pos) = return $ ELitTrue pos

optimizeExpr(ELitFalse pos) = return $ ELitFalse pos

optimizeExpr(EApp pos ident exprs) = do
    newExprs <- mapM optimizeExpr exprs
    return $ EApp pos ident newExprs

optimizeExpr(EString pos string) = return $ EString pos string

optimizeExpr(Neg pos expr) = do
    newExpr <- optimizeExpr expr
    case newExpr of
        ELitInt _ val -> return $ ELitInt pos (-val)
        newExpr -> return $ Neg pos newExpr

optimizeExpr(Not pos expr) = do
    newExpr <- optimizeExpr expr
    case newExpr of
        ELitTrue _ -> return $ ELitFalse pos
        ELitFalse _ -> return $ ELitTrue pos
        newExpr -> return $ Neg pos newExpr

optimizeExpr (EMul pos expr1 op expr2) = do
    newExpr1 <- optimizeExpr expr1
    newExpr2 <- optimizeExpr expr2
    if isELitInt newExpr1 && isELitInt newExpr2 then
        return $ ELitInt pos (case op of
            Times _ -> extractInt newExpr1 * extractInt newExpr2
            Div _ -> extractInt newExpr1 `div` extractInt newExpr2
            Mod _ -> extractInt newExpr1 `mod` extractInt newExpr2)
    else
        return $ EMul pos newExpr1 op newExpr2

optimizeExpr (EAdd pos expr1 op expr2) = do
    newExpr1 <- optimizeExpr expr1
    newExpr2 <- optimizeExpr expr2
    if isELitInt newExpr1 && isELitInt newExpr2 then
        return $ ELitInt pos (case op of
            Plus _ -> extractInt newExpr1 + extractInt newExpr2
            Minus _ -> extractInt newExpr1 - extractInt newExpr2)
    else if isEString newExpr1 && isEString newExpr2 then
        return $ EString pos (case op of
           Plus _ -> extractString newExpr1 ++ extractString newExpr2
           Minus _ -> undefined
        )
    else
        return $ EAdd pos newExpr1 op newExpr2

optimizeExpr(ERel pos expr1 op expr2) = do
    newExpr1 <- optimizeExpr expr1
    newExpr2 <- optimizeExpr expr2
    if isELitInt newExpr1 && isELitInt newExpr2 then do
        let result = (case op of
                LTH _ -> extractInt newExpr1 < extractInt newExpr2
                LE _ -> extractInt newExpr1 <= extractInt newExpr2
                GTH _ -> extractInt newExpr1 > extractInt newExpr2
                GE _ -> extractInt newExpr1 >= extractInt newExpr2
                EQU _ -> extractInt newExpr1 == extractInt newExpr2
                NE _ -> extractInt newExpr1 /= extractInt newExpr2)
        if result then
            return $ ELitTrue pos
        else
            return $ ELitFalse pos
    else
        return $ ERel pos newExpr1 op newExpr2

optimizeExpr(EAnd pos expr1 expr2) = do
    newExpr1 <- optimizeExpr expr1
    if isELitBool newExpr1 then
        case newExpr1 of
            ELitFalse _ -> return $ ELitFalse pos
            ELitTrue _ -> optimizeExpr expr2
            _ -> undefined
    else do
        newExpr2 <- optimizeExpr expr2
        return $ EAnd pos newExpr1 newExpr2

optimizeExpr(EOr pos expr1 expr2) = do
    newExpr1 <- optimizeExpr expr1
    if isELitBool newExpr1 then
        case newExpr1 of
            ELitTrue _ -> return $ ELitTrue pos
            ELitFalse _ -> optimizeExpr expr2
            _ -> undefined
    else do
        newExpr2 <- optimizeExpr expr2
        return $ EOr pos newExpr1 newExpr2


---------------------------------------------------------------------------------------------------------------
--never used funcs (keep track of usage and filter defs at the end) when time
--never used arguments (?) when time
--return based dead code (OK)
--if/while/for based dead code (OK)

cleanDeadCode :: Program -> ExceptT String IO Program
cleanDeadCode (Program pos defs) = do 
    return $ Program pos (map cleanDeadCodeTopDef defs)

cleanDeadCodeTopDef :: TopDef -> TopDef
cleanDeadCodeTopDef (FnDef pos ret ident args block) =
    FnDef pos ret ident args (cleanDeadCodeBlock block)

cleanDeadCodeTopDef (ClassDef pos ident block) =
    ClassDef pos ident (cleanDeadCodeClassBlock block)

cleanDeadCodeTopDef (ExtClassDef pos ident ext block) =
    ExtClassDef pos ident ext (cleanDeadCodeClassBlock block)

cleanDeadCodeBlock :: Block -> Block
cleanDeadCodeBlock (Block pos stmts) =
    Block pos (filter isNotEmptyStmt (cleanDeadCodeStmt stmts))

cleanDeadCodeClassBlock :: ClassBlock -> ClassBlock
cleanDeadCodeClassBlock (ClassBlock pos stmts) =
    ClassBlock pos (filter isNotEmptyClassStmt (map cleanDeadCodeClassStmt stmts))

cleanDeadCodeClassStmt :: ClassStmt -> ClassStmt
cleanDeadCodeClassStmt stmt@(ClassEmpty pos) = stmt

cleanDeadCodeClassStmt stmt@(ClassDecl pos t items) = stmt

cleanDeadCodeClassStmt (ClassMethod pos ret ident args block) =
    ClassMethod pos ret ident args (cleanDeadCodeBlock block)

cleanDeadCodeExtIdent :: ExtIdent -> ExtIdent
cleanDeadCodeExtIdent i = i

cleanDeadCodeStmt :: [Stmt] -> [Stmt]
cleanDeadCodeStmt [] = []
cleanDeadCodeStmt (s:ss) = case s of
  BStmt pos block -> BStmt pos (cleanDeadCodeBlock block) : cleanDeadCodeStmt ss
  Ret _ _ -> [s]
  VRet _ -> [s]
  While pos expr stmt ->
    if isELitBool expr then
        case expr of
            ELitFalse _ -> Empty pos : cleanDeadCodeStmt ss
            ELitTrue _ -> [While pos expr (head $ cleanDeadCodeStmt [stmt])]
            _ -> undefined
    else
        While pos expr (head $ cleanDeadCodeStmt [stmt]) : cleanDeadCodeStmt ss
  Cond pos expr stmt ->
    if isELitBool expr then
        case expr of
            ELitFalse _ -> Empty pos : cleanDeadCodeStmt ss
            ELitTrue _ -> BStmt pos (Block pos (cleanDeadCodeStmt [stmt])) : cleanDeadCodeStmt ss
            _ -> undefined
    else
        Cond pos expr (head $ cleanDeadCodeStmt [stmt]) : cleanDeadCodeStmt ss
  CondElse pos expr istmt estmt ->
    if isELitBool expr then
        case expr of
            ELitTrue _ -> BStmt pos (Block pos (cleanDeadCodeStmt [istmt])) : cleanDeadCodeStmt ss
            ELitFalse _ -> BStmt pos (Block pos (cleanDeadCodeStmt [estmt])) : cleanDeadCodeStmt ss
            _ -> undefined
    else
        CondElse pos expr (head $ cleanDeadCodeStmt [istmt]) (head $ cleanDeadCodeStmt [estmt]) : cleanDeadCodeStmt ss
  For pos t ident1 ident2 fstmt -> For pos t ident1 ident2 (head $ cleanDeadCodeStmt [fstmt]) : cleanDeadCodeStmt ss
  _ -> s : cleanDeadCodeStmt ss

cleanDeadCodeItem :: Type -> Item -> Item
cleanDeadCodeItem t i = i

cleanDeadCodeExpr :: Expr -> Expr
cleanDeadCodeExpr e = e