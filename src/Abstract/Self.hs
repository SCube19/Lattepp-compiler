module Abstract.Self where
import           Control.Monad.Except (ExceptT)
import           Control.Monad.State  (StateT, evalStateT, gets, modify)
import qualified Data.Map             as M
import qualified Data.Set             as S
import           Syntax.AbsLattepp

type SelfState = StateT SelfS (ExceptT String IO)
newtype SelfS = SelfS {
    defined :: S.Set Ident
}

initSelfS :: SelfS
initSelfS = SelfS {
    defined = S.empty
}

resetSelfState :: SelfState ()
resetSelfState = modify (const initSelfS)

addDefinition :: Ident -> SelfState ()
addDefinition i = modify (\s -> SelfS {
    defined = S.insert i (defined s)
})

addSelf :: Program -> ExceptT String IO Program
addSelf p = do
    evalStateT (addSelf' p) initSelfS

addSelf' :: Program -> SelfState Program
addSelf' (Program p defs) = do
    selfdefs <- mapM addSelfTopDef defs
    return $ Program p selfdefs

addSelfTopDef :: TopDef -> SelfState TopDef
addSelfTopDef def@(FnDef pos ret (Ident ident) args block) = return def

addSelfTopDef (ClassDef pos ident block) = do
    resetSelfState
    ClassDef pos ident <$> addSelfClassBlock block

addSelfTopDef (ExtClassDef pos ident ext block) = do
    resetSelfState
    ExtClassDef pos ident ext <$> addSelfClassBlock block

addSelfBlock :: Block -> SelfState Block
addSelfBlock (Block pos stmts) = do
    selfStmts <- mapM addSelfStmt stmts
    return $ Block pos selfStmts

addSelfClassBlock :: ClassBlock -> SelfState ClassBlock
addSelfClassBlock (ClassBlock pos stmts) = do
    selfStmts <- mapM addSelfClassStmt stmts
    return $ ClassBlock pos selfStmts

addSelfClassStmt :: ClassStmt -> SelfState ClassStmt
addSelfClassStmt s@(ClassEmpty pos)                       = return s
addSelfClassStmt s@(ClassDecl pos t items)                = return s
addSelfClassStmt (ClassMethod pos ret i args block) = do
    mapM_ (addDefinition . (\(Arg _ _ i) -> i)) args
    selfBlock <- addSelfBlock block
    return $ ClassMethod pos ret i args selfBlock


addSelfExtIdent :: ExtIdent -> SelfState ExtIdent
addSelfExtIdent (Id pos ident)         = do
    defs <- gets defined
    if S.member ident defs then
        return $ Id pos ident
    else
        return $ AttrId pos (EVar pos (Ident "self")) (EVar pos ident)

addSelfExtIdent (ArrId pos ident expr)  = do
    selfExpr <- addSelfExpr expr
    defs <- gets defined
    if S.member ident defs then
        return $ ArrId pos ident selfExpr
    else
        return $ AttrId pos (EVar pos (Ident "self")) (EArr pos ident selfExpr)

addSelfExtIdent a@(AttrId pos expr1 expr2) = do
    selfExpr1 <- addSelfExpr expr1
    selfExpr2 <- addSelfExpr expr2
    if selfExpr1 == expr1 then
        return $ AttrId pos expr1 selfExpr2
    else
        return $ AttrId pos (EVar pos (Ident "self")) (EObject pos expr1 selfExpr2)

addSelfStmt :: Stmt -> SelfState Stmt
addSelfStmt s@(Empty pos)                     = return s

addSelfStmt s@(BStmt pos block)               = do
    selfBlock <- addSelfBlock block
    return $ BStmt pos selfBlock

addSelfStmt s@(Decl pos t items)              = do
    selfItems <- mapM addSelfItem items
    return $ Decl pos t selfItems

addSelfStmt s@(Ass pos ident expr)            = do
    selfIdent <- addSelfExtIdent ident
    selfExpr <- addSelfExpr expr
    return $ Ass pos selfIdent selfExpr

addSelfStmt s@(Incr pos ident)                = do
    selfIdent <- addSelfExtIdent ident
    return $ Incr pos selfIdent

addSelfStmt s@(Decr pos ident)                = do
    selfIdent <- addSelfExtIdent ident
    return $ Decr pos selfIdent

addSelfStmt s@(Ret pos expr)              = do
    selfExpr <- addSelfExpr expr
    return $ Ret pos selfExpr

addSelfStmt s@(VRet pos)                      = return s

addSelfStmt s@(Cond pos expr stmt)            = do
    selfExpr <- addSelfExpr expr
    selfStmt <- addSelfStmt stmt
    return $ Cond pos selfExpr selfStmt

addSelfStmt s@(CondElse pos expr stmt1 stmt2) = do
    selfExpr <- addSelfExpr expr
    selfStmt1 <- addSelfStmt stmt1
    selfStmt2 <- addSelfStmt stmt2
    return $ CondElse pos selfExpr selfStmt1 selfStmt2

addSelfStmt s@(While pos expr stmt)           = do
    selfExpr <- addSelfExpr expr
    selfStmt <- addSelfStmt stmt
    return $ While pos selfExpr selfStmt

addSelfStmt s@(For pos t ident1 ident2 stmt)  = do
    selfIdent <- addSelfExtIdent ident2
    selfStmt <- addSelfStmt stmt
    return $ For pos t ident1 selfIdent selfStmt

addSelfStmt s@(SExp pos expr)                 = do
    selfExpr <- addSelfExpr expr
    return $ SExp pos selfExpr

addSelfItem :: Item -> SelfState Item
addSelfItem i@(NoInit pos ident)    = do
    addDefinition ident
    return i

addSelfItem (Init pos ident expr) = do
    addDefinition ident
    selfExpr <- addSelfExpr expr
    return $ Init pos ident selfExpr

addSelfExpr :: Expr -> SelfState Expr
addSelfExpr e@(ECast pos ident expr)    = do
    selfExpr <- addSelfExpr expr
    return $ ECast pos ident selfExpr

addSelfExpr e@(ECastPrim pos t expr)    = do
    selfExpr <- addSelfExpr expr
    return $ ECastPrim pos t selfExpr

addSelfExpr e@(ENewObject pos ident)    = return e

addSelfExpr e@(ENewArr pos t expr)      = do
    selfExpr <- addSelfExpr expr
    return $ ENewArr pos t selfExpr

addSelfExpr e@(ENull pos)               = return e

addSelfExpr e@(EObject pos expr1 expr2) = do
    selfExpr1 <- addSelfExpr expr1
    selfExpr2 <- addSelfExpr expr2
    if selfExpr1 == expr1 then
        return $ EObject pos expr1 selfExpr2
    else if isEVar expr1 && not (isSelf expr1) then
        return $ EObject pos (EVar pos (Ident "self")) (EObject pos expr1 selfExpr2)
    else
        return $ EObject pos selfExpr1 selfExpr2

addSelfExpr e@(EArr pos ident expr)     = do
    defs <- gets defined
    selfExpr <- addSelfExpr expr
    if S.member ident defs then
        return $ EArr pos ident selfExpr
    else
        return $ EObject pos (EVar pos (Ident "self")) (EArr pos ident selfExpr)

addSelfExpr e@(EVar pos ident)          = do
    defs <- gets defined
    if S.member ident defs || isSelf e then
        return e
    else
        return $ EObject pos (EVar pos (Ident "self")) e

addSelfExpr e@(ELitInt pos integer)     = return e

addSelfExpr e@(ELitTrue pos)            = return e

addSelfExpr e@(ELitFalse pos)           = return e

addSelfExpr e@(EString pos s)           = return e

addSelfExpr e@(EApp pos ident exprs) = do
    selfExprs <- mapM addSelfExpr exprs
    return $ EApp pos ident selfExprs

addSelfExpr e@(Neg pos expr)        = do
    selfExpr <- addSelfExpr expr
    return $ Neg pos selfExpr

addSelfExpr e@(Not pos expr)        = do
    selfExpr <- addSelfExpr expr
    return $ Not pos selfExpr

addSelfExpr e@(EMul pos expr1 op expr2) = do
    selfExpr1 <- addSelfExpr expr1
    selfExpr2 <- addSelfExpr expr2
    return $ EMul pos selfExpr1 op selfExpr2

addSelfExpr e@(EAdd pos expr1 op expr2) = do
    selfExpr1 <- addSelfExpr expr1
    selfExpr2 <- addSelfExpr expr2
    return $ EAdd pos selfExpr1 op selfExpr2

addSelfExpr e@(ERel pos expr1 op expr2) = do
    selfExpr1 <- addSelfExpr expr1
    selfExpr2 <- addSelfExpr expr2
    return $ ERel pos selfExpr1 op selfExpr2

addSelfExpr e@(EAnd pos expr1 expr2)    = do
    selfExpr1 <- addSelfExpr expr1
    selfExpr2 <- addSelfExpr expr2
    return $ EAnd pos selfExpr1 selfExpr2

addSelfExpr e@(EOr pos expr1 expr2) = do
    selfExpr1 <- addSelfExpr expr1
    selfExpr2 <- addSelfExpr expr2
    return $ EOr pos selfExpr1 selfExpr2

isEVar :: Expr -> Bool
isEVar EVar {} = True
isEVar _       = False

isSelf :: Expr -> Bool
isSelf (EVar _ (Ident i)) = i == "self"
isSelf _                  = False
