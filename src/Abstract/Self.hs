module Abstract.Self where
import           Control.Monad.Except   (ExceptT)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.State    (StateT, evalStateT, gets, modify)
import           Data.Bool              (Bool (False))
import qualified Data.Map               as M
import qualified Data.Set               as S
import           Syntax.AbsLattepp
import           Utils                  (rawInt, rawVoid)

type SelfState = StateT SelfS (ExceptT String IO)
data SelfS = SelfS {
    defined :: S.Set Ident,
    methods :: S.Set Ident
}

initSelfS :: SelfS
initSelfS = SelfS {
    defined = S.empty,
    methods = S.empty
}

resetDefinitions :: SelfState ()
resetDefinitions = modify (\s -> s {defined = S.empty})

resetMethods :: SelfState ()
resetMethods = modify (\s -> s {methods = S.empty})

resetSelfState :: SelfState ()
resetSelfState = modify (const initSelfS)

addDefinition :: Ident -> SelfState ()
addDefinition i = modify (\s -> SelfS {
    defined = S.insert i (defined s),
    methods = methods s
})

addMethod :: Maybe Ident -> SelfState ()
addMethod mi =
    case mi of
        Nothing -> return ()
        Just i -> modify (\s -> SelfS {
            defined = defined s,
            methods = S.insert i (methods s)
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
    mapM_ (addMethod . getMethodIdent) stmts
    selfStmts <- mapM addSelfClassStmt stmts
    return $ ClassBlock pos selfStmts

getMethodIdent :: ClassStmt -> Maybe Ident
getMethodIdent (ClassEmpty _)          = Nothing
getMethodIdent ClassDecl {}            = Nothing
getMethodIdent (ClassMethod _ _ i _ _) = Just i

addSelfClassStmt :: ClassStmt -> SelfState ClassStmt
addSelfClassStmt s@(ClassEmpty pos)                       = return s
addSelfClassStmt s@(ClassDecl pos t items)                = return s
addSelfClassStmt (ClassMethod pos ret i args block) = do
    resetDefinitions
    mapM_ (addDefinition . (\(Arg _ _ i) -> i)) (Arg Nothing rawInt (Ident "self") : args)
    selfBlock <- addSelfBlock block
    return $ ClassMethod pos ret i args selfBlock


addSelfExtIdent :: ExtIdent -> Bool -> SelfState ExtIdent
addSelfExtIdent ext@(Id pos ident) trySelf = do
    defs <- gets defined
    if not trySelf || S.member ident defs then
        return ext
    else
        return $ AttrId pos (EVar pos (Ident "self")) (EVar pos ident)

addSelfExtIdent (ArrId pos ident expr) trySelf = do
    selfExpr <- addSelfExpr expr True
    defs <- gets defined
    if not trySelf || S.member ident defs then
        return $ ArrId pos ident selfExpr
    else
        return $ AttrId pos (EVar pos (Ident "self")) (EArr pos ident selfExpr)

addSelfExtIdent a@(AttrId pos expr1 expr2) True = do
    selfExpr1 <- addSelfExpr expr1 True
    selfExpr2 <- addSelfExpr expr2 False
    case selfExpr1 of
        EObject {} -> return $ AttrId pos (EVar pos (Ident "self")) (EObject pos expr1 selfExpr2)
        _ -> return $ AttrId pos selfExpr1 selfExpr2

addSelfStmt :: Stmt -> SelfState Stmt
addSelfStmt s@(Empty pos)                  = return s

addSelfStmt s@(BStmt pos block)             = do
    selfBlock <- addSelfBlock block
    return $ BStmt pos selfBlock

addSelfStmt s@(Decl pos t items)            = do
    selfItems <- mapM addSelfItem items
    return $ Decl pos t selfItems

addSelfStmt s@(Ass pos ident expr)          = do
    selfIdent <- addSelfExtIdent ident True
    selfExpr <- addSelfExpr expr True
    return $ Ass pos selfIdent selfExpr

addSelfStmt s@(Incr pos ident)                = do
    selfIdent <- addSelfExtIdent ident True
    return $ Incr pos selfIdent

addSelfStmt s@(Decr pos ident)                = do
    selfIdent <- addSelfExtIdent ident True
    return $ Decr pos selfIdent

addSelfStmt s@(Ret pos expr)              = do
    selfExpr <- addSelfExpr expr True
    return $ Ret pos selfExpr

addSelfStmt s@(VRet pos)                      = return s

addSelfStmt s@(Cond pos expr stmt)            = do
    selfExpr <- addSelfExpr expr True
    selfStmt <- addSelfStmt stmt
    return $ Cond pos selfExpr selfStmt

addSelfStmt s@(CondElse pos expr stmt1 stmt2) = do
    selfExpr <- addSelfExpr expr True
    selfStmt1 <- addSelfStmt stmt1
    selfStmt2 <- addSelfStmt stmt2
    return $ CondElse pos selfExpr selfStmt1 selfStmt2

addSelfStmt s@(While pos expr stmt)           = do
    selfExpr <- addSelfExpr expr True
    selfStmt <- addSelfStmt stmt
    return $ While pos selfExpr selfStmt

addSelfStmt s@(For pos t ident1 ident2 stmt)  = do
    selfIdent <- addSelfExtIdent ident2 True
    selfStmt <- addSelfStmt stmt
    return $ For pos t ident1 selfIdent selfStmt

addSelfStmt s@(SExp pos expr)                 = do
    selfExpr <- addSelfExpr expr True
    return $ SExp pos selfExpr

addSelfItem :: Item -> SelfState Item
addSelfItem i@(NoInit pos ident)    = do
    addDefinition ident
    return i

addSelfItem (Init pos ident expr) = do
    addDefinition ident
    selfExpr <- addSelfExpr expr True
    return $ Init pos ident selfExpr

addSelfExpr :: Expr -> Bool -> SelfState Expr
addSelfExpr e@(ECast pos ident expr) trySelf  = do
    selfExpr <- addSelfExpr expr trySelf
    return $ ECast pos ident selfExpr

addSelfExpr e@(ECastPrim pos t expr) trySelf = do
    selfExpr <- addSelfExpr expr trySelf
    return $ ECastPrim pos t selfExpr

addSelfExpr e@(ENewObject pos ident) _  = return e

addSelfExpr e@(ENewArr pos t expr) _ = do
    selfExpr <- addSelfExpr expr True
    return $ ENewArr pos t selfExpr

addSelfExpr e@(ENull pos) _             = return e

addSelfExpr e@(EObject pos expr1 expr2) trySelf = do
    selfExpr1 <- addSelfExpr expr1 trySelf
    selfExpr2 <- addSelfExpr expr2 False
    case selfExpr1 of
        EObject {} -> return $ EObject pos (EVar pos (Ident "self")) (EObject pos expr1 selfExpr2)
        _ -> return $ EObject pos selfExpr1 selfExpr2

addSelfExpr e@(EArr pos ident expr) trySelf = do
    defs <- gets defined
    selfExpr <- addSelfExpr expr True
    if not trySelf || S.member ident defs then
        return $ EArr pos ident selfExpr
    else
        return $ EObject pos (EVar pos (Ident "self")) (EArr pos ident selfExpr)

addSelfExpr e@(EVar pos ident) trySelf = do
    defs <- gets defined
    if not trySelf || S.member ident defs then
        return e
    else
        return $ EObject pos (EVar pos (Ident "self")) e

addSelfExpr e@(ELitInt pos integer) _     = return e

addSelfExpr e@(ELitTrue pos) _           = return e

addSelfExpr e@(ELitFalse pos) _         = return e

addSelfExpr e@(EString pos s) _          = return e

addSelfExpr e@(EApp pos ident exprs) trySelf = do
    selfExprs <- mapM (`addSelfExpr` True) exprs
    methods <- gets methods
    if trySelf && S.member ident methods then
        return $ EObject pos (EVar pos (Ident "self")) (EApp pos ident selfExprs)
    else
        return $ EApp pos ident selfExprs

addSelfExpr e@(Neg pos expr)  _      = do
    selfExpr <- addSelfExpr expr True
    return $ Neg pos selfExpr

addSelfExpr e@(Not pos expr) _       = do
    selfExpr <- addSelfExpr expr True
    return $ Not pos selfExpr

addSelfExpr e@(EMul pos expr1 op expr2) _ = do
    selfExpr1 <- addSelfExpr expr1 True
    selfExpr2 <- addSelfExpr expr2 True
    return $ EMul pos selfExpr1 op selfExpr2

addSelfExpr e@(EAdd pos expr1 op expr2) _ = do
    selfExpr1 <- addSelfExpr expr1 True
    selfExpr2 <- addSelfExpr expr2 True
    return $ EAdd pos selfExpr1 op selfExpr2

addSelfExpr e@(ERel pos expr1 op expr2) _ = do
    selfExpr1 <- addSelfExpr expr1 True
    selfExpr2 <- addSelfExpr expr2 True
    return $ ERel pos selfExpr1 op selfExpr2

addSelfExpr e@(EAnd pos expr1 expr2) _ = do
    selfExpr1 <- addSelfExpr expr1 True
    selfExpr2 <- addSelfExpr expr2 True
    return $ EAnd pos selfExpr1 selfExpr2

addSelfExpr e@(EOr pos expr1 expr2) _ = do
    selfExpr1 <- addSelfExpr expr1 True
    selfExpr2 <- addSelfExpr expr2 True
    return $ EOr pos selfExpr1 selfExpr2

isEVar :: Expr -> Bool
isEVar EVar {} = True
isEVar _       = False

isSelf :: Expr -> Bool
isSelf (EVar _ (Ident i)) = i == "self"
isSelf _                  = False
