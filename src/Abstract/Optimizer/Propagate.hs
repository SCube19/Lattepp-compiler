module Abstract.Optimizer.Propagate where
import           Control.Monad.Trans.Except
import           Debug.Trace                (trace)
import           Syntax.AbsLattepp
import           Utils                      (rawPrimBool, rawPrimInt)

propagateNegAndMinus :: Program -> Program
propagateNegAndMinus (Program pos defs) = Program pos $ map propagateTopDef defs

propagateTopDef :: TopDef -> TopDef
propagateTopDef (FnDef pos ret ident args block)  = FnDef pos ret ident args $ propagateBlock block
propagateTopDef (ClassDef pos ident block)        = ClassDef pos ident $ propagateClassBlock block
propagateTopDef (ExtClassDef pos ident ext block) = ExtClassDef pos ident ext $ propagateClassBlock block

propagateBlock :: Block -> Block
propagateBlock (Block pos stmts) = Block pos $ map propagateStmt stmts

propagateClassBlock :: ClassBlock -> ClassBlock
propagateClassBlock (ClassBlock pos stmts) = ClassBlock pos $ map propagateClassStmt stmts

propagateClassStmt :: ClassStmt ->  ClassStmt
propagateClassStmt (ClassMethod pos ret ident args block) = ClassMethod pos ret ident args $ propagateBlock block
propagateClassStmt c@(ClassEmpty pos)        = c
propagateClassStmt c@(ClassDecl pos t items) = c

propagateExtIdent :: ExtIdent -> ExtIdent
propagateExtIdent i@(Id pos ident)           = i
propagateExtIdent i@(ArrId pos ident expr)   = ArrId pos ident $ propagateExpr expr Nothing
propagateExtIdent i@(AttrId pos expr1 expr2) = AttrId pos (propagateExpr expr1 Nothing) (propagateExpr expr2 Nothing)

propagateStmt :: Stmt -> Stmt
propagateStmt s@(Empty pos)                     = s
propagateStmt (BStmt pos block)               = BStmt pos $ propagateBlock block
propagateStmt (Decl pos t items)              = Decl pos t $ map propagateItem items
propagateStmt (Ass pos ident expr)            = Ass pos ident $ propagateExpr expr Nothing
propagateStmt s@(Incr pos ident)                = s
propagateStmt s@(Decr pos ident)                = s
propagateStmt (Ret pos expr)                  = Ret pos $ propagateExpr expr Nothing
propagateStmt s@(VRet pos)                      = s
propagateStmt (Cond pos expr stmt)            = Cond pos (propagateExpr expr Nothing) (propagateStmt stmt)
propagateStmt (CondElse pos expr stmt1 stmt2) = CondElse pos (propagateExpr expr Nothing) (propagateStmt stmt1) (propagateStmt stmt2)
propagateStmt (While pos expr stmt)           = While pos (propagateExpr expr Nothing) (propagateStmt stmt)
propagateStmt (For pos t ident1 ident2 stmt)  = For pos t ident1 ident2 (propagateStmt stmt)
propagateStmt (SExp pos expr)                 = SExp pos (propagateExpr expr Nothing)

propagateItem :: Item -> Item
propagateItem i@(NoInit pos ident)  = i
propagateItem (Init pos ident expr) = Init pos ident (propagateExpr expr Nothing)

propagateExpr :: Expr -> Maybe PrimType -> Expr
propagateExpr (ECast pos ident expr) _    = ECast pos ident (propagateExpr expr Nothing)

propagateExpr (ECastPrim pos t expr) neg   =
    let newExpr = ECastPrim pos t (propagateExpr expr Nothing) in
    makeNegation newExpr pos neg

propagateExpr e@(ENewObject pos ident) _   = e

propagateExpr (ENewArr pos t expr) neg  =
    let newExpr = ENewArr pos t (propagateExpr expr Nothing) in
    makeNegation newExpr pos neg

propagateExpr e@(ENull pos) _              = e

propagateExpr (EObject pos expr1 expr2) neg =
    let newExpr = EObject pos (propagateExpr expr1 Nothing) (propagateExpr expr2 Nothing) in
    makeNegation newExpr pos neg

propagateExpr (EArr pos ident expr) neg    =
    let newExpr = EArr pos ident (propagateExpr expr Nothing) in
    makeNegation newExpr pos neg

propagateExpr e@(EVar pos ident) neg        = makeNegation e pos neg

propagateExpr e@(ELitInt pos integer) neg    = case neg of
  Nothing -> e
  Just pt -> ELitInt pos (-integer)

propagateExpr e@(ELitTrue pos) neg           = case neg of
  Nothing -> e
  Just pt -> ELitFalse pos

propagateExpr e@(ELitFalse pos) neg          = case neg of
  Nothing -> e
  Just pt -> ELitTrue pos

propagateExpr e@(EString pos s) _          = e

propagateExpr (EApp pos ident exprs) neg  =
    let newExpr = EApp pos ident (map (`propagateExpr` Nothing) exprs) in
    makeNegation newExpr pos neg

propagateExpr (Neg pos expr) neg            = case neg of
  Nothing -> propagateExpr expr $ Just rawPrimInt
  Just pt -> propagateExpr expr Nothing

propagateExpr (Not pos expr) neg          = case neg of
  Nothing -> propagateExpr expr $ Just rawPrimBool
  Just pt -> propagateExpr expr Nothing

propagateExpr (EMul pos expr1 op expr2) neg =
    case neg of
        Nothing -> EMul pos (propagateExpr expr1 neg) op (propagateExpr expr2 neg)
        Just t -> case t of
            Int _ ->  EMul pos (propagateExpr expr1 neg) op (propagateExpr expr2 Nothing)
            Bool _ -> EMul pos (propagateExpr expr1 Nothing) op (propagateExpr expr2 Nothing)
            _ -> undefined

propagateExpr (EAdd pos expr1 op expr2) neg = case neg of
  Nothing -> case op of
    Plus ma -> EAdd pos (propagateExpr expr1 Nothing) op (propagateExpr expr2 Nothing)
    Minus ma -> EAdd pos (propagateExpr expr1 Nothing) (Plus ma) (propagateExpr expr2 $ Just rawPrimInt)
  Just t -> case t of
    Int _ -> case op of
                Plus ma -> EAdd pos (propagateExpr expr1 neg) (Plus ma) (propagateExpr expr2 neg)
                Minus ma -> EAdd pos (propagateExpr expr1 neg) (Plus ma) (propagateExpr expr2 Nothing)
    Bool _ -> case op of
        Plus ma -> EAdd pos (propagateExpr expr1 Nothing) op (propagateExpr expr2 Nothing)
        Minus ma -> EAdd pos (propagateExpr expr1 Nothing) (Plus ma) (propagateExpr expr2 $ Just rawPrimInt)
    _ -> undefined

propagateExpr (ERel pos expr1 op expr2) neg = case neg of
  Nothing -> ERel pos (propagateExpr expr1 Nothing) op (propagateExpr expr2 Nothing)
  Just pt -> ERel pos (propagateExpr expr1 $ Just rawPrimBool) (revRelOp op) (propagateExpr expr2 $ Just rawPrimBool)

propagateExpr (EAnd pos expr1 expr2) neg    = case neg of
  Nothing -> EAnd pos (propagateExpr expr1 Nothing) (propagateExpr expr2 Nothing)
  Just pt -> EOr pos (propagateExpr expr1 neg) (propagateExpr expr2 neg)

propagateExpr (EOr pos expr1 expr2) neg    = case neg of
  Nothing -> EOr pos (propagateExpr expr1 Nothing) (propagateExpr expr2 Nothing)
  Just pt -> EAnd pos (propagateExpr expr1 neg) (propagateExpr expr2 neg)


makeNegation :: Expr -> BNFC'Position -> Maybe PrimType -> Expr
makeNegation e pos neg =
    case neg of
    Nothing -> e
    Just t -> case t of
        Int ma  -> Neg pos e
        Bool ma -> Not pos e
        _       -> undefined

revRelOp :: RelOp -> RelOp
revRelOp op = case op of
  LTH ma -> GE ma
  LE ma  -> GTH ma
  GTH ma -> LE ma
  GE ma  -> LTH ma
  EQU ma -> NE ma
  NE ma  -> EQU ma
