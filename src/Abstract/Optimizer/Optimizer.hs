{-# LANGUAGE AllowAmbiguousTypes #-}
module Abstract.Optimizer.Optimizer where

import           Abstract.Optimizer.Data    (OptimizerS (OptimizerS, consts),
                                             OptimizerState,
                                             Value (BoolV, IntV, StrV),
                                             ValueType (makeValue), addConst,
                                             castBool, castInteger, castString,
                                             constFromExpr, extractValue,
                                             isIntV, localOptimizerEnv,
                                             mapConst, removeChanges,
                                             removeConst, resetConst,
                                             retBoolLit)
import           Abstract.Optimizer.Utils
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Control.Monad.Trans.State  (StateT (runStateT), evalStateT,
                                             get, gets, modify, put)
import           Data.Foldable              (Foldable (foldl'))
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust, isJust)
import qualified Data.Set                   as S
import           Debug.Trace
import           Syntax.AbsLattepp
import           Utils                      (Raw (raw), prettyPrint, rawBool,
                                             rawExtIdent, rawInt, rawStr)

optimize :: Program -> ExceptT String IO Program
optimize p@(Program pos defs) = do
    newDefs <- evalStateT (mapM optimizeTopDef defs) (OptimizerS {consts = M.empty})
    cleaned <- cleanDeadCode $ Program pos newDefs
    if cleaned == p then
        return cleaned
    else
        optimize cleaned

optimizeTopDef :: TopDef -> OptimizerState TopDef
optimizeTopDef (FnDef pos ret ident args block) = do
    resetConst
    newBlock <- optimizeBlock block
    return $ FnDef pos ret ident args newBlock

optimizeTopDef (ClassDef pos ident block) = do
    resetConst
    newBlock <- optimizeClassBlock block
    return $ ClassDef pos ident newBlock

optimizeTopDef (ExtClassDef pos ident ext block) = do
    resetConst
    newBlock <- optimizeClassBlock block
    return $ ExtClassDef pos ident ext newBlock

optimizeBlock :: Block -> OptimizerState Block
optimizeBlock (Block pos stmts) = do
    removeChanges stmts
    st <- get
    newStmts <- localOptimizerEnv st $ mapM optimizeStmt stmts
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
    (newExpr, m) <- optimizeExpr expr
    return $ ArrId pos ident (reconstructExpr newExpr m)

optimizeExtIdent (AttrId pos expr1 expr2) = do
    (newExpr1, m1) <- optimizeExpr expr1
    (newExpr2, m2) <- optimizeExpr expr2
    return $ AttrId pos (reconstructExpr newExpr1 m1) (reconstructExpr newExpr2 m2)

optimizeStmt :: Stmt -> OptimizerState Stmt
optimizeStmt (Empty pos) = return $ Empty pos

optimizeStmt (BStmt pos block) = do
    newBlock <- optimizeBlock block
    return $ BStmt pos newBlock

optimizeStmt (Decl pos t items) = do
    newItems <- mapM (optimizeItem t) items
    return $ Decl pos t newItems

optimizeStmt (Ass pos ident expr) = do
    (newExpr, m) <- optimizeExpr expr
    let reconstructed = reconstructExpr newExpr m
    newIdent <- optimizeExtIdent ident
    case newIdent of
        Id _ i -> constFromExpr reconstructed i
        _      -> return ()
    return $ Ass pos newIdent reconstructed

optimizeStmt (Incr pos ident) = do
    newIdent <- optimizeExtIdent ident
    case ident of
        Id _ i -> modify (\s -> s {consts = M.adjust (\(t, v) -> (t, IntV (castInteger v + 1))) i (consts s)})
        _      -> return ()
    return $ Incr pos newIdent

optimizeStmt (Decr pos ident) = do
    newIdent <- optimizeExtIdent ident
    case ident of
        Id _ i -> modify (\s -> s {consts = M.adjust (\(t, v) -> (t, IntV $ castInteger v - 1)) i (consts s)})
        _      -> return ()
    return $ Decr pos newIdent

optimizeStmt (Ret pos expr) = do
    (newExpr, m) <- optimizeExpr expr
    return $ Ret pos (reconstructExpr newExpr m)

optimizeStmt (VRet pos) = return $ VRet pos

optimizeStmt (Cond pos expr stmt) = do
    (newExpr, m) <- optimizeExpr expr
    removeChanges [stmt]
    st <- get
    newStmt <- localOptimizerEnv st $ optimizeStmt stmt
    return $ Cond pos (reconstructExpr newExpr m) newStmt

optimizeStmt (CondElse pos expr stmt1 stmt2) = do
    (newExpr, m) <- optimizeExpr expr
    removeChanges [stmt1]
    removeChanges [stmt2]
    st <- get
    newStmt1 <- localOptimizerEnv st $optimizeStmt stmt1
    newStmt2 <- localOptimizerEnv st $optimizeStmt stmt2
    return $ CondElse pos (reconstructExpr newExpr m) newStmt1 newStmt2

optimizeStmt (While pos expr stmt) = do
    removeChanges [stmt]
    (newExpr, m) <- optimizeExpr expr
    st <- get
    newStmt <- localOptimizerEnv st $ optimizeStmt stmt
    return $ While pos (reconstructExpr newExpr m) newStmt

optimizeStmt (For pos t ident1 ident2 stmt) = do
    newIdent <- optimizeExtIdent ident2
    let (_, changes) = getDefsAndChanges [Decl Nothing t [NoInit Nothing ident1], stmt] (S.empty, S.empty)
    mapM_ removeConst changes
    st <- get
    newStmt <- localOptimizerEnv (st {consts = M.delete ident1 (consts st)}) $ optimizeStmt stmt
    return $ For pos t ident1 newIdent newStmt

optimizeStmt (SExp pos expr) = do
    (newExpr, m) <- optimizeExpr expr
    return $ SExp pos (reconstructExpr newExpr m)

optimizeItem :: Type -> Item -> OptimizerState Item
optimizeItem t i@(NoInit pos ident) =
    case t of
      Primitive _ pt -> do
        addConst ident (pt, case pt of
          Int _   -> IntV 0
          Str ma  -> StrV ""
          Bool ma -> BoolV False
          _       -> undefined)
        return i
      _ -> return i

optimizeItem t (Init pos ident expr) = do
    (newExpr, m) <- optimizeExpr expr
    let reconstructed = reconstructExpr newExpr m
    constFromExpr reconstructed ident
    return $ Init pos ident reconstructed

optimizeExpr :: Expr -> OptimizerState (Expr, Maybe (Value, Either MulOp AddOp))
optimizeExpr(ECast pos ident expr) = do
    (newExpr, m) <- optimizeExpr expr
    return (ECast pos ident (reconstructExpr newExpr m), Nothing)

optimizeExpr(ECastPrim pos t expr) = do
    (newExpr, m) <- optimizeExpr expr
    return (ECastPrim pos t (reconstructExpr newExpr m), Nothing)

optimizeExpr(ENewObject pos ident) = return (ENewObject pos ident, Nothing)

optimizeExpr(ENewArr pos t expr) = do
    (newExpr, m) <- optimizeExpr expr
    return (ENewArr pos t (reconstructExpr newExpr m), Nothing)

optimizeExpr(ENull pos) = return (ENull pos, Nothing)

optimizeExpr(EObject pos expr1 expr2) = do
    (newExpr1, m1) <- optimizeExpr expr1
    (newExpr2, m2) <- optimizeExpr expr2
    return (EObject pos (reconstructExpr newExpr1 m1) (reconstructExpr newExpr2 m2), Nothing)

optimizeExpr(EArr pos ident expr) = do
    (newExpr, m) <- optimizeExpr expr
    return (EArr pos ident (reconstructExpr newExpr m), Nothing)

optimizeExpr e@(EVar pos ident) = do
    newExpr <- mapConst e
    return (newExpr, Nothing)

optimizeExpr(ELitInt pos integer) = return (ELitInt pos integer, Nothing)

optimizeExpr(ELitTrue pos) = return (ELitTrue pos, Nothing)

optimizeExpr(ELitFalse pos) = return (ELitFalse pos, Nothing)

optimizeExpr(EApp pos ident exprs) = do
    newExprs <- mapM optimizeExpr exprs
    return (EApp pos ident (map (uncurry reconstructExpr) newExprs), Nothing)

optimizeExpr(EString pos string) = return (EString pos string, Nothing)

optimizeExpr(Neg pos expr) = do
    (newExpr, m) <- optimizeExpr expr
    case newExpr of
        ELitInt _ val -> return (ELitInt pos (-val), Nothing)
        newExpr       -> return (Neg pos (reconstructExpr newExpr m), Nothing)

optimizeExpr(Not pos expr) = do
    (newExpr, m) <- optimizeExpr expr
    case newExpr of
        ELitTrue _  -> return (ELitFalse pos, Nothing)
        ELitFalse _ -> return (ELitTrue pos, Nothing)
        newExpr     -> return (Not pos (reconstructExpr newExpr m), Nothing)

optimizeExpr (EMul pos expr1 op expr2) = do
    (newExpr1, m1) <- optimizeExpr expr1
    (newExpr2, m2) <- optimizeExpr expr2
    if isELitInt newExpr1 && isELitInt newExpr2 then
        return (ELitInt pos (convMulOp op (extractInt newExpr1) (extractInt newExpr2)), Nothing)
    else
        let reconstructed1 = reconstructExpr newExpr1 m1 in
        let reconstructed2 = reconstructExpr newExpr2 m2 in
        case op of
            Times _ ->
                if isELitInt newExpr1 && isJust m2 then do
                    let (val2, op2) = fromJust m2 in
                        if isTimes op2 then
                            return (newExpr2, Just (IntV $ extractInt newExpr1 * castInteger val2, Left op))
                        else
                            return (reconstructed2, Just (IntV $ extractInt newExpr1, Left op))
                else if isELitInt newExpr2 && isJust m1 then do
                    let (val1, op1) = fromJust m1 in
                        if isTimes op1 then do
                            return (newExpr1, Just (IntV $ extractInt newExpr2 * castInteger val1, Left op))
                        else
                            return (reconstructed1, Just (IntV $ extractInt newExpr2, Left op))
                else if isJust m1 && isJust m2 then do
                    let (val1, op1) = fromJust m1 in
                        let (val2, op2) = fromJust m2 in
                        if isTimes op1 && isTimes op2 then
                            return (EMul pos newExpr1 op newExpr2, Just (IntV $ castInteger val1 * castInteger val2, Left op))
                        else if isTimes op1 then
                            return (EMul pos newExpr1 op reconstructed2, Just (val1, Left op))
                        else if isTimes op2 then
                            return (EMul pos reconstructed1 op newExpr2, Just (val2, Left op))
                        else
                            return (EMul pos reconstructed1 op reconstructed2, Nothing)
                else if isELitInt newExpr1 then do
                    return (newExpr2, Just (IntV $ extractInt newExpr1, Left op))
                else if isELitInt newExpr2 then
                    return (newExpr1, Just (IntV $ extractInt newExpr2, Left op))
                else if isJust m1 then
                    let (val1, op1) = fromJust m1 in
                    if isTimes op1 then
                        return (EMul pos newExpr1 op newExpr2, Just (val1, Left op))
                    else
                        return (EMul pos reconstructed1 op newExpr2, Nothing)
                else if isJust m2 then
                    let (val2, op2) = fromJust m2 in
                   if isTimes op2 then
                        return (EMul pos newExpr1 op newExpr2, Just (val2, Left op))
                    else
                        return (EMul pos newExpr1 op reconstructed2, Nothing)
                else return (EMul pos reconstructed1 op reconstructed2, Nothing)
            _ -> return (EMul pos reconstructed1 op reconstructed2, Nothing)


optimizeExpr (EAdd pos expr1 op expr2) = do
    (newExpr1, m1) <- optimizeExpr expr1
    (newExpr2, m2) <- optimizeExpr expr2
    let reconstructed1 = reconstructExpr newExpr1 m1 in
        let reconstructed2 = reconstructExpr newExpr2 m2 in
        if isELitInt newExpr1 && isELitInt newExpr2 then
            return (ELitInt pos (convAddOp op (extractInt newExpr1) (extractInt newExpr2)), Nothing)
        else if isEString newExpr1 && isEString newExpr2 then
            return (EString pos (case op of
                Plus _  -> extractString newExpr1 ++ extractString newExpr2
                Minus _ -> undefined
                ), Nothing)
        else
                let reconstructed1 = reconstructExpr newExpr1 m1 in
                let reconstructed2 = reconstructExpr newExpr2 m2 in
                case op of
                    Plus _ ->
                        if (isELitInt newExpr1 || isELitString newExpr1) && isJust m2 then do
                            let (val2, op2) = fromJust m2 in
                                if isPlus op2 then
                                    return (newExpr2, Just (extractValue newExpr1 + val2, Right op))
                                else
                                    return (reconstructed2, Just (extractValue newExpr1, Right op))
                        else if (isELitInt newExpr2 || isELitString newExpr2) && isJust m1 then do
                            let (val1, op1) = fromJust m1 in
                                if isPlus op1 then do
                                    return (newExpr1, Just (extractValue newExpr2 + val1, Right op))
                                else
                                    return (reconstructed1, Just (extractValue newExpr2, Right op))
                        else if isJust m1 && isJust m2 then do
                            let (val1, op1) = fromJust m1 in
                                let (val2, op2) = fromJust m2 in
                                if isPlus op1 && isPlus op2 then
                                    return (EAdd pos newExpr1 op newExpr2, Just (val1 + val2, Right op))
                                else if isPlus op1 then
                                    return (EAdd pos newExpr1 op reconstructed2, Just (val1, Right op))
                                else if isPlus op2 then
                                    return (EAdd pos reconstructed1 op newExpr2, Just (val2, Right op))
                                else
                                    return (EAdd pos reconstructed1 op reconstructed2, Nothing)
                        else if isELitInt newExpr1 || isELitString newExpr1 then do
                            return (newExpr2, Just (extractValue newExpr1, Right op))
                        else if isELitInt newExpr2 || isELitString newExpr2 then
                            return (newExpr1, Just (extractValue newExpr2, Right op))
                        else if isJust m1 then
                            let (val1, op1) = fromJust m1 in
                            if isPlus op1 then
                                return (EAdd pos newExpr1 op newExpr2, Just (val1, Right op))
                            else
                                return (EAdd pos reconstructed1 op newExpr2, Nothing)
                        else if isJust m2 then
                            let (val2, op2) = fromJust m2 in
                        if isPlus op2 then
                                return (EAdd pos newExpr1 op newExpr2, Just (val2, Right op))
                            else
                                return (EAdd pos newExpr1 op reconstructed2, Nothing)
                        else return (EAdd pos reconstructed1 op reconstructed2, Nothing)
                    _ -> return (EAdd pos reconstructed1 op reconstructed2, Nothing)

optimizeExpr(ERel pos expr1 op expr2) = do
    (newExpr1, m1) <- optimizeExpr expr1
    (newExpr2, m2) <- optimizeExpr expr2
    let reconstructed1 = reconstructExpr newExpr1 m1 in
        let reconstructed2 = reconstructExpr newExpr2 m2 in
        if isELitInt reconstructed1 && isELitInt reconstructed2 then do
            let result = (case op of
                    LTH _ -> extractInt reconstructed1 < extractInt  reconstructed2
                    LE _  -> extractInt reconstructed1 <= extractInt reconstructed2
                    GTH _ -> extractInt reconstructed1 > extractInt  reconstructed2
                    GE _  -> extractInt reconstructed1 >= extractInt reconstructed2
                    EQU _ -> extractInt reconstructed1 == extractInt reconstructed2
                    NE _  -> extractInt reconstructed1 /= extractInt reconstructed2)
            retBoolLit pos result
        else if isELitBool reconstructed1 && isELitBool reconstructed2 then do
            let result = (case op of
                    EQU _ -> extractBool reconstructed1 == extractBool reconstructed2
                    NE _  -> extractBool reconstructed1 /= extractBool reconstructed2
                    _     -> undefined)
            retBoolLit pos result
        else if isEString reconstructed1 && isEString reconstructed2 then do
            let result = (case op of
                    EQU _ -> extractString reconstructed1 == extractString reconstructed2
                    NE _  -> extractString reconstructed1 /= extractString reconstructed2
                    _     -> undefined)
            retBoolLit pos result
        else
            return (ERel pos reconstructed1 op reconstructed2, Nothing)

-- jeszcze pogrzebać
optimizeExpr(EAnd pos expr1 expr2) = do
    (newExpr1, m1) <- optimizeExpr expr1
    let reconstructed1 = reconstructExpr newExpr1 m1 in
        if isELitBool reconstructed1 then
            case reconstructed1 of
                ELitFalse _ -> retBoolLit pos False
                ELitTrue _  -> optimizeExpr expr2
                _           -> undefined
        else do
            (newExpr2, m2) <- optimizeExpr expr2
            let reconstructed2 = reconstructExpr newExpr2 m2 in
                return (EAnd pos reconstructed1 reconstructed2, Nothing)

optimizeExpr(EOr pos expr1 expr2) = do
    (newExpr1, m1) <- optimizeExpr expr1
    let reconstructed1 = reconstructExpr newExpr1 m1 in
        if isELitBool reconstructed1 then
            case reconstructed1 of
                ELitTrue _  -> retBoolLit pos True
                ELitFalse _ -> optimizeExpr expr2
                _           -> undefined
        else do
            (newExpr2, m2) <- optimizeExpr expr2
            let reconstructed2 = reconstructExpr newExpr2 m2 in
                return (EOr pos reconstructed1 reconstructed2, Nothing)


reconstructExpr :: Expr -> Maybe (Value, Either MulOp AddOp) -> Expr
reconstructExpr e Nothing = e
reconstructExpr e (Just (v, op)) =
    case op of
        Left mulop ->
            case mulop of
                Times pos -> EMul pos e mulop (ELitInt pos $ castInteger v)
                _         -> undefined
        Right addop ->
            case addop of
                Plus pos -> EAdd pos e addop (if isIntV v then ELitInt pos $ castInteger v else EString pos $ castString v)
                Minus pos -> undefined

---------------------------------------------------------------------------------------------------------------

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
            ELitTrue _  -> [While pos expr (head $ cleanDeadCodeStmt [stmt])]
            _           -> undefined
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
