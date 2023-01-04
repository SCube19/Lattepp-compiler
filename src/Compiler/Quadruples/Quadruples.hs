module Compiler.Quadruples.Quadruples where

import           Compiler.Quadruples.Data
import           Compiler.Quadruples.Predata    (initPreprocessS)
import           Compiler.Quadruples.Preprocess (preprocess)
import           Control.Monad.State            (MonadState (get, put),
                                                 MonadTrans (lift), evalStateT,
                                                 gets, modify)
import           Control.Monad.Trans.Except     (ExceptT)
import           Syntax.AbsLattepp              as Abs


quadruplize :: Program -> QuadruplesState QProgram
quadruplize p@(Program _ defs) = do
    preprocessed <- lift $ evalStateT (preprocess p) initPreprocessS
    modify (`setPreprocessing` preprocessed)
    mapM_ quadruplizeTopDef defs
    gets qprogram


quadruplizeTopDef :: TopDef -> QuadruplesState ()
quadruplizeTopDef (FnDef pos ret ident args block)  = return ()
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
