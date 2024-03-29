module Quadruples.Preprocess where

import           Abstract.Typechecker.Data (TypeCheckerS (classEnv))
import qualified Abstract.Typechecker.Data as Tc
import           Control.Monad.Cont        (MonadIO (liftIO))
import           Control.Monad.RWS         (get, gets)
import           Data.Function             (on)
import           Data.List                 (sortBy)
import qualified Data.Map                  as M
import           Quadruples.Predata
import           Syntax.AbsLattepp

preprocess :: Program -> TypeCheckerS -> PreprocessState PreprocessS
preprocess (Program _ defs) tcEnv = do
    convertFromTc tcEnv
    cs <- gets preclasses
    -- liftIO $ putStrLn $ concatMap (\(_, def) -> show def) (M.toList cs)
    get

preprocessTopDef :: TopDef -> PreprocessState ()
preprocessTopDef (FnDef pos ret ident args block)  = return ()
preprocessTopDef (ClassDef pos ident block)        = do
    return ()
preprocessTopDef (ExtClassDef pos ident ext block) = return ()
preprocessBlock :: Block -> PreprocessState ()
preprocessBlock (Block pos stmts) = return ()
preprocessClassBlock :: ClassBlock -> PreprocessState ()
preprocessClassBlock (ClassBlock pos stmts) = return ()
preprocessClassStmt :: ClassStmt -> PreprocessState ()
preprocessClassStmt (ClassEmpty pos)                       = return ()
preprocessClassStmt (ClassDecl pos t items)                = return ()
preprocessClassStmt (ClassMethod pos ret ident args block) = return ()
preprocessExtIdent :: ExtIdent -> PreprocessState ()
preprocessExtIdent (Id pos ident)           = return ()
preprocessExtIdent (ArrId pos ident expr)   = return ()
preprocessExtIdent (AttrId pos expr1 expr2) = return ()
preprocessStmt :: Stmt -> PreprocessState ()
preprocessStmt (Empty pos)                     = return ()
preprocessStmt (BStmt pos block)               = return ()
preprocessStmt (Decl pos t items)              = return ()
preprocessStmt (Ass pos ident expr)            = return ()
preprocessStmt (Incr pos ident)                = return ()
preprocessStmt (Decr pos ident)                = return ()
preprocessStmt (Ret pos expr)                  = return ()
preprocessStmt (VRet pos)                      = return ()
preprocessStmt (Cond pos expr stmt)            = return ()
preprocessStmt (CondElse pos expr stmt1 stmt2) = return ()
preprocessStmt (While pos expr stmt)           = return ()
preprocessStmt (For pos t ident1 ident2 stmt)  = return ()
preprocessStmt (SExp pos expr)                 = return ()
preprocessItem :: Type -> Item -> PreprocessState ()
preprocessItem t (NoInit pos ident)    = return ()
preprocessItem t (Init pos ident expr) = return ()
preprocessExpr :: Expr -> PreprocessState ()
preprocessExpr (ECast pos ident expr)    = return ()
preprocessExpr (ECastPrim pos t expr)    = return ()
preprocessExpr (ENewObject pos ident)    = return ()
preprocessExpr (ENewArr pos t expr)      = return ()
preprocessExpr (ENull pos)               = return ()
preprocessExpr (EObject pos expr1 expr2) = return ()
preprocessExpr (EArr pos ident expr)     = return ()
preprocessExpr (EVar pos ident)          = return ()
preprocessExpr (ELitInt pos integer)     = return ()
preprocessExpr (ELitTrue pos)            = return ()
preprocessExpr (ELitFalse pos)           = return ()
preprocessExpr (EString pos s)           = return ()
preprocessExpr (EApp pos ident exprs)    = return ()
preprocessExpr (Neg pos expr)            = return ()
preprocessExpr (Not pos expr)            = return ()
preprocessExpr (EMul pos expr1 op expr2) = return ()
preprocessExpr (EAdd pos expr1 op expr2) = return ()
preprocessExpr (ERel pos expr1 op expr2) = return ()
preprocessExpr (EAnd pos expr1 expr2)    = return ()
preprocessExpr (EOr pos expr1 expr2)     = return ()

