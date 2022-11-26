module Typechecker.TypeChecker where

import Syntax.AbsLattepp
import Control.Applicative.Lift ()
import Control.Monad ( unless, when, zipWithM_ )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class ()
import Control.Monad.Trans.Except ( ExceptT )
import Control.Monad.Trans.State (evalStateT, get, gets, put)
import Data.List ()
import Debug.Trace (trace)
import Typechecker.Data
import Utils
import qualified Data.Set as S

typeCheck :: Program -> ExceptT String IO ()
typeCheck (Program _ defs) = 
   evalStateT (mapM_ typeCheckTopDef defs) initTypeCheckerS

typeCheckTopDef :: TopDef -> TypeCheckerState ()
typeCheckTopDef (FnDef pos ret ident args block) = return  ()
typeCheckTopDef (ClassDef pos ident block) = return ()
typeCheckTopDef (ExtClassDef pos ident ext block) = return ()

typeCheckBlock :: Block -> TypeCheckerState ()
typeCheckBlock (Block pos stmts) = return ()

typeCheckClassBlock :: ClassBlock -> TypeCheckerState ()
typeCheckClassBlock (ClassBlock pos stmts) = return ()

typeCheckClassStmt :: ClassStmt -> TypeCheckerState ()
typeCheckClassStmt (ClassEmpty pos) = return ()
typeCheckClassStmt (ClassDecl pos t item) = return ()
typeCheckClassStmt (ClassMethod pos ret ident args block) = return  ()

typeCheckExtIdent :: ExtIdent -> TypeCheckerState ()
typeCheckExtIdent (Id pos ident) = return ()
typeCheckExtIdent (ArrId pos ident expr) = return  ()
typeCheckExtIdent (AttrId pos expr1 expr2) = return  ()

typeCheckStmt :: Stmt -> TypeCheckerState ()
typeCheckStmt (Empty pos) = return ()
typeCheckStmt (BStmt pos block) = return ()
typeCheckStmt (Decl pos t item) = return ()
typeCheckStmt (Ass pos ident expr) = return ()
typeCheckStmt (Incr pos ident) = return ()
typeCheckStmt (Decr pos ident) = return ()
typeCheckStmt (Ret pos expr) = return ()
typeCheckStmt (VRet pos) = return ()
typeCheckStmt (Cond pos expr stmt) = return ()
typeCheckStmt (CondElse pos expr istmt estmt) = return ()
typeCheckStmt (While pos expr stmt) = return ()
typeCheckStmt (For pos t ident collection stmt) = return ()
typeCheckStmt (SExp pos expr) = return ()

typeCheckItem :: Item -> TypeCheckerState ()
typeCheckItem (NoInit pos ident) = return ()
typeCheckItem (Init pos ident expr) = return ()

typeCheckExpr :: Expr -> TypeCheckerState Type
typeCheckExpr (ECast pos ident expr) = return $ Primitive pos (Int pos)
typeCheckExpr (ECastPrim pos t expr) = return $ Primitive pos (Int pos)
typeCheckExpr (ENewObject pos ident) = return $ Primitive pos (Int pos)
typeCheckExpr (ENewArr pos t expr) = return $ Primitive pos (Int pos)
typeCheckExpr (ENull pos) = return $ Primitive pos (Int pos)
typeCheckExpr (EObject pos expr1 expr2) = return $ Primitive pos (Int pos)
typeCheckExpr (EArr pos ident expr) = return $ Primitive pos (Int pos)
typeCheckExpr (EVar pos ident) = return $ Primitive pos (Int pos)
typeCheckExpr (ELitInt pos val) = return $ Primitive pos (Int pos)
typeCheckExpr (ELitTrue pos) = return $ Primitive pos (Int pos)
typeCheckExpr (ELitFalse pos) = return$ Primitive pos (Int pos)
typeCheckExpr (EApp pos ident expr) = return $ Primitive pos (Int pos)
typeCheckExpr (EString pos s) = return $ Primitive pos (Int pos)
typeCheckExpr (Neg pos expr) = return $ Primitive pos (Int pos)
typeCheckExpr (Not pos expr) = return $ Primitive pos (Int pos)
typeCheckExpr (EMul pos expr1 op expr2) = return $ Primitive pos (Int pos)
typeCheckExpr (EAdd pos expr1 op expr2) = return $ Primitive pos (Int pos)
typeCheckExpr (ERel pos expr1 op expr2) = return $ Primitive pos (Int pos)
typeCheckExpr (EAnd pos expr1 expr2) = return $ Primitive pos (Int pos)
typeCheckExpr (EOr pos expr1 expr2) = return $ Primitive pos (Int pos)
---------------------------------------------------------------------------

checkReturns :: Program -> ExceptT String IO ()
checkReturns (Program _ stmts) = return ()