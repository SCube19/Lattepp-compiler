module Optimizer.Optimizer where

import Syntax.AbsLattepp
import Control.Monad.Trans.Except (ExceptT, runExceptT)


optimize :: Program -> ExceptT String IO Program
optimize (Program pos defs) = do
    newDefs <- mapM optimizeTopDef defs
    return $ Program pos newDefs

optimizeTopDef :: TopDef -> ExceptT String IO TopDef
optimizeTopDef (FnDef pos ret ident args block) = do
    newBlock <- optimizeBlock block
    return $ FnDef pos ret ident args newBlock

optimizeTopDef (ClassDef pos ident block) = do
    newBlock <- optimizeClassBlock block
    return $ ClassDef pos ident newBlock

optimizeTopDef (ExtClassDef pos ident ext block) = do
    newBlock <- optimizeClassBlock block
    return $ ExtClassDef pos ident ext newBlock

optimizeBlock :: Block -> ExceptT String IO Block
optimizeBlock (Block pos stmts) = do
    newStmts <- mapM optimizeStmt stmts
    return $ Block pos (filter isNotEmptyStmt newStmts) 

optimizeClassBlock :: ClassBlock -> ExceptT String IO ClassBlock
optimizeClassBlock (ClassBlock pos stmts) = do
    newStmts <- mapM optimizeClassStmt stmts
    return $ ClassBlock pos (filter isNotEmptyClassStmt newStmts) 

optimizeClassStmt :: ClassStmt -> ExceptT String IO ClassStmt
optimizeClassStmt (ClassEmpty pos) = return $ ClassEmpty pos
optimizeClassStmt (ClassDecl pos ret items) = do
    newItems <- mapM optimizeItem items
    return $ ClassDecl pos ret newItems

optimizeClassStmt (ClassMethod pos ret ident args block) = do
    newBlock <- mapM optimizeBlock block
    return $ ClassMethod pos ret ident args newBlock

optimizeStmt :: Stmt -> ExceptT String IO Stmt
optimizeStmt = return

optimizeItem :: Item -> ExceptT String IO Item
optimizeItem = return

optimizeExpr :: Expr -> ExceptT String IO Expr
optimizeExpr = return 


---------------------------------------------------------------------------
isNotEmptyStmt :: Stmt -> Bool
isNotEmptyStmt (Empty _) = False
isNotEmptyStmt _ = True

isNotEmptyClassStmt :: ClassStmt -> Bool
isNotEmptyClassStmt (ClassEmpty _) = False
isNotEmptyClassStmt _ = True