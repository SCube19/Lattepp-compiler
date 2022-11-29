module Optimizer.Utils where

import           Optimizer.Data    (OptimizerState)
import           Syntax.AbsLattepp

isNotEmptyStmt :: Stmt -> Bool
isNotEmptyStmt (Empty _) = False
isNotEmptyStmt _         = True

isNotEmptyClassStmt :: ClassStmt -> Bool
isNotEmptyClassStmt (ClassEmpty _) = False
isNotEmptyClassStmt _              = True

isELitInt :: Expr -> Bool
isELitInt (ELitInt _ _) = True
isELitInt _             = False

isELitBool :: Expr -> Bool
isELitBool (ELitTrue _)  = True
isELitBool (ELitFalse _) = True
isELitBool _             = False

isEString :: Expr -> Bool
isEString (EString _ _) = True
isEString _             = False

extractInt :: Expr -> Integer
extractInt (ELitInt _ int) = int
extractInt _               = undefined

extractBool :: Expr -> Bool
extractBool (ELitTrue _)  = True
extractBool (ELitFalse _) = False
extractBool _             = undefined

extractString :: Expr -> String
extractString (EString _ s) = s
extractString _             = undefined









