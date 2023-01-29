{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
module Utils where

import           Control.Monad.Trans.Class         (MonadTrans (lift))
import           Control.Monad.Trans.Except        (throwE)
import           Control.Monad.Trans.State         (StateT, get, put)
import           Data.List                         (intercalate)
import qualified Data.Set                          as Set
import           Syntax.AbsLattepp
import           System.Directory.Internal.Prelude (getArgs)
import           System.Exit                       (exitFailure, exitSuccess)
import           System.IO                         (hPutStrLn, stderr)
------------------------------GENERAL--------------------------------------------------------
exitError :: String -> IO ()
exitError e = do
    hPutStrLn stderr ("ERROR\nFATAL: " ++ e ++ " ¯\\_(ツ)_/¯")
    exitFailure

-- class Typical a where
--     toType :: a -> Type

getArgIdent :: Arg -> Ident
getArgIdent (Arg _ t ident) = ident

getArgType :: Arg -> Type
getArgType (Arg _ t _) = t

throwException x = lift $ throwE $ "Static Error: " ++ show x

firstDuplicateIndex :: Ord a => [a] -> Maybe Int
firstDuplicateIndex xs = dup' xs Set.empty
  where dup' [] _ = Nothing
        dup' (x:xs) s = if Set.member x s
                           then Just $ length s
                           else dup' xs (Set.insert x s)

noheadtail :: [a] -> [a]
noheadtail []  = []
noheadtail [a] = []
noheadtail xs  = tail $ init xs

align16 :: Int -> Int
align16 0 = 0
align16 x = 16 * (x `div` 16 + 1)

-- -------------PRETTY-------------------------------------------------------------------
class Pretty a where
    pretty :: a -> String

instance Pretty BNFC'Position where
    pretty (Just (row, col)) = "position " ++ show row ++ ":" ++ show col
    pretty _                 = "???"

instance Pretty ExtIdent where
    pretty (Id _ ident)      = pretty ident
    pretty (ArrId _ ident _) = pretty ident
    pretty (AttrId _ e1 e2)  = "obj attr"

instance Pretty Ident where
    pretty (Ident x) = x

instance Pretty Type where
    pretty (Primitive _ t) = pretty t
    pretty (ObjectType _ ident) = pretty ident
    pretty (Array _ t) = pretty t ++ "[]"
    pretty (Fun _ ret args) = "fun [" ++ intercalate "," (map pretty args) ++ " -> " ++ pretty ret ++ "]"

instance Pretty PrimType where
    pretty (Int _)  = "int"
    pretty (Str _)  = "string"
    pretty (Bool _) = "boolean"
    pretty (Void _) = "void"

instance Pretty Arg where
    pretty (Arg _ t i) = pretty t ++ " " ++ pretty i
-- ------------------------------------TYPE MANIP--------------------------------------------
class Raw a where
    raw :: a -> a

instance Raw a => Raw [a] where
    raw []     = []
    raw (a:as) = raw a : raw as
instance Raw PrimType where
    raw (Int _)  = Int Nothing
    raw (Str _)  = Str Nothing
    raw (Bool _) = Bool Nothing
    raw (Void _) = Void Nothing

instance Raw Type where
    raw (Primitive _ t)      = Primitive Nothing (raw t)
    raw (ObjectType _ ident) = ObjectType Nothing ident
    raw (Array _ t)          = Array Nothing (raw t)
    raw (Fun _ t ts)         = Fun Nothing (raw t) (map raw ts)

instance Raw ExtIdent where
    raw :: ExtIdent -> ExtIdent
    raw (Id _ ident)      = Id Nothing ident
    raw (ArrId _ ident e) = ArrId Nothing ident (raw e)
    raw (AttrId _ e1 e2)  = AttrId Nothing (raw e1) (raw e2)

instance Raw Expr where
    raw :: Expr -> Expr
    raw (ECast _ ident e)    = ECast Nothing ident (raw e)
    raw (ECastPrim _ t e)    = ECastPrim Nothing (raw t) (raw e)
    raw (ENewObject _ ident) = ENewObject Nothing ident
    raw (ENewArr _ t e)      = ENewArr Nothing (raw t) (raw e)
    raw (ENull _)            = ENull Nothing
    raw (EObject _ e1 e2)    = EObject Nothing (raw e1) (raw e2)
    raw (EArr _ ident e)     = EArr Nothing ident (raw e)
    raw (EVar _ ident)       = EVar Nothing ident
    raw (ELitInt _ val)      = ELitInt Nothing val
    raw (ELitTrue _)         = ELitTrue Nothing
    raw (ELitFalse _)        = ELitFalse Nothing
    raw (EApp _ ident es)    = EApp Nothing ident (map raw es)
    raw (EString _ s)        = EString Nothing s
    raw (Neg _ e)            = Neg Nothing (raw e)
    raw (Not _ e)            = Not Nothing (raw e)
    raw (EMul _ e1 op e2)    = EMul Nothing (raw e1) (raw op) (raw e2)
    raw (EAdd _ e1 op e2)    = EAdd Nothing (raw e1) (raw op) (raw e2)
    raw (ERel _ e1 op e2)    = ERel Nothing (raw e1) (raw op) (raw e2)
    raw (EAnd _ e1 e2)       = EAnd Nothing (raw e1) (raw e2)
    raw (EOr _ e1 e2)        = EOr Nothing (raw e1) (raw e2)

instance Raw AddOp where
    raw (Plus _)  = Plus Nothing
    raw (Minus _) = Minus Nothing

instance Raw MulOp where
    raw (Times _) = Times Nothing
    raw (Div _)   = Div Nothing
    raw (Mod _)   = Mod Nothing

instance Raw RelOp where
    raw (LTH _) = LTH Nothing
    raw (LE a)  = LE Nothing
    raw (GTH a) = GTH Nothing
    raw (GE a)  = GE Nothing
    raw (EQU a) = EQU Nothing
    raw (NE a)  = NE Nothing

rawInt =  Primitive Nothing $ Int Nothing
rawStr =  Primitive Nothing $ Str Nothing
rawBool =  Primitive Nothing $ Bool Nothing
rawVoid = Primitive Nothing $ Void Nothing
rawFun :: Type -> Type
rawFun r = Fun Nothing (raw r) []
rawExtIdent = Id Nothing


printProgram :: Program -> String
printProgram (Program _ defs ) = concatMap printTopDef defs

printTopDef :: TopDef -> String
printTopDef (FnDef pos ret ident args block)  = pretty ret ++ " " ++ pretty ident ++ "(" ++ intercalate "," (map pretty args) ++ ")\n" ++ printBlock block
printTopDef (ClassDef pos ident block)        = "class " ++ pretty ident ++ "\n" ++ printClassBlock block
printTopDef (ExtClassDef pos ident ext block) = "class " ++ pretty ident ++ " extends " ++ pretty ext ++ "\n" ++ printClassBlock block

printBlock :: Block -> String
printBlock (Block pos stmts) = "{\n" ++ concatMap printStmt stmts ++ "}\n"

printClassBlock :: ClassBlock -> String
printClassBlock (ClassBlock pos stmts) = "{\n" ++ concatMap printClassStmt stmts ++ "}\n"

printClassStmt :: ClassStmt -> String
printClassStmt (ClassEmpty pos)                       = ""
printClassStmt (ClassDecl pos t items)                = pretty t ++ " " ++ intercalate "," (map pretty items) ++ ";\n"
printClassStmt (ClassMethod pos ret ident args block) = pretty ret ++ " " ++ pretty ident ++ "(" ++ intercalate "," (map pretty args) ++ ")\n" ++ printBlock block

printExtIdent :: ExtIdent -> String
printExtIdent (Id pos ident)           = pretty ident
printExtIdent (ArrId pos ident expr)   = "array"
printExtIdent (AttrId pos expr1 expr2) = "object"

printStmt :: Stmt -> String
printStmt (Empty pos)                     = ";\n"
printStmt (BStmt pos block)               = printBlock block
printStmt (Decl pos t items)              = pretty t ++ " " ++ intercalate "," (map printItem items) ++ ";\n"
printStmt (Ass pos ident expr)            = pretty ident ++ " = " ++ printExpr expr ++ ";\n"
printStmt (Incr pos ident)                = pretty ident ++ "++;\n"
printStmt (Decr pos ident)                = pretty ident ++ "--;\n"
printStmt (Ret pos expr)                  = "return " ++ printExpr expr ++ ";\n"
printStmt (VRet pos)                      = "return;\n"
printStmt (Cond pos expr stmt)            = "if (" ++ printExpr expr ++ ")\n" ++ printStmt stmt
printStmt (CondElse pos expr stmt1 stmt2) = "if (" ++ printExpr expr ++ ")\n" ++ printStmt stmt1 ++ "else\n" ++ printStmt stmt2
printStmt (While pos expr stmt)           = "while (" ++ printExpr expr ++ ")\n" ++ printStmt stmt
printStmt (For pos t ident1 ident2 stmt)  = "for"
printStmt (SExp pos expr)                 = printExpr expr ++ ";\n"

printItem ::  Item -> String
printItem (NoInit pos ident)    = pretty ident
printItem (Init pos ident expr) = pretty ident ++ " = " ++ printExpr expr

printExpr :: Expr -> String
printExpr (ECast pos ident expr)    = "cast"
printExpr (ECastPrim pos t expr)    = "cast"
printExpr (ENewObject pos ident)    = "newobj"
printExpr (ENewArr pos t expr)      = "newarr"
printExpr (ENull pos)               = "null"
printExpr (EObject pos expr1 expr2) = "obj."
printExpr (EArr pos ident expr)     = "arr[]"
printExpr (EVar pos ident)          = pretty ident
printExpr (ELitInt pos integer)     = show integer
printExpr (ELitTrue pos)            = "true"
printExpr (ELitFalse pos)           = "false"
printExpr (EString pos s)           = s
printExpr (EApp pos ident exprs)    = pretty ident ++ "(" ++ intercalate "," (map printExpr exprs) ++ ")"
printExpr (Neg pos expr)            = "-" ++ printExpr expr
printExpr (Not pos expr)            = "!" ++ printExpr expr
printExpr (EMul pos expr1 op expr2) = printExpr expr1 ++ (case op of
   Times ma -> "*"
   Div ma   -> "/"
   Mod ma   -> "%") ++ printExpr expr2
printExpr (EAdd pos expr1 op expr2) = printExpr expr1 ++ (case op of
   Plus ma  -> "+"
   Minus ma -> "-") ++ printExpr expr2
printExpr (ERel pos expr1 op expr2) = printExpr expr1 ++ (case op of
  LTH ma -> "<"
  LE ma  -> "<="
  GTH ma -> ">"
  GE ma  -> ">="
  EQU ma -> "=="
  NE ma  -> "!=") ++ printExpr expr2
printExpr (EAnd pos expr1 expr2)    = printExpr expr1 ++ "&&" ++ printExpr expr2
printExpr (EOr pos expr1 expr2)     = printExpr expr1 ++ "||" ++ printExpr expr2
