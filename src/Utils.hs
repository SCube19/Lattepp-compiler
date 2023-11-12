{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
module Utils where

import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Control.Monad.Trans.Class         (MonadTrans (lift))
import           Control.Monad.Trans.Except        (throwE)
import           Control.Monad.Trans.State         (StateT, get, put)
import           Data.List                         (intercalate)
import qualified Data.Set                          as S
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

isArrayType :: Type -> Bool
isArrayType (Array _ _) = True
isArrayType _           = False

getArrayType :: Type -> Type
getArrayType (Array _ t) = raw t
getArrayType _           = undefined

-- -------------PRETTY-------------------------------------------------------------------
class Pretty a where
    pretty :: a -> String

instance Pretty BNFC'Position where
    pretty (Just (row, col)) = "position " ++ show row ++ ":" ++ show col
    pretty _                 = "???"

instance Pretty ExtIdent where
    pretty (Id _ ident)      = pretty ident
    pretty (ArrId _ ident _) = pretty ident
    pretty (AttrId _ e1 e2)  = pretty e1 ++ "." ++ pretty e2

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

instance Pretty Expr where
    pretty (ECast pos ident expr)    = "(" ++ pretty ident ++ ")" ++ pretty expr
    pretty (ECastPrim pos t expr)    = "(" ++ pretty t ++ ")" ++ pretty expr
    pretty (ENewObject pos ident)    = "new " ++ pretty ident
    pretty (ENewArr pos t expr)      = "new " ++ pretty t ++ " " ++ "[" ++ pretty expr ++ "]"
    pretty (ENull pos)               = "null"
    pretty (EObject pos expr1 expr2) = pretty expr1 ++ "." ++ pretty expr2
    pretty (EArr pos ident expr)     = pretty ident ++ "[" ++ pretty expr ++ "]"
    pretty (EVar pos ident)          = "xddd" ++ pretty ident
    pretty (ELitInt pos integer)     = "siur" ++ show integer
    pretty (ELitTrue pos)            = "true"
    pretty (ELitFalse pos)           = "false"
    pretty (EString pos s)           = s
    pretty (EApp pos ident exprs)    = pretty ident ++ "(" ++ intercalate "," (map pretty exprs) ++ ")"
    pretty (Neg pos expr)            = "!" ++ pretty expr
    pretty (Not pos expr)            = "-" ++ pretty expr
    pretty (EMul pos expr1 op expr2) = "(" ++ pretty expr1 ++ ")" ++ pretty op ++ "(" ++ pretty expr1 ++ ")"
    pretty (EAdd pos expr1 op expr2) = "(" ++ pretty expr1 ++ ")" ++ pretty op ++ "(" ++ pretty expr1 ++ ")"
    pretty (ERel pos expr1 op expr2) = "(" ++ pretty expr1 ++ ")" ++ pretty op ++ "(" ++ pretty expr1 ++ ")"
    pretty (EAnd pos expr1 expr2)    = "(" ++ pretty expr1 ++ ") && (" ++ pretty expr1 ++ ")"
    pretty (EOr pos expr1 expr2)     = "(" ++ pretty expr1 ++ ") || (" ++ pretty expr1 ++ ")"

instance Pretty MulOp where
    pretty (Times _) = "*"
    pretty (Div _)   = "/"
    pretty (Mod _)   = "%"

instance Pretty AddOp where
    pretty (Plus _)  = "+"
    pretty (Minus _) = "-"

instance Pretty RelOp where
    pretty (LTH _) = "<"
    pretty (LE _)  = "<="
    pretty (GTH _) = ">"
    pretty (GE _)  = ">="
    pretty (EQU _) = "=="
    pretty (NE _)  = "!="


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
printExtIdent = pretty

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
printExpr = pretty

prettyPrint :: (Pretty a, MonadIO m) => a -> m ()
prettyPrint = liftIO . print . pretty
