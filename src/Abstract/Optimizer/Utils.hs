module Abstract.Optimizer.Utils where

import           Data.Foldable     (Foldable (foldl'))
import qualified Data.Set          as S
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

isELitString :: Expr -> Bool
isELitString (EString  _ _) = True
isELitString _              = False

isELitBool :: Expr -> Bool
isELitBool (ELitTrue _)  = True
isELitBool (ELitFalse _) = True
isELitBool _             = False

isEString :: Expr -> Bool
isEString (EString _ _) = True
isEString _             = False

isLiteral x = isELitBool x || isELitInt x || isELitString x

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

getDefsAndChanges :: [Stmt] -> (S.Set Ident, S.Set Ident) -> (S.Set Ident, S.Set Ident)
getDefsAndChanges stmts dc = foldl' (flip getDefOrChange) dc stmts

getDefOrChange :: Stmt -> (S.Set Ident, S.Set Ident) -> (S.Set Ident, S.Set Ident)
getDefOrChange s dc@(defs, changes) = case s of
  Empty ma                 -> dc
  BStmt ma (Block _ stmts) -> combineSetTuples dc $ getDefsAndChanges stmts dc
  Decl ma ty its           -> (S.union defs $ S.fromList $ map getDef its, changes)
  Ass ma ei ex             ->
    case ei of
        Id _ i -> (defs, if S.member i defs then changes else S.insert i changes)
        _ -> dc
  Incr ma ei               ->
        case ei of
        Id _ i -> (defs, if S.member i defs then changes else S.insert i changes)
        _ -> dc
  Decr ma ei               ->
        case ei of
        Id _ i -> (defs, if S.member i defs then changes else S.insert i changes)
        _ -> dc
  Ret ma ex                -> dc
  VRet ma                  -> dc
  Cond ma ex st            -> combineSetTuples dc $ getDefOrChange st dc
  CondElse ma ex st st'    ->
    let res1 = getDefOrChange st dc in
    let res2 = getDefOrChange st' res1 in
    combineSetTuples dc res2
  While ma ex st           -> combineSetTuples dc $ getDefOrChange st dc
  For ma ty id ei st       -> combineSetTuples dc $ getDefOrChange st dc
  SExp ma ex               -> dc

getDef :: Item -> Ident
getDef (Init _ i _) = i
getDef (NoInit _ i) = i

combineSetTuples :: Ord a => (S.Set a, S.Set a) -> (S.Set a, S.Set a) -> (S.Set a, S.Set a)
combineSetTuples (a, b) (c, d) = (S.union a c, S.union b d)








