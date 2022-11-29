{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Typechecker.Utils where
import           Control.Monad              (unless, when, zipWithM_)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (Except, ExceptT, runExceptT)
import           Control.Monad.Trans.State  (StateT (runStateT), evalStateT,
                                             get, gets, put)
import           Data.Function              (on)
import           Data.List                  (sort, sortBy)
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import           MonadUtils                 (MonadIO (liftIO), concatMapM)
import           Syntax.AbsLattepp          (Arg, BNFC'Position, ClassBlock,
                                             ClassBlock' (ClassBlock),
                                             ClassStmt,
                                             ClassStmt' (ClassDecl, ClassEmpty, ClassMethod),
                                             Expr, HasPosition (hasPosition),
                                             Ident (Ident), TopDef,
                                             TopDef' (ClassDef, ExtClassDef, FnDef),
                                             Type,
                                             Type' (ObjectType, Primitive))
import           Typechecker.Data           (ClassDefS,
                                             TypeCheckerException (..),
                                             TypeCheckerS (classEnv, funEnv),
                                             TypeCheckerState, addAttr,
                                             addAttrs, addClass, addFun,
                                             addMethod, emptyScope,
                                             setExpectedReturnType, setSelf,
                                             setTypes)
import           Utils                      (Raw (raw), firstDuplicateIndex,
                                             getArgIdent, getArgType, rawFun,
                                             rawInt, rawVoid, throwException)

functionState :: TypeCheckerS -> [Arg] -> Type -> TypeCheckerS
functionState s args rType = setTypes (emptyScope $ setExpectedReturnType s $ Just rType)
                              (map getArgIdent args) (map getArgType args)

classState :: TypeCheckerS -> Maybe Ident -> TypeCheckerS
classState = setSelf

ensureUniqueIdents :: [Arg] -> TypeCheckerState ()
ensureUniqueIdents args =
  case firstDuplicateIndex $ map getArgIdent args of
    Just index -> throwException $ ArgumentRedeclarationException (hasPosition $ args !! index) (getArgIdent $ args !! index)
    Nothing -> return ()

dontAllowVoidArgs :: [Type] -> TypeCheckerState ()
dontAllowVoidArgs = mapM_ dontAllowVoid

dontAllowVoid :: Type -> TypeCheckerState ()
dontAllowVoid t =
   when (raw t == rawVoid) $ throwException $ VoidNotAllowedException (hasPosition t)

gatherHeaders :: TopDef -> TypeCheckerState ()
gatherHeaders (FnDef pos ret ident args block) = do
   st <- get
   case M.lookup ident (funEnv st) of
     Nothing -> do
        dontAllowVoidArgs $ map getArgType args
        ensureUniqueIdents args
        put $ addFun st ret ident (map getArgType args)
     Just _ -> throwException $ FunctionRedeclarationException pos ident

gatherHeaders (ClassDef pos ident block) = do
   st <- get
   case M.lookup ident (classEnv st) of
      Nothing -> do
         put $ addClass st ident Nothing pos
      Just _ -> throwException $ ClassRedeclarationException pos ident

gatherHeaders (ExtClassDef pos ident ext block) = do
   st <- get
   case M.lookup ident (classEnv st) of
      Nothing -> do
         put $ addClass st ident Nothing pos
      Just _ -> throwException $ ClassRedeclarationException pos ident

checkMain :: TypeCheckerS -> TypeCheckerState ()
checkMain s = case M.lookup (Ident "main") (funEnv s) of
  Nothing -> throwException NoMainException
  Just (ret, args) ->
    if raw ret /= rawInt then
        throwException $ MainReturnTypeException (hasPosition ret) ret
    else unless (null args) $ throwException $ ArgumentInMainException (hasPosition $ head args)

defineInheritance :: TopDef -> TypeCheckerState ()
defineInheritance (FnDef pos ret ident args block) = return ()

defineInheritance (ClassDef pos ident block) = return ()

defineInheritance (ExtClassDef pos ident ext block) = do
   st <- get
   case M.lookup ext (classEnv st) of
      Nothing -> throwException $ UndefinedTypeException pos (ObjectType pos ext)
      Just _ -> put $ addClass st ident (Just ext) pos


_topoSort :: [(TopDef, Int)] -> TypeCheckerState [TopDef]
_topoSort defs = return $ map fst (sortBy (compare `on` snd) defs)

topoSort :: [TopDef] -> TypeCheckerState [TopDef]
topoSort defs = do
    env <- gets classEnv
    let classes = map (\(i, ((ext, pos, _), defs)) -> (i, ext, pos)) (M.toList env)
    checkCycle [] classes (head classes)
    depthMap <- prepareDepthMap
    _topoSort (map (`prepareDepth` depthMap) defs)

checkCycle :: [Ident] -> [(Ident, Maybe Ident, BNFC'Position)] -> (Ident, Maybe Ident, BNFC'Position) -> TypeCheckerState ()
checkCycle _ [] _ = return ()
checkCycle _ _ (_, Nothing, _) = return ()
checkCycle visited all (curr, Just ext, pos) = do
    if ext `elem` visited then
        throwException $ CircularInheritanceException pos curr ext
    else
        checkCycle (curr : visited) all (head $ filter (\(c, _, _) -> c == ext) all)

doLookups :: [Ident] -> TypeCheckerState [(Ident, [Ident])]
doLookups [] = return []
doLookups (i:is) = do
    env <- gets classEnv
    case M.lookup i env of
        Nothing -> undefined
        Just ((_, _, childs), _) -> do
            res <- doLookups is
            return $ (i, childs) : res

_prepareDepthMap :: Int -> (Ident, [Ident]) -> TypeCheckerState [(Ident, Int)]
_prepareDepthMap depth c@(i, []) = return [(i, depth)]
_prepareDepthMap depth c@(i, childs)  = do
    env <- gets classEnv
    lookups <- doLookups childs
    res <- concatMapM (_prepareDepthMap (depth + 1)) lookups
    return $ (i, depth) : res

prepareDepthMap :: TypeCheckerState (Map Ident Int)
prepareDepthMap = do
    env <- gets classEnv
    let grandpas = map (\(i, ((_, _, childs), _)) -> (i, childs)) $ filter (\c -> case c of
                                                                                    (_, ((Nothing, _, _), _)) -> True
                                                                                    _ -> False) (M.toList env)
    res <- concatMapM (_prepareDepthMap 0) grandpas
    return $ M.fromList res

prepareDepth :: TopDef -> Map Ident Int -> (TopDef, Int)
prepareDepth f@(FnDef pos ret ident args block) m = (f, 0)
prepareDepth c@(ClassDef pos ident block) m = (c, 0)
prepareDepth c@(ExtClassDef pos ident ext block) m =
    case M.lookup ident m of
        Nothing -> undefined
        Just n  -> (c, n)

gatherFields :: TopDef -> TypeCheckerState ()
gatherFields FnDef {}                          = return ()

gatherFields (ClassDef pos ident block)        = gatherFieldsBlock block ident

gatherFields (ExtClassDef pos ident ext block) = gatherFieldsBlock block ident

gatherFieldsBlock :: ClassBlock -> Ident -> TypeCheckerState ()
gatherFieldsBlock (ClassBlock pos stmts) c =
    mapM_ (`gatherFieldsStmt` c) stmts

gatherFieldsStmt :: ClassStmt -> Ident -> TypeCheckerState ()
gatherFieldsStmt (ClassEmpty _) _ = return ()

gatherFieldsStmt (ClassDecl pos t idents) c =
    addAttrs c t idents

gatherFieldsStmt (ClassMethod pos ret ident args _) c =
    addMethod c ret ident args
