module Typechecker.Utils where

-- --------------------ENSURE------------------------------------------------------
-- ensureTypeMatch :: BNFC'Position -> Type -> Type -> TypeCheckerState ()
-- ensureTypeMatch pos type1 type2 =
--   if raw type1 == raw type2
--     then return ()
--     else throwException $ InvalidTypeExpectedException pos type2 type1

-- ensureType :: Type -> Expr -> TypeCheckerState ()
-- ensureType t expr = do
--   eType <- typeCheckExpr expr
--   ensureTypeMatch (hasPosition eType) t eType

-- ensureArgTypes :: BNFC'Position -> [ArgType] -> [Expr] -> TypeCheckerState ()
-- ensureArgTypes pos ts exprs =
--   if length ts /= length exprs
--     then throwException $ InvalidNumberOfParametersException pos
--     else zipWithM_ ensureArgType ts exprs

-- ensureArgType :: ArgType -> Expr -> TypeCheckerState ()
-- ensureArgType (Val _ t) expr = do
--   ensureType t expr

-- ensureArgType (Ref pos t) expr = do
--   case expr of
--     (EVar pos ident) -> do
--       env <- get
--       case getType env ident of
--         Nothing -> throwException $ UndefinedException pos ident
--         Just t1 -> if snd t1 then throwException $ ConstReferenceException pos else ensureTypeMatch (hasPosition $ fst t1) t (fst t1)
--     _ -> throwException $ ReferenceNotVariableException pos

-- ensureUniqueIdents :: [Arg] -> TypeCheckerState ()
-- ensureUniqueIdents args =
--   case firstDuplicateIndex $ map getArgIdent args of
--     Just index -> throwException $ RedeclarationException (hasPosition . getArgType $ args !! index) (getArgIdent $ args !! index)
--     Nothing -> return ()

-- -------------DONT ALLOW---------------------------------------------------------------
-- dontAllowTypeMatch :: BNFC'Position -> Type -> Type -> TypeCheckerState ()
-- dontAllowTypeMatch pos type1 type2 =
--   if cleanRaw type1 /= cleanRaw type2
--     then return ()
--     else throwException $ InvalidTypeException pos type2

-- dontAllowTypes :: [Type] -> Expr -> TypeCheckerState Type
-- dontAllowTypes ts expr = do
--   eType <- typeCheckExpr expr
--   mapM_ (dontAllowTypeMatch (hasPosition eType) eType) ts
--   return eType

-- dontAllowType :: Type -> Expr -> TypeCheckerState Type
-- dontAllowType t expr = do
--   eType <- typeCheckExpr expr
--   dontAllowTypeMatch (hasPosition eType) t eType
--   return t

-- dontAllowVoidArgs :: [ArgType] -> TypeCheckerState ()
-- dontAllowVoidArgs = mapM_ (dontAllowVoid . toType)

-- dontAllowVoid :: Type -> TypeCheckerState ()
-- dontAllowVoid (Fun _ args _) =
--   dontAllowVoidArgs args

-- dontAllowVoid t =
--   when (raw t == rawVoid) $ throwException $ VoidNotAllowedException (hasPosition t)

-- dontAllowFun :: Type -> Ident -> TypeCheckerState ()
-- dontAllowFun t ident =
--   when (cleanRaw t == rawFun) $ throwException $ FunctionNotDefinedException (hasPosition t) ident
-- ------------------FUNCTION STATE-----------------------------
-- functionState :: TypeCheckerS -> [Arg] -> Type -> TypeCheckerS
-- functionState s args rType = setTypes (emptyScope $ setExpectedReturnType s $ Just rType)
--                               (map getArgIdent args)
--                               (map (makeFalsyTuple . toType . getArgType) args)