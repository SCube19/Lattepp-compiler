{-# LANGUAGE FlexibleInstances #-}
module Utils where

import Syntax.AbsLattepp
import System.Directory.Internal.Prelude (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO ( stderr, hPutStrLn )
import Data.List ( intercalate )
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import qualified Data.Set as Set
import Control.Monad.Trans.State (StateT, get, put)
------------------------------GENERAL--------------------------------------------------------
exitError :: String -> IO ()
exitError e = do
    hPutStrLn stderr ("FATAL: " ++ e ++ " ¯\\_(ツ)_/¯")
    exitFailure

-- class Typical a where
--     toType :: a -> Type

-- getArgIdent :: Arg -> Ident
-- getArgIdent (Arg _ t ident) = ident

-- makeFalsyTuple :: a -> (a, Bool)
-- makeFalsyTuple x = (x, False)

-- throwException x = lift $ throwE $ show x

-- firstDuplicateIndex :: Ord a => [a] -> Maybe Int
-- firstDuplicateIndex xs = dup' xs Set.empty
--   where dup' [] _ = Nothing
--         dup' (x:xs) s = if Set.member x s
--                            then Just $ length s
--                            else dup' xs (Set.insert x s)

-- -------------PRETTY-------------------------------------------------------------------

-- prettyPosition :: BNFC'Position -> String
-- prettyPosition (Just (row, col)) = "position " ++ show row ++ ":" ++ show col
-- prettyPosition _ = "???"

-- prettyIdent :: Ident -> String
-- prettyIdent (Ident x) = x

-- prettyType :: Type -> String
-- prettyType (Int _) = "int"
-- prettyType (Str _) = "string"
-- prettyType (Bool _) = "bool"
-- prettyType (Void _) = "void"


-- ------------------------------------TYPE MANIP--------------------------------------------
-- class Raw a where
--     raw :: a -> a
--     cleanRaw :: a -> a
--     cleanRaw = raw

-- instance Raw Type where
--     raw (Int _) = rawInt
--     raw (Str _) = rawStr
--     raw (Bool _) = rawBool
--     raw (Fun _ args rType) = Fun Nothing (map raw args) (raw rType)
--     raw (Void _) = Void Nothing
--     cleanRaw x = raw x


-- rawInt = Int Nothing
-- rawChar = Char Nothing
-- rawStr = Str Nothing
-- rawBool = Bool Nothing
-- rawFun = Fun Nothing [] (Void Nothing)
-- rawVoid = Void Nothing