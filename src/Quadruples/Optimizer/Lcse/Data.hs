module Quadruples.Optimizer.Lcse.Data where
import           Control.Monad.Except (ExceptT)
import           Control.Monad.State  (StateT, gets, modify)
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe)
import           Quadruples.Data      (Quadruple (Add, Div, Mod, Mul, Sub),
                                       Register)

type LcseState = StateT LcseS (ExceptT String IO)
data LcseS = LcseS {
    quadAcc     :: [Quadruple],
    expressions :: M.Map String Register,
    regMapping  :: M.Map Register Register
}

initLcseS :: LcseS
initLcseS = LcseS {
    quadAcc = [],
    expressions = M.empty,
    regMapping = M.empty
}

addAcc :: Quadruple -> LcseState ()
addAcc q = modify (\s -> LcseS {
    quadAcc = q : quadAcc s,
    expressions = expressions s,
    regMapping = regMapping s
})

addExpression :: Quadruple -> LcseState ()
addExpression q = do
    let entries = getMapEntries q
    mapM_ (\(k, v) -> modify (\s -> LcseS {
        quadAcc = quadAcc s,
        expressions = M.insert k v (expressions s),
        regMapping = regMapping s
    })) entries

addMapping :: Register -> Register -> LcseState ()
addMapping r1 r2 = modify (\s -> LcseS {
    quadAcc = quadAcc s,
    expressions = expressions s,
    regMapping = M.insert r1 r2 (regMapping s)
})

getMapEntries :: Quadruple -> [(String, Register)]
getMapEntries e@(Add r1 r2 res) = [(makeKey e, res), (makeKey $ Add r2 r1 res, res)]
getMapEntries e@(Sub r1 r2 res) = [(makeKey e, res)]
getMapEntries e@(Div r1 r2 res) = [(makeKey e, res)]
getMapEntries e@(Mul r1 r2 res) = [(makeKey e, res), (makeKey $ Mul r2 r1 res, res)]
getMapEntries e@(Mod r1 r2 res) = [(makeKey e, res)]
getMapEntries _ = []

clearState :: LcseState ()
clearState = modify (\s -> LcseS {
    quadAcc = quadAcc s,
    expressions = expressions s,
    regMapping = M.empty
})

getExpression :: Quadruple -> LcseState (Maybe Register)
getExpression q = do
    exprs <- gets expressions
    return $ M.lookup (makeKey q) exprs

makeKey :: Quadruple -> String
makeKey (Add r1 r2 res) = "add" ++ show r1 ++ show r2
makeKey (Sub r1 r2 res) = "sub" ++ show r1 ++ show r2
makeKey (Div r1 r2 res) = "div" ++ show r1 ++ show r2
makeKey (Mul r1 r2 res) = "mul" ++ show r1 ++ show r2
makeKey (Mod r1 r2 res) = "mod" ++ show r1 ++ show r2
makeKey _               = ""


getMapping :: Register -> M.Map Register Register -> Register
getMapping reg m = fromMaybe reg (M.lookup reg m)
