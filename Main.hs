import Syntax.AbsLattepp (Program)
import Syntax.LexLattepp (tokens)
import Syntax.ParLattepp (myLexer, pProgram)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Utils (exitError)
import System.Directory.Internal.Prelude (exitFailure, getArgs)
import System.Exit (exitSuccess)
import System.IO ()
import Optimizer.Optimizer (optimize)
import Typechecker.TypeChecker ( typeCheck )
import Compiler.Compiler (compile)


tokenize :: String -> ExceptT String IO Program
tokenize s = case pProgram $ myLexer s of
  Left str -> throwE str
  Right prog -> return prog

runProgram :: String -> ExceptT String IO String
runProgram s = do
  tokens <- tokenize s
  optimized <- optimize tokens
  typecheck <- typeCheck optimized
  compile optimized

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      program <- readFile file
      result <- runExceptT $ runProgram program
      either exitError putStrLn result
    _ -> exitFailure

test :: String -> IO ()
test s = do
  result <- runExceptT $ runProgram s
  either exitError print result