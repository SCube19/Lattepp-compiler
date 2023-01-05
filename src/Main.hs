import           Compiler.Compiler                 (compile)
import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Control.Monad.Trans.Except        (ExceptT, runExceptT, throwE)
import           Optimizer.Optimizer               (cleanDeadCode, optimize)
import           Syntax.AbsLattepp                 (Program)
import           Syntax.LexLattepp                 (tokens)
import           Syntax.ParLattepp                 (myLexer, pProgram)
import           System.Directory.Internal.Prelude (exitFailure, getArgs,
                                                    hPutStr, hPutStrLn, stderr)
import           System.Exit                       (exitSuccess)
import           System.IO                         ()
import           Typechecker.TypeChecker           (checkReturn, typeCheck)
import           Utils                             (exitError)


tokenize :: String -> ExceptT String IO Program
tokenize s = case pProgram $ myLexer s of
  Left str   -> throwE str
  Right prog -> return prog

runProgram :: String -> ExceptT String IO String
runProgram s = do
  tokens <- tokenize s
  typeCheck tokens
  --optimized <- optimize tokens
  cleaned <- cleanDeadCode tokens --optimized
  checkReturn cleaned
  liftIO $ hPutStrLn stderr "OK"
  compile cleaned

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      program <- readFile file
      result <- runExceptT $ runProgram program
      either exitError putStrLn result
    _ -> exitFailure

