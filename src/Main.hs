import           Abstract.Optimizer.Optimizer      (cleanDeadCode, optimize)
import           Abstract.Self                     (addSelf)
import           Abstract.Typechecker.TypeChecker  (checkReturn, typeCheck)
import           Compiler.Compiler                 (compile)
import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Control.Monad.Trans.Except        (ExceptT, runExceptT, throwE)
import           Data.Either                       (either)
import           Syntax.AbsLattepp                 (Program)
import           Syntax.LexLattepp                 (tokens)
import           Syntax.ParLattepp                 (myLexer, pProgram)
import           Syntax.PrintLattepp               (printTree)
import           System.Directory.Internal.Prelude (exitFailure, getArgs,
                                                    hPutStr, hPutStrLn, stderr,
                                                    stdout)
import           System.Exit                       (exitSuccess)
import           System.FilePath                   (replaceExtension,
                                                    takeBaseName, takeDirectory)
import           System.IO                         ()
import           System.Process                    (callCommand)
import           Utils                             (exitError, printProgram)
import qualified Utils                             as Syntax.AbsLattepp


tokenize :: String -> ExceptT String IO Program
tokenize s = case pProgram $ myLexer s of
  Left str   -> throwE str
  Right prog -> return prog

runProgram :: String -> ExceptT String IO String
runProgram s = do
  tokens <- tokenize s
  tcEnv <- typeCheck tokens
  optimized <- optimize tokens
  tcEnv2 <- checkReturn optimized
  liftIO $ hPutStrLn stderr "OK"
  selfed <- addSelf optimized
  liftIO $ writeFile "tokens.txt" $ printTree tokens
  liftIO $ writeFile "optimized.txt" $ printTree optimized
  liftIO $ writeFile "selfed.txt" $ printTree selfed
  compile selfed tcEnv

dump :: String -> String -> IO ()
dump file asm = do
  let name = replaceExtension file "s"
  let object = replaceExtension file "o"
  let binary = takeDirectory file ++ "/" ++ takeBaseName file
  writeFile name asm
  callCommand $ "nasm -g -f elf64 " ++ name
  callCommand $ "gcc -g -no-pie lib/runtime.o " ++ object ++ " -o " ++ binary
  callCommand $ "rm " ++ object

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      program <- readFile file
      result <- runExceptT $ runProgram program
      either exitError (dump file) result
    _ -> exitFailure

