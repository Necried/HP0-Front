module Lib where

import Control.Monad.IO.Class
import Data.Text
import Prettyprinter
import System.Console.Haskeline
import System.Process
import Text.Megaparsec
import Text.Megaparsec.Error
import Text.Pretty.Simple

import Parser
import PrettyPrint
import CGWatv2
import Semant

compileAndRun :: String -> String -> IO ()
compileAndRun fileName s = do
  case runParser program "" (pack s) of
    Left err -> putStrLn $ errorBundlePretty err
    Right res -> case runSemant (checkProgram res) initEnv of
      Left semantErr -> print semantErr
      Right sAST ->
        let
          genCode = snd $ runCodeGen ((_funcTable . metaData) sAST) (genWASM sAST)
        in do
          putStrLn "AST debug: "
          pPrint res
          putStrLn "---------------"
          putStrLn "SemantAST debug: "
          pPrint sAST
          putStrLn "WASM output: "
          print genCode
          writeFile (fileName <> ".wat") $ show genCode
          callProcess "wat2wasm" [fileName <> ".wat", "-o", fileName <> ".wasm"]
          callProcess wasmerExecutable [fileName <> ".wasm"]
  where
    wasmerExecutable = "WasmerRuntime/target/debug/WasmerRuntime"

runInterpret :: String -> InputT IO ()
runInterpret = liftIO . compileAndRun "out"
