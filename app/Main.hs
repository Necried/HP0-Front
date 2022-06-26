{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main where

import Control.Monad
import Control.Exception
import Data.Text.Prettyprint.Doc
import Text.Megaparsec
import System.Console.Haskeline
import System.Environment

import Parser
import PrettyPrint
import Lib

data MainProgramException = FileNameException String
  deriving anyclass Exception

instance Show MainProgramException where
  show (FileNameException ext) =
    "Expecting filename extension .p, instead found extension '" ++ ext ++ "'"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> interpreter
    [fileName] -> do
      let (name, ext) = span (/= '.') fileName
      when (ext /= ".p") $ throwIO $ FileNameException ext
      contents <- readFile fileName
      compileAndRun name contents
    _ -> putStrLn "Usage: stack run [file]"

interpreter :: IO ()
interpreter = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> outputStrLn "Exiting"
        Just ":q" -> outputStrLn "Exiting"
        Just input -> runInterpret input >> loop
