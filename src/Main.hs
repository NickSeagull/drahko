{-# LANGUAGE DoAndIfThenElse #-}

module Main where

import IRTS.CodegenEmpty
import IRTS.Compiler
import Idris.AbsSyntax
import Idris.ElabDecls
import Idris.Main
import Idris.Options
import System.Environment
import System.Exit

data Opts
  = Opts
      { inputs :: [FilePath],
        output :: FilePath
      }

showUsage :: IO ()
showUsage = do
  putStrLn "Usage: idris-ahk <ibc-files> [-o <output-file>]"
  exitSuccess

getOpts :: IO Opts
getOpts =
  process (Opts [] "a.out") <$> getArgs
  where
    process opts ("-o" : o : xs) = process (opts {output = o}) xs
    process opts (x : xs) = process (opts {inputs = x : inputs opts}) xs
    process opts [] = opts

cgMain :: Opts -> Idris ()
cgMain opts = do
  elabPrims
  _ <- loadInputs (inputs opts) Nothing
  mainProg <- elabMain
  ir <- compile (Via IBCFormat "ahk") (output opts) (Just mainProg)
  runIO $ codegenEmpty ir

main :: IO ()
main = do
  _ <- getOpts
  showUsage
