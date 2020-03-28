module Main where

import Drahko
import IRTS.Compiler
import Idris.AbsSyntax
import Idris.ElabDecls
import Idris.Main
import Idris.Options
import Relude
import System.Environment

data Opts
  = Opts
      { inputs :: [FilePath],
        output :: FilePath
      }

showUsage :: IO ()
showUsage = do
  putTextLn "Usage: idris-ahk <ibc-files> [-o <output-file>]"
  exitSuccess

getOpts :: IO Opts
getOpts =
  process (Opts [] "output.ahk") <$> getArgs
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
  runIO $ codegenAHK ir

main :: IO ()
main = do
  opts <- getOpts
  if null (inputs opts)
    then showUsage
    else runMain (cgMain opts)
