module Drahko.Generate.TopLevel where

import qualified Drahko.Generate.Block as Block
import qualified Drahko.Generate.Name as Name
import Drahko.Syntax
import qualified IRTS.Simplified as Idris
import qualified Idris.Core.TT as Idris
import Relude

generate :: MonadIO m => Name -> (Idris.Name, Idris.SDecl) -> m Statement
generate programName (functionName, Idris.SFun _ args _ definition)
  | Idris.showCG functionName `elem` ignoredTopLevels = pure NoOp
  | otherwise = do
    let funName = Name.fromName functionName
    let funArgs = Name.fromName <$> args
    funBlock <- Block.generate Return definition
    pure $ Function funName funArgs funBlock

ignoredTopLevels :: [String]
ignoredTopLevels =
  primitiveFunctions
    <> [ "unsafePerformPrimIO",
         "run__IO",
         "call__IO",
         "mkForeignPrim",
         "idris_crash",
         "assert_unreachable"
       ]
  where
    primitiveFunctions =
      map
        ("prim__" <>)
        [ "writeFile",
          "vm",
          "stdout",
          "stdin",
          "stderr",
          "sizeofPtr",
          "registerPtr",
          "readFile",
          "readChars",
          "ptrOffset",
          "pokeSingle",
          "pokePtr",
          "pokeDouble",
          "poke8",
          "poke64",
          "poke32",
          "poke16",
          "peekSingle",
          "peekPtr",
          "peekDouble",
          "peek8",
          "peek64",
          "peek32",
          "peek16",
          "null",
          "managedNull",
          "eqPtr",
          "eqManagedPtr",
          "asPtr"
        ]
