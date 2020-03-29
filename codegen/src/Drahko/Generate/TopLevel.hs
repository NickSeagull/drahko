module Drahko.Generate.TopLevel where

import qualified Drahko.Generate.Block as Block
import Drahko.Generate.Common
import qualified Drahko.Generate.Name as Name
import Drahko.Syntax
import qualified IRTS.Lang as Idris (LDecl (..))
import qualified Idris.Core.TT as Idris
import Relude

generate :: MonadIO m => Name -> (Idris.Name, Idris.LDecl) -> m Statement
generate programName (functionName, Idris.LFun _ _ args definition)
  | Idris.showCG functionName `elem` ignoredTopLevels = pure NoOp
  | otherwise = do
    putStrLn (funHeader functionName args)
    let funName = Name.generate functionName
    let funArgs = Name.generate <$> args
    funBlock <- Block.generate Return definition
    let funBody = Function (Name "run") funArgs (copyFunArgs funArgs <> funBlock)
    let result = Class funName (Just programName) [funBody]
    putTextLn "}"
    pure result
generate _ (_, Idris.LConstructor {}) =
  pure NoOp

funHeader name args = Idris.showCG name <> " " <> show args <> "{"

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
