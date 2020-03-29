module Drahko.Generate.Program where

import qualified Drahko.Generate.Name as Name
import qualified Drahko.Generate.TopLevel as TopLevel
import Drahko.Syntax
import qualified IRTS.Simplified as Idris
import qualified Idris.Core.TT as IdrisCore
import Relude

generate :: MonadIO m => [(IdrisCore.Name, Idris.SDecl)] -> m Program
generate definitions = do
  functions <- traverse TopLevel.generate definitions
  let mainName = Name.fromName (IdrisCore.sMN 0 "runMain")
  let start = Call (Variable mainName) []
  let statements = functions <> [start]
  pure $ Program statements
