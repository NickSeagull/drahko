module Drahko.Generate.Program where

import qualified Drahko.Generate.Name as Name
import qualified Drahko.Generate.TopLevel as TopLevel
import Drahko.Syntax
import qualified IRTS.Simplified as Idris
import qualified Idris.Core.TT as IdrisCore
import Relude

generate :: MonadIO m => [(IdrisCore.Name, Idris.SDecl)] -> m Program
generate definitions = do
  programName <- Name.random
  functions <- traverse (TopLevel.generate programName) definitions
  let mainName = Name.fromName (IdrisCore.sMN 0 "runMain")
  let start = Call (DotAccess (Variable programName) (DotAccess (Variable mainName) (Variable $ Name "run"))) []
  let mainClass = Class programName Nothing functions
  let statements = [mainClass, start]
  pure $ Program statements
