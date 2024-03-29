module Drahko.Generate.Program where

import Drahko.Generate.Common
import qualified Drahko.Generate.Name as Name
import qualified Drahko.Generate.TopLevel as TopLevel
import qualified Drahko.Generate.Variable as Variable
import Drahko.Syntax
import qualified IRTS.Lang as Idris
import qualified Idris.Core.TT as IdrisCore
import Relude

generate :: MonadIO m => [(IdrisCore.Name, Idris.LDecl)] -> m Program
generate definitions = do
  godClassName <- Name.random
  let unusedNames = definitions & map fst & toUnusedNames
  let mainName = IdrisCore.sMN 0 "runMain"
  let mainVar = Variable.generate mainName
  let start = Call (DotAccess (Variable godClassName) mainVar) []
  (allFunctions, unused) <-
    flip runStateT unusedNames $ do
      setUsed mainName -- we set it used because the call is generated already
      traverse TopLevel.generate definitions
  let functions = filterUnused allFunctions unused
  let godClass = Class godClassName Nothing functions
  let statements = [godClass, start]
  pure $ Program statements
