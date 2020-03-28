module Drahko.Generate.TopLevel where

import qualified Drahko.Generate.Block as Block
import Drahko.Generate.Common
import qualified Drahko.Generate.Name as Name
import Drahko.Syntax
import qualified IRTS.Lang as Idris (LDecl (..))
import qualified Idris.Core.TT as Idris (Name)
import Relude

generate :: MonadIO m => Name -> (Idris.Name, Idris.LDecl) -> m Statement
generate programName (functionName, Idris.LFun _ _ args definition) = do
  let funName = Name.generate functionName
  let funArgs = Name.generate <$> args
  let funBody = Function (Name "run") funArgs (copyFunArgs funArgs <> Block.generate Return definition)
  pure $ Class funName (Just programName) [funBody]
generate _ (_, Idris.LConstructor {}) =
  pure NoOp
