module Drahko.Generate.TopLevel where

import qualified Drahko.Generate.Block as Block
import Drahko.Generate.Common
import Drahko.Generate.Name
import Drahko.Syntax
import qualified IRTS.Lang as Idris
import qualified Idris.Core.TT as Idris
import Relude

generate :: MonadState UnusedNames m => MonadIO m => (Idris.Name, Idris.LDecl) -> m Statement
generate (_, Idris.LConstructor {}) = pure NoOp
generate (functionName, Idris.LFun _ _ args definition) = do
  let funName = toName functionName
  let funArgs = toName <$> args
  funBlock <- flip runReaderT funArgs $ Block.generate Return definition
  pure $ Function funName funArgs funBlock
