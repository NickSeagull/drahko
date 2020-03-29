module Drahko.Generate.Variable where

import qualified Drahko.Generate.Name as Name
import Drahko.Syntax (Expression (..))
import qualified IRTS.Lang as Idris (LVar (..))

generate :: Idris.LVar -> Expression
generate var =
  Variable (Name.fromVar var)
