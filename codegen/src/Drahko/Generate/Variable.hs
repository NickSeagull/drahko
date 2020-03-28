module Drahko.Generate.Variable where

import qualified Drahko.Generate.Name as Name
import Drahko.Syntax (Expression (..))
import qualified IRTS.Lang as Idris (LVar (..))

generate :: Idris.LVar -> Expression
generate var =
  case var of
    Idris.Loc i -> Variable (Name.loc i)
    Idris.Glob n -> Variable (Name.generate n)
