module Drahko.Generate.Variable where

import Drahko.Generate.Name
import Drahko.Syntax (Expression (..))
import Relude

generate :: ToName a => a -> Expression
generate = Variable . toName

thisScoped :: ToName a => a -> Expression
thisScoped name =
  DotAccess (generate ("this" :: Text)) (generate name)
