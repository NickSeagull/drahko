module Drahko.Generate.Variable where

import Drahko.Generate.Name
import Drahko.Syntax (Expression (..))
import Relude

generate :: ToName a => a -> Expression
generate = Variable . toName
