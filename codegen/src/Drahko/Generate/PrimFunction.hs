module Drahko.Generate.PrimFunction where

import Drahko.Generate.Common
import qualified Drahko.Generate.Name as Name
import Drahko.Syntax (Expression (..))
import qualified IRTS.Lang as Idris (PrimFn (..))
import Relude

generate :: Idris.PrimFn -> [Expression] -> Expression
generate Idris.LWriteStr [_, str] =
  Apply (Variable "idris_putStr") [str]
generate (Idris.LExternal n) params =
  Apply (thisDot $ Variable $ Name.generate n) params
generate Idris.LCrash args =
  Apply (Variable "idris_crash") args
generate x _ =
  error ("\nPrimitive function not implemented \n\n\t" <> show x <> "\n")