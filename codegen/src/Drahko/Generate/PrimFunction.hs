module Drahko.Generate.PrimFunction where

import qualified Drahko.Generate.Name as Name
import Drahko.Syntax
import qualified IRTS.Lang as Idris (PrimFn (..))
import Relude

generate :: Idris.PrimFn -> [Expression] -> Expression
generate Idris.LWriteStr [_, str] =
  Apply (primitiveFunction "putStr") [str]
generate (Idris.LExternal n) params =
  Apply (Variable $ Name.fromName n) params
generate Idris.LCrash args =
  Apply (primitiveFunction "crash") args
generate Idris.LStrConcat [lhs, rhs] =
  BinaryOperatorApply Concat lhs rhs
generate x _ =
  error ("\nPrimitive function not implemented \n\n\t" <> show x <> "\n")

primitiveFunction :: Text -> Expression
primitiveFunction nameText =
  DotAccess (Variable "__drahko") (Variable $ Name nameText)
