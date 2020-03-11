module IdrisCG.AutoHotkey.Generate.PrimFunction where

import qualified IRTS.Lang as Idris (PrimFn (..))
import qualified IdrisCG.AutoHotkey.Generate.Name as Name
import IdrisCG.AutoHotkey.Syntax (Expression (..))
import Relude

generate :: Idris.PrimFn -> [Expression] -> Expression
generate Idris.LWriteStr [_, str] =
  Apply (Variable "idris_putStr") [str]
generate (Idris.LExternal n) params =
  Apply (Variable $ Name.generate n) params
generate x _ =
  error ("\nPrimitive function not implemented \n\n\t" <> show x <> "\n")
