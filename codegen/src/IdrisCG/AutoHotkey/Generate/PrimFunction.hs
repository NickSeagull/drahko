module IdrisCG.AutoHotkey.Generate.PrimFunction where

import qualified IRTS.Lang as Idris (PrimFn (..))
import IdrisCG.AutoHotkey.Generate.Common
import qualified IdrisCG.AutoHotkey.Generate.Name as Name
import IdrisCG.AutoHotkey.Syntax (Expression (..))
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
