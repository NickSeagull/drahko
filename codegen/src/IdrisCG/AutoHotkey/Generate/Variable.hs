module IdrisCG.AutoHotkey.Generate.Variable where

import qualified IRTS.Lang as Idris (LVar (..))
import qualified IdrisCG.AutoHotkey.Generate.Name as Name
import IdrisCG.AutoHotkey.Syntax (Expression (..))

generate :: Idris.LVar -> Expression
generate var =
  case var of
    Idris.Loc i -> Variable (Name.loc i)
    Idris.Glob n -> Variable (Name.generate n)
