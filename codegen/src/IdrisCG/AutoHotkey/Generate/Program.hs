module IdrisCG.AutoHotkey.Generate.Program where

import qualified IRTS.Lang as Idris
import qualified Idris.Core.TT as IdrisCore
import qualified IdrisCG.AutoHotkey.Generate.Name as Name
import qualified IdrisCG.AutoHotkey.Generate.TopLevel as TopLevel
import qualified IdrisCG.AutoHotkey.Syntax as Syntax
import Relude

generate :: [(IdrisCore.Name, Idris.LDecl)] -> Syntax.Program
generate definitions = do
  let functions = fmap TopLevel.generate definitions
  let mainName = Name.generate (IdrisCore.sMN 0 "runMain")
  let start = Syntax.Call mainName []
  let statements = functions <> [start]
  Syntax.Program statements
