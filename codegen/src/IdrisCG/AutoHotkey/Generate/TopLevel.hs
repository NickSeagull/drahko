module IdrisCG.AutoHotkey.Generate.TopLevel where

import qualified IRTS.Simplified as Idris (SDecl (..))
import qualified Idris.Core.TT as Idris (Name)
import qualified IdrisCG.AutoHotkey.Generate.Block as Block
import qualified IdrisCG.AutoHotkey.Generate.Name as Name
import IdrisCG.AutoHotkey.Syntax (Statement (..))
import Relude

generate :: (Idris.Name, Idris.SDecl) -> Statement
generate (functionName, Idris.SFun _ args _ definition) =
  Function (Name.generate functionName) (fmap Name.generate args) (Block.generate Return definition)
