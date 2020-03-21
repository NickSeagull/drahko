module IdrisCG.AutoHotkey.Generate.TopLevel where

import qualified IRTS.Lang as Idris (LDecl (..))
import qualified Idris.Core.TT as Idris (Name)
import qualified IdrisCG.AutoHotkey.Generate.Block as Block
import qualified IdrisCG.AutoHotkey.Generate.Name as Name
import IdrisCG.AutoHotkey.Syntax (Statement (..))
import Relude

generate :: (Idris.Name, Idris.LDecl) -> Statement
generate (functionName, Idris.LFun _ _ args definition) =
  Function (Name.generate functionName) (fmap Name.generate args) (Block.generate Return definition)
generate (_, Idris.LConstructor {}) =
  NoOp
