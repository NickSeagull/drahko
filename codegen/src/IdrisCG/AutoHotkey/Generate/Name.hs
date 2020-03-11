module IdrisCG.AutoHotkey.Generate.Name where

import qualified Idris.Core.TT as Idris (Name, showCG)
import IdrisCG.AutoHotkey.Syntax (Name (..))
import Relude
import Text.Encoding.Z (zEncodeString)

generate :: Idris.Name -> Name
generate idrisName = do
  let encodedName = zEncodeString (Idris.showCG idrisName)
  Name ("idris_" <> toText encodedName)

loc :: Int -> Name
loc i = Name ("loc" <> show i)
