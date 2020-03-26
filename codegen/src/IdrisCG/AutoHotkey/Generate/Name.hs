module IdrisCG.AutoHotkey.Generate.Name where

import qualified Idris.Core.TT as Idris (Name, showCG)
import IdrisCG.AutoHotkey.Syntax (Name (..))
import Relude

notAllowedSymbols :: String
notAllowedSymbols = "{}[]()"

generate :: Idris.Name -> Name
generate idrisName = do
  let removeSymbols = filter (`notElem` notAllowedSymbols)
  let encodedName = Idris.showCG idrisName & removeSymbols
  Name (toText encodedName)

loc :: Int -> Name
loc i = Name ("loc" <> show i)
