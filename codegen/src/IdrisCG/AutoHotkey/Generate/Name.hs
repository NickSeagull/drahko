module IdrisCG.AutoHotkey.Generate.Name where

import qualified Data.Text as Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Idris.Core.TT as Idris (Name, showCG)
import IdrisCG.AutoHotkey.Syntax (Name (..))
import Relude

notAllowedSymbols :: String
notAllowedSymbols = "{}[]()"

generate :: Idris.Name -> Name
generate idrisName =
  new (Idris.showCG idrisName)

new :: ToText a => a -> Name
new s = do
  let removeSymbols =
        Text.filter (`notElem` notAllowedSymbols)
  let replaceDots =
        Text.replace "." "__"
  let replaceDashes =
        Text.replace "-" "_"
  let encodedName =
        toText s
          & removeSymbols
          & replaceDots
          & replaceDashes
  Name encodedName

random :: MonadIO m => m Name
random = do
  uuid <- liftIO UUID.nextRandom
  pure (new $ "f" <> UUID.toText uuid)

loc :: Int -> Name
loc i = Name ("loc" <> show i)
