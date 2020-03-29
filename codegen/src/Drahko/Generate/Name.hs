module Drahko.Generate.Name where

import qualified Data.Text as Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Drahko.Syntax (Name (..))
import qualified IRTS.Lang as Idris
import qualified Idris.Core.TT as Idris
import Relude

notAllowedSymbols :: String
notAllowedSymbols = "{}[]()"

fromVar :: Idris.LVar -> Name
fromVar (Idris.Glob idrisName) =
  fromName idrisName
fromVar (Idris.Loc idrisRegistry) =
  loc idrisRegistry

loc :: Int -> Name
loc i = new ("r" <> show i :: Text)

fromName :: Idris.Name -> Name
fromName idrisName =
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
