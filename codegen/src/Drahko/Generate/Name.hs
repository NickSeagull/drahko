module Drahko.Generate.Name where

import qualified Data.Text as Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Drahko.Syntax (Name (..))
import qualified IRTS.Lang as Idris
import qualified Idris.Core.TT as Idris
import Relude

class ToName a where
  toName :: a -> Name

instance ToName Idris.LVar where
  toName (Idris.Glob idrisName) =
    toName idrisName
  toName (Idris.Loc idrisRegistry) =
    toName idrisRegistry

instance ToName Int where
  toName i =
    toName (registerPrefix <> show i)

instance ToName Idris.Name where
  toName idrisName =
    toName (Idris.showCG idrisName)

instance ToName String where
  toName = toName . toText

instance ToName Text where
  toName s = do
    let removeSymbols =
          Text.filter (`notElem` notAllowedSymbols)
    let replaceDots =
          Text.replace "." "__"
    let replaceDashes =
          Text.replace "-" "_"
    let encodedName =
          removeSymbols s
            & replaceDots
            & replaceDashes
    Name encodedName

random :: MonadIO m => m Name
random = do
  uuid <- liftIO UUID.nextRandom
  pure (toName $ programPrefix <> UUID.toText uuid)

notAllowedSymbols :: String
notAllowedSymbols = "{}[]()"

registerPrefix :: Text
registerPrefix = "r"

programPrefix :: Text
programPrefix = "f"
