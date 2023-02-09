module FilePath
  ( addConnectionPrefix
  , filterConnectionPrefix
  , getConnectionPrefix
  , modifyUri
  , normalizeUri
  ) where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson
import System.FilePath (normalise)
import Common (ConnectionM)
import Config (ServerConfig (..), ConnectionConfig (..))
import Data.Text qualified as Text

type Uri = Text

uriToFilePath :: Uri -> FilePath
uriToFilePath = Text.unpack . Text.drop 7

filePathToUri :: FilePath -> Uri
filePathToUri fp = Text.pack ("file://" <> fp)

normalizeUri :: Uri -> Uri
normalizeUri = filePathToUri . normalise . uriToFilePath

addConnectionPrefix :: FilePath -> ConnectionM FilePath
addConnectionPrefix fp = getConnectionPrefix <&> (<> fp)

filterConnectionPrefix :: Text -> ConnectionM Text
filterConnectionPrefix txt = do
  pref <- Text.pack <$> getConnectionPrefix
  pure (Text.replace pref "/" txt)

getConnectionPrefix :: ConnectionM FilePath
getConnectionPrefix = do
  workspacePrefix <- Text.unpack <$> asks (scWorkspacePrefix . ccServerConfig)
  connectionId <- asks ccId
  pure $ workspacePrefix <> "/connection" <> show connectionId <> "/"

modifyUri :: (Uri -> Uri) -> Aeson.Value -> Aeson.Value
modifyUri f = \case
  Aeson.Object keyMap ->
    keyMap
    & fmap (modifyUri f)
    & updateKeyMap "uri" (\case
       Aeson.String u -> Aeson.String (f u)
       x -> x
      )
    & Aeson.Object
  Aeson.Array arr -> Aeson.Array (fmap (modifyUri f) arr)
  x -> x

updateKeyMap :: Aeson.Key -> (a -> a) -> Aeson.KeyMap a -> Aeson.KeyMap a
updateKeyMap k f km = runIdentity (Aeson.alterF (Identity . fmap f) k km)
