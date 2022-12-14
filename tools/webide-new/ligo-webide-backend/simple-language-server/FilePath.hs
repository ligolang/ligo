module FilePath (addConnectionPrefix, filterConnectionPrefix) where

import Katip (logFM, Severity (DebugS))
import Common (ConnectionM)
import Config (ServerConfig (..), ConnectionConfig (..))
import Data.Text qualified as Text

type Uri = Text

uriToFilePath :: Uri -> FilePath
uriToFilePath = Text.unpack . Text.drop 7

filePathToUri :: FilePath -> Uri
filePathToUri fp = Text.pack ("file://" <> fp)

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
