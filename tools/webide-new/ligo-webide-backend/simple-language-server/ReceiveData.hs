module ReceiveData (receiveData) where

import Control.Arrow ((>>>))
import Control.Lens (to)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Aeson (encodePretty)
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text (decodeUtf8, encodeUtf8)
import Data.Text.Lazy.Builder qualified as Text (fromText)
import Katip (LogStr(..), Severity(DebugS), logFM)
import Language.LSP.Types (Method(..), NotificationMessage, Uri(..))
import Language.LSP.Types.Lens (params, text, textDocument, uri)
import Network.WebSockets (Connection)
import Network.WebSockets qualified as WS
import Prelude hiding (Key)
import System.Directory (createDirectoryIfMissing)
import System.IO (hFlush)

import Common (ConnectionM)
import Config (ConnectionConfig(..), ServerConfig(..))

receiveData :: Int -> Connection -> Handle -> ConnectionM ()
receiveData clientId conn stdinProducer = forever $ do
  workspacePrefix <- asks (scWorkspacePrefix . ccServerConfig)
  let prefix = workspacePrefix <> "/connection" <> show clientId <> "/"
  msg <- liftIO (WS.receiveData conn)
  logFM DebugS ("Received: " <> LogStr (Text.fromText (prettyJSON msg)))
  let prefixed = addPrefix prefix msg
  createMissingFiles prefixed
  liftIO (hPutStr stdinProducer (Text.decodeUtf8 (addContentLength prefixed)))
  liftIO (hFlush stdinProducer)

prettyJSON :: BS.ByteString -> Text
prettyJSON bs =
  case Aeson.decodeStrict bs of
    Just (obj :: Aeson.Value) ->
      decodeUtf8 (BSL.toStrict (Aeson.encodePretty obj))
    Nothing -> "<no parse>"

addContentLength :: BS.ByteString -> BS.ByteString
addContentLength msg =
  let len = BS.length msg
      header =
        Text.encodeUtf8 . Text.pack
        $ "Content-Length: " ++ show len ++ "\r\n\r\n"
   in BS.append header msg

addPrefix :: Text -> BS.ByteString -> BS.ByteString
addPrefix clientPrefix bs =
  case Aeson.decodeStrict bs of
    Just (val :: Aeson.Value) ->
      BSL.toStrict . Aeson.encode . modifyUri (addPrefixText clientPrefix) $ val
    Nothing -> bs

modifyUri :: (Text -> Text) -> Aeson.Value -> Aeson.Value
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

addPrefixText :: Text -> Text -> Text
addPrefixText prefix txt =
  if Text.take 7 txt == "file://"
  then "file://" <> prefix <> Text.drop 7 txt
  else error $ "couldn't parse " <> txt

updateKeyMap :: forall a. Key -> (a -> a) -> KeyMap a -> KeyMap a
updateKeyMap k f km = runIdentity (KeyMap.alterF (Identity . fmap f) k km)

createMissingFiles :: BS.ByteString -> ConnectionM BS.ByteString
createMissingFiles bs =
  case bs of
    (Aeson.decodeStrict @(NotificationMessage 'TextDocumentDidOpen) -> Just rm) -> do
      let filename = rm ^. (params . textDocument . uri . to getUri)
                   & Text.drop 7
          contents = rm ^. (params . textDocument . text)
      liftIO (createDirectoryIfMissing True (Text.unpack (getDir filename)))
      writeFile (Text.unpack filename) contents
      pure bs
    _ -> pure bs

getDir :: Text -> Text
getDir =
  Text.reverse
  >>> Text.dropWhile (/= '/')
  >>> Text.reverse
