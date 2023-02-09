module SendData (sendData) where

import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Aeson (encodePretty)
import Data.Aeson.Parser (json)
import Data.Attoparsec.ByteString qualified as AP
import Data.Attoparsec.ByteString.Char8 qualified as AP
import Data.Attoparsec.ByteString.Lazy (Result(Done, Fail), parse)
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy.Builder qualified as Text (fromText)
import Katip (LogStr(..), Severity(DebugS, ErrorS), logFM)
import Network.WebSockets qualified as WS

import Common (ConnectionM)
import FilePath (filterConnectionPrefix, normalizeUri, modifyUri)

sendData :: WS.Connection -> Handle -> ConnectionM ()
sendData conn stdoutConsumer = do
  bsl :: LBS.ByteString <- liftIO $ LBS.hGetContents stdoutConsumer
  parseAllInput conn bsl

parseAllInput :: WS.Connection -> LBS.ByteString -> ConnectionM ()
parseAllInput conn bsl =
  case parse contentLengthJson bsl of
    Fail _ _ err -> do
      logFM ErrorS $ "Couldn't parse message from language server: " <> show err
    Done cont val -> do
      let normalizedVal = modifyUri normalizeUri val
      let prettyVal =
            Text.decodeUtf8 $ LBS.toStrict $ Aeson.encodePretty normalizedVal
      logFM DebugS $ "Sending: " <> LogStr (Text.fromText prettyVal)

      let output = Text.decodeUtf8 . LBS.toStrict . Aeson.encode $ normalizedVal
      filtered <- filterConnectionPrefix output
      liftIO $ WS.sendTextData conn filtered
      parseAllInput conn cont

contentLengthJson :: AP.Parser Value
contentLengthJson = do
  void (AP.string "Content-Length: ")
  void (AP.manyTill AP.digit (AP.char '\r'))
  void (AP.string "\n\r\n")
  json
