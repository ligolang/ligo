module SendData (sendData) where

import Common (ConnectionM)
import Network.WebSockets qualified as WS
import FilePath (filterConnectionPrefix)
import Data.Text qualified as Text
import Katip (Severity (DebugS), logFM)
import Data.Text.IO (hGetChunk)
import Text.Regex (subRegex, mkRegex)

sendData :: WS.Connection -> Handle -> ConnectionM ()
sendData conn stdoutConsumer = forever $ do
  txt <- liftIO (stripContentLength <$> hGetChunk stdoutConsumer)
  unless (Text.null txt) $ do
    filtered <- filterConnectionPrefix txt
    logFM DebugS ("Sending: " <> show filtered)
    liftIO $ WS.sendTextData conn filtered

stripContentLength :: Text -> Text
stripContentLength txt =
  let str = Text.unpack txt
   in Text.pack (subRegex (mkRegex "Content-Length: [0-9]*\\s*") str "")
