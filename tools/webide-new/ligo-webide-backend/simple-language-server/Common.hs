module Common (ServerM, ConnectionM, withConnectionId) where

import Config (ServerConfig, ConnectionConfig (..), ConnectionId)
import Katip (KatipContextT)
import Control.Monad.Trans.Reader (withReaderT)
import Control.Monad.Trans.Control (liftWith)

type ServerM = KatipContextT (ReaderT ServerConfig IO)
type ConnectionM = KatipContextT (ReaderT ConnectionConfig IO)

withConnectionId :: ConnectionId -> ConnectionM a -> ServerM a
withConnectionId connId m =
  liftWith $ \run ->
    flip withReaderT (run m) $ \serverConfig ->
      ConnectionConfig
        { ccId = connId
        , ccServerConfig = serverConfig
        }
