module Common
  ( WebIDEM
  , LSPM
  , ConnectionM
  , withConnectionId
  ) where

import Control.Monad.Trans.Control (liftWith)
import Control.Monad.Trans.Reader (withReaderT)
import Katip (KatipContextT, KatipT)

import Config (ConnectionConfig(..), ConnectionId, ServerConfig)
import Servant.Server.Internal.ServerError (ServerError)

type WebIDEM = KatipT (ReaderT ServerConfig (ExceptT ServerError IO))
type LSPM = KatipContextT (ReaderT ServerConfig IO)
type ConnectionM = KatipContextT (ReaderT ConnectionConfig IO)

withConnectionId :: ConnectionId -> ConnectionM a -> LSPM a
withConnectionId connId m =
  liftWith $ \run ->
    flip withReaderT (run m) $ \serverConfig ->
      ConnectionConfig
        { ccId = connId
        , ccServerConfig = serverConfig
        }
