module RIO
  ( module RIO.Types
  , newRioEnv
  , initializeRio
  , run

  , updateCustomConfig
  ) where

import Algebra.Graph.Class qualified as G (empty)
import Control.Monad (void)
import Control.Monad.Reader (asks, runReaderT)
import Data.Aeson qualified as Aeson (Result (..), Value, fromJSON)
import Data.Default (def)
import Data.HashSet qualified as HashSet
import Data.String.Interpolate.IsString (i)
import Language.LSP.Server qualified as S
import Language.LSP.Types qualified as J
import StmContainers.Map (newIO)
import UnliftIO.MVar (newEmptyMVar, newMVar, tryPutMVar, tryReadMVar, tryTakeMVar)

import AST (Fallback)
import ASTMap qualified
import Config (Config (..), getConfigFromNotification)
import Log (LogT)
import Log qualified
import RIO.Document qualified (load)
import RIO.Registration qualified
import RIO.Types (Contract (..), RIO (..), RioEnv (..), getCustomConfig)

newRioEnv :: IO RioEnv
newRioEnv = do
  reConfig <- newEmptyMVar
  reCache <- ASTMap.empty $ RIO.Document.load @Fallback
  reOpenDocs <- newMVar HashSet.empty
  reIncludes <- newMVar G.empty
  reTempFiles <- newIO
  reIndexOpts <- newEmptyMVar
  reBuildGraph <- newMVar G.empty
  pure RioEnv {..}

initializeRio :: RIO ()
initializeRio = do
  RIO.Registration.registerDidChangeConfiguration
  void fetchCustomConfig
  RIO.Registration.registerFileWatcher

-- Fetch the configuration from the server and write it to the Config MVar
fetchCustomConfig :: RIO (Maybe Config)
fetchCustomConfig = Log.addNamespace "fetchCustomConfig" do
  _ <- S.sendRequest J.SWorkspaceConfiguration configRequestParams handleResponse
  tryReadMVar =<< asks reConfig
  where
    configRequestParams =
      J.ConfigurationParams (J.List [J.ConfigurationItem Nothing Nothing])

    handleResponse
        :: Either J.ResponseError (J.ResponseResult 'J.WorkspaceConfiguration)
        -> RIO ()
    handleResponse response = do
      config <- parseResponse response
      mConfig <- asks reConfig
      _oldConfig <- tryTakeMVar mConfig
      void $ tryPutMVar mConfig config

    parseResponse
      :: Either J.ResponseError (J.ResponseResult 'J.WorkspaceConfiguration)
      -> RIO Config
    parseResponse (Right (J.List [value])) = do
      case Aeson.fromJSON value of
        Aeson.Success c -> pure c
        Aeson.Error _err -> useDefault
    parseResponse _ = useDefault

    useDefault :: RIO Config
    useDefault = do
      $(Log.warning)
        "Couldn't parse config from server, using default"
      pure def

updateCustomConfig :: Aeson.Value -> RIO ()
updateCustomConfig config = Log.addNamespace "updateCustomConfig" do
  mConfig <- asks reConfig
  tryTakeMVar mConfig >>= \case
    Nothing -> decodeConfig def mConfig
    Just oldConfig -> decodeConfig oldConfig mConfig
  where
    decodeConfig old mConfig = case getConfigFromNotification old config of
      Left err -> do
        $(Log.err) [i|Failed to decode configuration: #{err}|]
        maybe (void $ tryPutMVar mConfig old) (const $ pure ()) =<< fetchCustomConfig
      Right newConfig -> do
        $(Log.debug) [i|Set new configuration: #{newConfig}|]
        void $ tryPutMVar mConfig newConfig

run :: (S.LanguageContextEnv Config, RioEnv) -> RIO a -> LogT IO a
run (lcEnv, env) (RIO action) = S.runLspT lcEnv $ runReaderT action env
