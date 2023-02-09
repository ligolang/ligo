module Main (main) where

import Control.Concurrent.Async (Async, cancel, wait, withAsync)
import Data.Text qualified as Text
import Katip
import Network.WebSockets qualified as WS
import System.Directory (removeDirectoryRecursive)
import System.Process
  (CreateProcess(std_err, std_in, std_out), StdStream(UseHandle), createPipe, createProcess, proc,
  terminateProcess, waitForProcess)
import UnliftIO (withRunInIO)

import Config (ServerConfig(..), ConnectionConfig (..), readConfig)
import Common (ServerM, ConnectionM, withConnectionId)
import FilePath (getConnectionPrefix)
import ReceiveData (receiveData)
import SendData (sendData)

main :: IO ()
main = do
  config <- readConfig

  -- set up logging
  let logLevel = case scVerbosity config of
        0 -> WarningS
        1 -> InfoS
        _ -> DebugS

  handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem logLevel) V2
  let makeLogEnv =
        registerScribe "stdout" handleScribe defaultScribeSettings
          =<< initLogEnv "webide-language-server" ""

  bracket makeLogEnv closeScribes $ \logEnv ->
    flip runReaderT config $
      runKatipContextT logEnv () "main" $
        runServer
          (Text.unpack (scHostname config))
          (scPort config)
          handlePendingConnection

runServer
  :: String
  -> Int
  -> (WS.PendingConnection -> ServerM ())
  -> ServerM ()
runServer host port pendingCallback = withRunInIO $ \unlift ->
  WS.runServer host port (unlift . pendingCallback)

handlePendingConnection :: WS.PendingConnection -> ServerM ()
handlePendingConnection pending = do
  conn <- liftIO $ WS.acceptRequest pending
  clientCounter <- asks scClientCounter
  connectionId <- atomicModifyIORef' clientCounter (\x -> (x+1,x+1))
  withConnectionId connectionId (handleConnection conn)

handleConnection :: WS.Connection -> ConnectionM ()
handleConnection conn = do
    connectionId <- asks ccId
    logFM InfoS ("Handling connection " <> show connectionId)

    logFM InfoS "Opening stdin"
    (stdinConsumer, stdinProducer) <- liftIO createPipe

    logFM InfoS "Opening stdout"
    (stdoutConsumer, stdoutProducer) <- liftIO createPipe

    let stderrFile :: FilePath
        stderrFile = "/dev/null"

    stderrProducer :: Handle <- openFile stderrFile AppendMode
    ligoPath <- asks (scLigoPath . ccServerConfig)

    let cmd :: CreateProcess
        cmd = (proc ligoPath ["lsp"])
          { std_in = UseHandle stdinConsumer
          , std_out = UseHandle stdoutProducer
          , std_err = UseHandle stderrProducer
          }

    logFM InfoS "Forking 'ligo lsp'"
    (_, _, _, ligoProcessHandle) <- liftIO (createProcess cmd)

    withAsync' (sendData conn stdoutConsumer) $ \sendAsync ->
      withAsync' (receiveData connectionId conn stdinProducer)$ \receiveAsync ->
        liftIO (wait receiveAsync)
        `catch` (\(_ :: WS.ConnectionException) -> do
           logFM InfoS "Client closed connection!"

           logFM InfoS "Waiting for 'ligo lsp' to end..."
           liftIO (terminateProcess ligoProcessHandle)
           liftIO (waitForProcess ligoProcessHandle)

           logFM InfoS "Canceling sendData"
           liftIO (cancel sendAsync)

           logFM InfoS "Canceling receiveData"
           liftIO (cancel receiveAsync)

           connectionPrefix <- getConnectionPrefix
           logFM InfoS $ "Flushing " <> show connectionPrefix
           liftIO $ removeDirectoryRecursive connectionPrefix
         )

    logFM InfoS ("Ending connection " <> show connectionId)

withAsync'
  :: ConnectionM a
  -> (Async a -> ConnectionM b)
  -> ConnectionM b
withAsync' x f = withRunInIO $ \unlift -> withAsync (unlift x) (unlift . f)
