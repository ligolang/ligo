module Main (main) where

import Config (ServerConfig(..), readConfig)
import Katip
  (ColorStrategy(ColorIfTerminal), Severity(DebugS, InfoS, WarningS), Verbosity(V2), closeScribes,
  defaultScribeSettings, initLogEnv, mkHandleScribe, permitItem, registerScribe)
import LSP.Server (handlePendingConnection, toServerApp)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets.Connection (defaultConnectionOptions)
import Server (mkApp)

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
    let serverApp = toServerApp logEnv config handlePendingConnection
        app = websocketsOr defaultConnectionOptions serverApp (mkApp config)
     in run (scPort config) app
