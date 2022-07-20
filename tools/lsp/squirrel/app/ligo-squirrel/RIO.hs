module RIO
  ( module RIO.Types
  , newRioEnv
  , initializeRio
  , run

  , fetchConfig
  , setConfigFromJSON
  ) where

import Algebra.Graph.Class qualified as G (empty)
import Control.Monad (void)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (Value, Result (Success, Error), fromJSON)
import Data.HashSet qualified as HashSet
import Language.LSP.Server qualified as S
import Language.LSP.Types qualified as J
import StmContainers.Map (newIO)
import UnliftIO.MVar (newEmptyMVar, newMVar)

import AST (Standard)
import ASTMap qualified
import Config (Config (..))
import Log (LogT, i)
import Log qualified
import RIO.Document qualified (load)
import RIO.Registration qualified
import RIO.Types (Contract (..), RIO (..), RioEnv (..))

newRioEnv :: IO RioEnv
newRioEnv = do
  reCache <- ASTMap.empty $ RIO.Document.load @Standard
  reOpenDocs <- newMVar HashSet.empty
  reIncludes <- newMVar G.empty
  reTempFiles <- newIO
  reIndexOpts <- newEmptyMVar
  reBuildGraph <- newMVar G.empty
  pure RioEnv {..}

initializeRio :: RIO ()
initializeRio = do
  RIO.Registration.registerDidChangeConfiguration
  RIO.Registration.registerFileWatcher

  -- A note on configuration initialization: If the client decides to send the
  -- configuration on initialization, then the lsp library will call
  -- onChangeConfiguration to set the config on the server side. However, the
  -- client is not required to do this, and indeed VSCode does not.
  --
  -- So the next best thing we can do is to request and set the configuration
  -- ourselves as soon as possible. The first handler that lsp calls is the
  -- doInitialize handler, but this handler doesn't run in the LspM monad, so we
  -- can't send requests to the client there. The next handler that is called is
  -- the initialized handler, which does run in the LspM monad, so we can request
  -- the configuration here, and that is what we do.
  --
  -- Unfortunately, it turns out that if a LIGO file is already open when VSCode
  -- is launched, it will be parsed by the language server before the
  -- configuration is initialized. This might be because of the way the lsp
  -- library schedules requests. When fetchConfig is called, the request for the
  -- config is added to some queue in the lsp library, behind existing requests
  -- from the client, so it's not actually executed until after those requests
  -- are handled.
  fetchConfig

-- Fetch the configuration from the client and set it on the server.
fetchConfig :: RIO ()
fetchConfig =
  let params = J.ConfigurationParams (J.List [J.ConfigurationItem Nothing Nothing])
   in void $ S.sendRequest J.SWorkspaceConfiguration params
        $ \case
            Right (J.List [value]) -> setConfigFromJSON value
            err -> $(Log.warning) [i|Client did not provide config, instead got: #{err}|]

-- Parse a Config from a JSON object and set it as the configuration on the server.
setConfigFromJSON :: Value -> RIO ()
setConfigFromJSON value =
  case fromJSON value of
    Success c -> S.setConfig c
    Error err -> $(Log.warning) [i|Could not parse config #{err}|]

run :: (S.LanguageContextEnv Config, RioEnv) -> RIO a -> LogT IO a
run (lcEnv, env) (RIO action) = S.runLspT lcEnv $ runReaderT action env
