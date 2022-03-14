module RIO.Types
  ( Contract (..)
  , RioEnv (..)
  , RIO (..)

  , getCustomConfig
  ) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, asks, mapReaderT)
import Control.Monad.Trans (lift)
import Data.Default (def)
import Data.HashSet (HashSet)
import Katip (Katip (..), KatipContext (..))
import Language.LSP.Server qualified as S
import Language.LSP.Types qualified as J
import StmContainers.Map qualified as StmMap
import UnliftIO.MVar (MVar, tryReadMVar)

import AST (ContractInfo', Includes, ParsedContractInfo)
import ASTMap (ASTMap)
import Cli (HasLigoClient (..), LigoClientEnv (..))
import Config (Config (..))
import Log (LogT)
import Log qualified

data Contract = Contract
  { cTree :: ContractInfo'
  , cDeps :: [J.NormalizedUri]
  }

data RioEnv = RioEnv
  { reConfig :: MVar Config
  , reCache :: ASTMap J.NormalizedUri Contract RIO
  , reOpenDocs :: MVar (HashSet J.NormalizedUri)
  , reIncludes :: MVar (Includes ParsedContractInfo)
  , reTempFiles :: StmMap.Map J.NormalizedFilePath J.NormalizedFilePath
  }

-- TODO: The lsp library provides no way to update the Config in the LspM monad
-- manually. So we have to maintain our own config to store the result of
-- `workspace/configuration` requests. We should get this fixed by the
-- maintainers, if possible.
getCustomConfig :: RIO Config
getCustomConfig = Log.addNamespace "getCustomConfig" do
  mConfig <- asks reConfig
  contents <- tryReadMVar mConfig
  case contents of
    -- TODO: The lsp library only sends the `workspace/configuration` request
    -- after initialization is complete, so during initialization, there is no
    -- way to know the client configuration. We need to get this fixed by the
    -- library maintainers, if possible.
    Nothing -> do
      $(Log.warning) "No config fetched yet, resorting to default"
      pure def
    Just config -> pure config

newtype RIO a = RIO
  { unRio :: ReaderT RioEnv (S.LspT Config (LogT IO)) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader RioEnv
    , MonadUnliftIO
    , S.MonadLsp Config.Config
    )

instance Katip RIO where
  getLogEnv = RIO $ lift $ lift getLogEnv
  localLogEnv f = RIO . mapReaderT (S.LspT . localLogEnv f . S.unLspT) . unRio

instance KatipContext RIO where
  getKatipContext = RIO $ lift $ lift getKatipContext
  localKatipContext f = RIO . mapReaderT (S.LspT . localKatipContext f . S.unLspT) . unRio
  getKatipNamespace = RIO $ lift $ lift getKatipNamespace
  localKatipNamespace f = RIO . mapReaderT (S.LspT . localKatipNamespace f . S.unLspT) . unRio

instance HasLigoClient RIO where
  getLigoClientEnv = fmap (LigoClientEnv . _cLigoBinaryPath) getCustomConfig
