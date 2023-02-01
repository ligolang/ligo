module RIO.Types
  ( IndexOptions (..)
  , LoadEffort (..)
  , OpenDocument (..)
  , ProjectSettings (..)
  , RioEnv (..)
  , RIO (..)
  ) where

import Control.Lens (_head)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (mapReaderT)
import Data.Aeson (Options (..), defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Char (toLower)
import Data.Default (Default (def))
import Katip (Katip (..), KatipContext (..))
import Language.LSP.Server qualified as S
import Language.LSP.Types qualified as J
import StmContainers.Map qualified as StmMap
import UnliftIO.Pool (Pool)

import AST (ContractInfo', Includes)
import ASTMap (ASTMap)
import Cli (HasLigoClient (..), LigoClientEnv (..), LigoProcess)
import Config (Config (..))
import Log (LogT)

-- | Records information regarding documents that are currently open by the user
-- in their editor.
newtype OpenDocument = OpenDocument
  { odIsDirty :: Bool
  -- ^ Whether this document is dirty (not saved) in the editor.
  }

-- | Represents the user's choice on how to index the project.
data IndexOptions
  = IndexOptionsNotSetYet
  -- ^ Waiting for 'getIndexDirectory' to set correct 'IndexOptions'.
  | IndexChoicePending
  -- ^ The choice was not yet processed and is pending. Only the currently
  -- opened contract is indexed.
  | DoNotIndex
  -- ^ The project should not be indexed. Like when the choice is pending, only
  -- the currently opened contract is indexed.
  | FromRoot FilePath
  -- ^ Index the project starting from the root directory. That is, the
  -- directory that is currently open in Visual Studio Code, if any.
  | FromGitProject FilePath
  -- ^ Index the project from the output of @git rev-parse --show-toplevel@, if
  -- Git is set.
  | FromLigoProject FilePath ProjectSettings
  -- ^ Index from the directory where the first @.ligoproject@ file is found, if
  -- it exists. This option has precedence over all others, and if this file is
  -- present, all other options will be ignored.
  deriving stock (Eq, Show)

-- | Indicates the depth of loading that should be done. This affects how many
-- files in the current WCC should be re-parsed and re-scoped.
data LoadEffort
  = NoLoading
  -- ^ Only load the current document. Doesn't try to fetch the files that were
  -- @#include@d, but because of the LIGO preprocessor, might load them anyway.
  | DirectLoading
  -- ^ Load the current document, as well as its transitive inclusions. This
  -- means that the directly included files, as well as all indirect inclusions,
  -- will be loaded. Note that, due to an optimization, we currently treat this
  -- option just like 'NoLoading' by loading only the current document.
  | FullLoading
  -- ^ Load all LIGO files in the given WCC. This operation is very costly, so
  -- use it only when needed.
  deriving stock (Eq, Ord)

-- | Stores information about the current language server environment, such as
-- loaded files, files in the project, etc. This is meant to be used inside a
-- `ReaderT`, and its internal `TVar`s updated as needed.
data RioEnv = RioEnv
  { reCache :: ASTMap J.NormalizedUri ContractInfo' LoadEffort RIO
  -- ^ Caches parsed and scoped contracts, as well as their include dependencies.
  -- Also contains metadata about contracts, such as when they were loaded, when
  -- they were invalidated, etc.
  , reOpenDocs :: StmMap.Map J.NormalizedUri OpenDocument
  -- ^ Records which files are current open in the editor, and keeps track of
  -- extra information, such as whether that file is dirty.
  , reTempFiles :: StmMap.Map J.NormalizedFilePath J.NormalizedFilePath
  -- ^ Provides a way to look which temporary files correspond to which open files.
  , reIndexOpts :: TVar IndexOptions
  -- ^ Stores the user's choice (or lack of) in how the project should be indexed.
  , reBuildGraph :: TVar (Includes FilePath)
  -- ^ Represents the build graph for all files that were looked up.
  , reLigo :: IORef (Maybe (Pool LigoProcess))
  -- ^ The spawned LIGO processes. This might be 'Nothing' in case the daemon
  -- was not initialized yet.
  , reActiveFile :: TVar (Maybe J.NormalizedFilePath)
  -- ^ Last file mentioned by LSP client. Can be `Nothing` only right after
  -- initializing (when no files were mentioned yet).
  }

newtype ProjectSettings = ProjectSettings
  { psIgnorePaths :: Maybe [FilePath]
  } deriving stock (Eq, Show)

instance Default ProjectSettings where
  def = ProjectSettings
    { psIgnorePaths = Nothing
    }

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

instance MonadFail RIO where
  fail = RIO . lift . lift . lift . fail

instance Katip RIO where
  getLogEnv = RIO $ lift $ lift getLogEnv
  localLogEnv f = RIO . mapReaderT (S.LspT . localLogEnv f . S.unLspT) . unRio

instance KatipContext RIO where
  getKatipContext = RIO $ lift $ lift getKatipContext
  localKatipContext f = RIO . mapReaderT (S.LspT . localKatipContext f . S.unLspT) . unRio
  getKatipNamespace = RIO $ lift $ lift getKatipNamespace
  localKatipNamespace f = RIO . mapReaderT (S.LspT . localKatipNamespace f . S.unLspT) . unRio

instance HasLigoClient RIO where
  getLigoClientEnv = do
    _lceClientPath <- _cLigoBinaryPath <$> S.getConfig
    _lceLigoProcesses <- readIORef =<< asks reLigo
    pure LigoClientEnv{..}

$(deriveJSON defaultOptions
  { fieldLabelModifier = over _head toLower . drop 2, omitNothingFields = True
  }
  ''ProjectSettings)
