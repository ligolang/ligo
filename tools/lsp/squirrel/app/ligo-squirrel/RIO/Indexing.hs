module RIO.Indexing
  ( IndexOptions (..)
  , indexOptionsPath
  , prettyIndexOptions
  , getIndexDirectory
  ) where

import Control.Lens (view)
import Control.Monad (void)
import Control.Monad.Reader (asks)
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Language.LSP.Server qualified as S
import Language.LSP.Types qualified as J
import Language.LSP.Types.Lens qualified as J
import UnliftIO.Environment (lookupEnv)
import UnliftIO.Exception (displayException, tryIO)
import UnliftIO.MVar (tryPutMVar, tryReadMVar)
import UnliftIO.Process (CreateProcess (..), proc, readCreateProcess)

import Log qualified
import RIO.Types (IndexOptions (..), RIO, RioEnv (..))

indexOptionsPath :: IndexOptions -> Maybe FilePath
indexOptionsPath = \case
  IndexChoicePending -> Nothing
  DoNotIndex -> Nothing
  FromRoot path -> Just path
  FromGitProject path -> Just path
  FromLigoProject path -> Just path

prettyIndexOptions :: IndexOptions -> String
prettyIndexOptions = \case
  IndexChoicePending -> "Pending index choice"
  DoNotIndex -> "Do not index"
  FromRoot path -> path
  FromGitProject path -> path
  FromLigoProject path -> path

-- FIXME: The user choice is not updated right away due to a limitation in LSP.
-- Check the comment in `askForIndexDirectory` for more information.
getIndexDirectory :: FilePath -> RIO IndexOptions
getIndexDirectory contractDir = do
  indexVar <- asks reIndexOpts
  maybe (askForIndexDirectory contractDir) pure =<< tryReadMVar indexVar

-- TODO: Write config to root directory, if set
askForIndexDirectory :: FilePath -> RIO IndexOptions
askForIndexDirectory contractDir = do
  rootDirectoryM <- S.getRootPath
  gitDirectoryM <- mkGitDirectory
  let
    suggestions = catMaybes
      [ FromRoot <$> rootDirectoryM
      , FromGitProject <$> gitDirectoryM
      , Just DoNotIndex
      ]

  env <- lookupEnv "LIGO_ENV"

  if
    -- On tests we want the directory to be indexed and we assume it's set.
    | Just "testing" <- env
    , Just rootDirectory <- rootDirectoryM -> pure $ FromRoot rootDirectory
    -- Not a git directory, and has no root in VS Code. Do nothing.
    | [DoNotIndex] <- suggestions -> pure DoNotIndex
    -- Ask the user what to do.
    | otherwise -> do
      indexVar <- asks reIndexOpts
      -- Wait for user input before proceeding.
      void $ S.sendRequest J.SWindowShowMessageRequest
        (mkRequest suggestions)
        (void . tryPutMVar indexVar . handleParams gitDirectoryM rootDirectoryM)
      -- lsp provides not easy way to get the callback of a request and MVars
      -- will block indefinitely. We don't index for now with the hope that it
      -- will be indexed later.
      -- https://github.com/haskell/lsp/issues/405
      pure IndexChoicePending
  where
    mkGitDirectory :: RIO (Maybe FilePath)
    mkGitDirectory = tryIO (readCreateProcess git "") >>= either
      (\e -> Nothing <$ $(Log.warning) [Log.i|#{displayException e}|])
      -- The output includes a trailing newline, we remove it with `init`.
      (pure . Just . init)
      where
        git :: CreateProcess
        git = (proc "git" ["rev-parse", "--show-toplevel"])
          { cwd = Just contractDir
          }

    mkRequest :: [IndexOptions] -> J.ShowMessageRequestParams
    mkRequest suggestions = J.ShowMessageRequestParams
      { _xtype = J.MtInfo
      -- TODO: improve this message
      , _message = "The LIGO Language Server would like to know where to start indexing the project from."
      , _actions = Just $ J.MessageActionItem . T.pack . prettyIndexOptions <$> suggestions
      }

    handleParams
      :: Maybe FilePath  -- Git project
      -> Maybe FilePath  -- Root project
      -> Either J.ResponseError (Maybe J.MessageActionItem)
      -> IndexOptions
    handleParams gitProject rootProject = either
      (const DoNotIndex)
      (maybe DoNotIndex getPathFromChosenOption)
      where
        getPathFromChosenOption :: J.MessageActionItem -> IndexOptions
        getPathFromChosenOption (view J.title -> T.unpack -> chosen)
          | Just path <- gitProject
          , chosen == path = FromGitProject path
          | Just path <- rootProject
          , chosen == path = FromRoot path
          | otherwise = DoNotIndex
