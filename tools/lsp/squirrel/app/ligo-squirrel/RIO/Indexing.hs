module RIO.Indexing
  ( IndexOptions (..)
  , indexOptionsPath
  , prettyIndexOptions
  , ligoProjectName
  , getIndexDirectory
  , handleProjectFileChanged
  ) where

import Control.Applicative ((<|>))
import Control.Lens (view)
import Control.Monad (void)
import Control.Monad.Reader (asks)
import Control.Monad.IO.Class (liftIO)
import Data.Bool (bool)
import Data.List (inits)
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Language.LSP.Server qualified as S
import Language.LSP.Types qualified as J
import Language.LSP.Types.Lens qualified as J
import System.FilePath (joinPath, splitPath, takeDirectory, (</>))
import UnliftIO.Directory (findFile)
import UnliftIO.Environment (lookupEnv)
import UnliftIO.Exception (displayException, tryIO)
import UnliftIO.MVar (tryPutMVar, tryReadMVar, tryTakeMVar)
import UnliftIO.Process (CreateProcess (..), proc, readCreateProcess)
import Witherable (ordNubOn)

import Language.LSP.Util (sendInfo)
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

ligoProjectName :: FilePath
ligoProjectName = ".ligoproject"

-- | Given a path /foo/bar/baz, check for a `.ligoproject` file in the specified
-- path and all of its parent directories, in this order, for the first matching
-- file found. Returns the directory where the file was found.
checkForLigoProjectFile :: FilePath -> RIO (Maybe FilePath)
checkForLigoProjectFile = liftIO
  . fmap (fmap takeDirectory)
  . flip findFile ligoProjectName
  . map joinPath . reverse . drop 1 . inits . splitPath

-- FIXME: The user choice is not updated right away due to a limitation in LSP.
-- Check the comment in `askForIndexDirectory` for more information.
getIndexDirectory :: FilePath -> RIO IndexOptions
getIndexDirectory contractDir = do
  indexOptsM <- tryReadMVar =<< asks reIndexOpts
  ligoProjectFileM <- checkForLigoProjectFile contractDir
  maybe (askForIndexDirectory contractDir) pure (indexOptsM <|> fmap FromLigoProject ligoProjectFileM)

askForIndexDirectory :: FilePath -> RIO IndexOptions
askForIndexDirectory contractDir = do
  rootDirectoryM <- S.getRootPath
  gitDirectoryM <- mkGitDirectory
  let
    suggestions = ordNubOn indexOptionsPath $ catMaybes
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
        (\response -> do
          let choice = handleParams gitDirectoryM rootDirectoryM response
          _ <- tryPutMVar indexVar choice
          handleChosenOption choice)
      -- lsp provides no easy way to get the callback of a request and MVars
      -- will block indefinitely. We don't index for now with the hope that it
      -- will be indexed later.
      -- https://github.com/haskell/lsp/issues/405
      pure IndexChoicePending
  where
    handleChosenOption :: IndexOptions -> RIO ()
    handleChosenOption (indexOptionsPath -> Just path) = do
      let
        projectPath = path </> ".ligoproject"
        don'tCreate =
          sendInfo [Log.i|To remember the directory, create an empty .ligoproject file in #{path}|]
        doCreate = do
          liftIO $ writeFile projectPath ""
          sendInfo [Log.i|Created #{projectPath}|]
      void $ S.sendRequest J.SWindowShowMessageRequest
        J.ShowMessageRequestParams
          { _xtype = J.MtInfo
          , _message = "Create LIGO Language Server project file?"
          , _actions = Just [J.MessageActionItem "Yes", J.MessageActionItem "No"]
          }
        (\(fmap (fmap (view J.title)) -> choice) ->
          bool don'tCreate doCreate (choice == Right (Just "Yes")))
    handleChosenOption _ = pure ()

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
      , _message = "Choose a directory to index LIGO files. See [docs/project-indexing.md](https://gitlab.com/serokell/ligo/ligo/-/tree/tooling/tools/lsp/vscode-plugin/docs/project-indexing.md) for more details."
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

handleProjectFileChanged :: J.NormalizedFilePath -> J.FileChangeType -> RIO ()
handleProjectFileChanged nfp change = do
  indexOptsVar <- asks reIndexOpts
  -- Regardless of the change, try to empty this MVar and let the project
  -- indexing mechanism take care of it.
  -- FIXME: Should probably invalidate contracts as well. Changing this file
  -- should hopefully be uncommon enough that taking care of it is not worth
  -- the trouble.
  _ <- tryTakeMVar indexOptsVar
  let fp = J.fromNormalizedFilePath nfp
  $(Log.debug) case change of
    J.FcCreated -> [Log.i|Created #{fp}|]
    -- XXX: Should not trigger, see note in registerFileWatcher.
    J.FcChanged -> [Log.i|Changed #{fp}|]
    J.FcDeleted -> [Log.i|Deleted #{fp}|]
