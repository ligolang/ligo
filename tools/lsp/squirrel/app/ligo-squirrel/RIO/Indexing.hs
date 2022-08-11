module RIO.Indexing
  ( IndexOptions (..)
  , indexOptionsPath
  , prettyIndexOptions
  , ligoProjectName
  , tryGetIgnoredPaths
  , getIndexDirectory
  , handleProjectFileChanged
  ) where

import Control.Lens (view)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (eitherDecodeFileStrict', encodeFile)
import Data.Bool (bool)
import Data.Default (def)
import Data.List (inits)
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Language.LSP.Server qualified as S
import Language.LSP.Types qualified as J
import Language.LSP.Types.Lens qualified as J
import System.FilePath (joinPath, splitPath, takeDirectory, (</>))
import System.IO (IOMode (ReadMode), hFileSize, withFile)
import UnliftIO.Directory (canonicalizePath, findFile, withCurrentDirectory)
import UnliftIO.Exception (displayException, tryIO)
import UnliftIO.MVar (isEmptyMVar, putMVar, swapMVar, tryPutMVar, tryReadMVar, tryTakeMVar)
import UnliftIO.Process (CreateProcess (..), proc, readCreateProcess)
import Witherable (ordNubOn)

import Cli qualified
import Language.LSP.Util (sendError, sendInfo)
import Log qualified
import RIO.Types (IndexOptions (..), ProjectSettings (..), RIO, RioEnv (..))

indexOptionsPath :: IndexOptions -> Maybe FilePath
indexOptionsPath = \case
  IndexChoicePending -> Nothing
  DoNotIndex -> Nothing
  FromRoot path -> Just path
  FromGitProject path -> Just path
  FromLigoProject path _ -> Just path

prettyIndexOptions :: IndexOptions -> String
prettyIndexOptions = \case
  IndexChoicePending -> "Pending index choice"
  DoNotIndex -> "Do not index"
  FromRoot path -> path
  FromGitProject path -> path
  FromLigoProject path _ -> path

ligoProjectName :: FilePath
ligoProjectName = ".ligoproject"

projectIndexingMarkdownLink :: String
projectIndexingMarkdownLink =
  "[docs/project-indexing.md](https://gitlab.com/serokell/ligo/ligo/-/tree/tooling/tools/lsp/vscode-plugin/docs/project-indexing.md)"

-- | If there is a LIGO project file and the user has provided ignored paths,
-- then this function will return it.
tryGetIgnoredPaths :: RIO (Maybe [FilePath])
tryGetIgnoredPaths = do
  indexOptsM <- tryReadMVar =<< asks reIndexOpts
  pure case indexOptsM of
    Just (FromLigoProject _ ProjectSettings{psIgnorePaths}) -> psIgnorePaths
    _ -> Nothing

-- | Given a path /foo/bar/baz, check for a `.ligoproject` file in the specified
-- path and all of its parent directories, in this order, for the first matching
-- file found. Returns the directory where the file was found.
checkForLigoProjectFile :: FilePath -> RIO (Maybe FilePath)
checkForLigoProjectFile = liftIO
  . fmap (fmap takeDirectory)
  . flip findFile ligoProjectName
  . map joinPath . reverse . drop 1 . inits . splitPath

decodeProjectSettings :: FilePath -> RIO ProjectSettings
decodeProjectSettings projectDir = do
  eitherSettings <- liftIO $ eitherDecodeFileStrict' $ projectDir </> ligoProjectName
  case eitherSettings of
    Left err -> do
      $(Log.err) [Log.i|Failed to read project settings.\n#{err}|]
      sendError [Log.i|Failed to read project settings. Using default settings. Check the logs for more information.|]
      pure def
    Right settings -> do
      canonicalizedPaths <-
        withCurrentDirectory projectDir $
          traverse (traverse canonicalizePath) $ psIgnorePaths settings
      pure settings
        { psIgnorePaths = canonicalizedPaths
        }

upgradeProjectSettingsFormat :: FilePath -> RIO ()
upgradeProjectSettingsFormat projectPath = do
  size <- liftIO $ withFile projectPath ReadMode hFileSize
  when (size == 0) do
    sendInfo [Log.i|Found an old version of #{ligoProjectName}, upgrading to a new version. For more information, see #{projectIndexingMarkdownLink}.|]
    liftIO $ encodeFile projectPath $ def @ProjectSettings

-- FIXME: The user choice is not updated right away due to a limitation in LSP.
-- Check the comment in `askForIndexDirectory` for more information.
getIndexDirectory :: FilePath -> RIO IndexOptions
getIndexDirectory contractDir = do
  indexOptsVar <- asks reIndexOpts
  tryReadMVar indexOptsVar >>= \case
    Nothing -> do
      newOpts <- checkForLigoProjectFile contractDir >>= \case
        Nothing -> askForIndexDirectory contractDir
        Just ligoProjectDir -> do
          upgradeProjectSettingsFormat $ ligoProjectDir </> ligoProjectName
          projectSettings <- decodeProjectSettings ligoProjectDir
          let indexOpts = FromLigoProject ligoProjectDir projectSettings
          hasNoOpts <- isEmptyMVar indexOptsVar
          indexOpts <$ bool (void . swapMVar indexOptsVar) (putMVar indexOptsVar) hasNoOpts indexOpts

      -- A root directory was set; restart the daemon.
      newOpts <$ Cli.cleanupLigoDaemon
    Just opts -> pure opts

askForIndexDirectory :: FilePath -> RIO IndexOptions
askForIndexDirectory contractDir = do
  rootDirectoryM <- S.getRootPath
  gitDirectoryM <- mkGitDirectory
  let
    suggestions = ordNubOn indexOptionsPath $ catMaybes
      [ FromGitProject <$> gitDirectoryM
      , FromRoot <$> rootDirectoryM
      , Just DoNotIndex
      ]

  case suggestions of
    -- Not a git directory, and has no root in VS Code. Do nothing.
    [DoNotIndex] -> pure DoNotIndex
    -- Ask the user what to do.
    _ -> do
      indexVar <- asks reIndexOpts
      -- Wait for user input before proceeding.
      void $ S.sendRequest J.SWindowShowMessageRequest
        (mkRequest suggestions)
        (\response -> do
          let choice = handleParams gitDirectoryM rootDirectoryM response
          _ <- tryPutMVar indexVar choice
          handleChosenOption choice)
      -- FIXME (LIGO-490): lsp provides no easy way to get the callback of a
      -- request and MVars will block indefinitely. We don't index for now with
      -- the hope that it will be indexed later.
      -- See also:
      -- * https://github.com/haskell/lsp/issues/405
      -- * https://github.com/haskell/lsp/issues/409
      -- * https://github.com/haskell/lsp/issues/417
      pure IndexChoicePending
  where
    handleChosenOption :: IndexOptions -> RIO ()
    handleChosenOption (indexOptionsPath -> Just path) = do
      let
        projectPath = path </> ligoProjectName
        don'tCreate =
          sendInfo [Log.i|To remember the directory, create an empty #{ligoProjectName} file in #{path}|]
        doCreate = do
          liftIO $ encodeFile projectPath $ def @ProjectSettings
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
      , _message = [Log.i|Choose a directory to index LIGO files. See #{projectIndexingMarkdownLink} for more details.|]
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
    J.FcChanged -> [Log.i|Changed #{fp}|]
    J.FcDeleted -> [Log.i|Deleted #{fp}|]
