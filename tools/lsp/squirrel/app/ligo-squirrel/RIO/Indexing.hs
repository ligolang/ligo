module RIO.Indexing
  ( IndexOptions (..)
  , indexOptionsPath
  , prettyIndexOptions
  , ligoProjectName
  , tryGetIgnoredPaths
  , getIndexDirectory
  , handleProjectFileChanged
  ) where

import Algebra.Graph.Class qualified as G (empty)
import Data.Aeson (eitherDecodeFileStrict', encodeFile)
import Data.Default (def)
import Language.LSP.Server qualified as S
import Language.LSP.Types qualified as J
import Language.LSP.Types.Lens qualified as J
import StmContainers.Map qualified as StmMap
import System.FilePath (joinPath, splitPath, takeDirectory, (</>))
import System.IO (hFileSize)
import UnliftIO.Directory (canonicalizePath, findFile, withCurrentDirectory)
import UnliftIO.Exception (tryIO)
import UnliftIO.Process (CreateProcess (..), proc, readCreateProcess)
import Unsafe qualified
import Witherable (ordNubOn)

import ASTMap qualified
import Cli qualified
import Language.LSP.Util (sendError, sendInfo)
import Log qualified
import RIO.Types (IndexOptions (..), ProjectSettings (..), RIO, RioEnv (..))

indexOptionsPath :: IndexOptions -> Maybe FilePath
indexOptionsPath = \case
  IndexOptionsNotSetYet -> Nothing
  IndexChoicePending -> Nothing
  DoNotIndex -> Nothing
  FromRoot path -> Just path
  FromGitProject path -> Just path
  FromLigoProject path _ -> Just path

prettyIndexOptions :: IndexOptions -> String
prettyIndexOptions = \case
  IndexOptionsNotSetYet -> "Index options were not set yet"
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
  indexOpts <- readTVarIO =<< asks reIndexOpts
  pure case indexOpts of
    FromLigoProject _ ProjectSettings{psIgnorePaths} -> psIgnorePaths
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
  let ligoProjectFile = projectDir </> ligoProjectName
  $Log.debug [Log.i|Using project file: #{ligoProjectFile}|]
  eitherSettings <- liftIO $ eitherDecodeFileStrict' ligoProjectFile
  case eitherSettings of
    Left err -> do
      $Log.err [Log.i|Failed to read project settings.\n#{err}|]
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
getIndexDirectory :: (FilePath -> RIO ()) -> FilePath -> RIO IndexOptions
getIndexDirectory directorySpecifyCallback contractDir = do
  indexOptsVar <- asks reIndexOpts
  readTVarIO indexOptsVar >>= \case
    IndexOptionsNotSetYet ->
      checkForLigoProjectFile contractDir >>= \case
        Nothing ->
          askForIndexDirectory
            (\dir -> restartStateDependingOnIndexOpts *> directorySpecifyCallback dir)
            contractDir
        Just ligoProjectDir -> do
          indexOpts <- useNewLigoProjectDir ligoProjectDir
          restartStateDependingOnIndexOpts
          directorySpecifyCallback ligoProjectDir
          pure indexOpts
    oldOpts -> checkForLigoProjectFile contractDir >>= \case
      -- FIXME: if .ligoproject is deleted, we may still use it as an old option
      Nothing -> pure oldOpts
      Just dirWithLigoProject -> do
        newOpts <- useNewLigoProjectDir dirWithLigoProject
        when (newOpts /= oldOpts) do
          restartStateDependingOnIndexOpts
          directorySpecifyCallback dirWithLigoProject
        return newOpts
    where
      useNewLigoProjectDir ligoProjectDir = do
        indexOptsVar <- asks reIndexOpts
        upgradeProjectSettingsFormat $ ligoProjectDir </> ligoProjectName
        projectSettings <- decodeProjectSettings ligoProjectDir
        let indexOpts = FromLigoProject ligoProjectDir projectSettings
        atomically $ writeTVar indexOptsVar indexOpts
        return indexOpts

      -- A root directory was set; restart the daemon and delete indexing cache.
      restartStateDependingOnIndexOpts = do
        Cli.cleanupLigoDaemon
        astMap <- asks reCache
        ASTMap.reset astMap
        tempFilesMap <- asks reTempFiles
        buildGraphVar <- asks reBuildGraph
        atomically $ do
          StmMap.reset tempFilesMap
          writeTVar buildGraphVar G.empty

askForIndexDirectory :: (FilePath -> RIO ()) -> FilePath -> RIO IndexOptions
askForIndexDirectory directorySpecifyCallback contractDir = do
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
          atomically $ writeTVar indexVar choice
          whenJust (indexOptionsPath choice) directorySpecifyCallback
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
    handleChosenOption _ = pass

    mkGitDirectory :: RIO (Maybe FilePath)
    mkGitDirectory = tryIO (readCreateProcess git "") >>= either
      (\e -> Nothing <$ $Log.warning [Log.i|#{displayException e}|])
      -- The output includes a trailing newline, we remove it with `init`.
      (pure . Just . Unsafe.init)
      where
        git :: CreateProcess
        git = (proc "git" ["rev-parse", "--show-toplevel"])
          { cwd = Just contractDir
          }

    mkRequest :: [IndexOptions] -> J.ShowMessageRequestParams
    mkRequest suggestions = J.ShowMessageRequestParams
      { _xtype = J.MtInfo
      , _message = [Log.i|Choose a directory to index LIGO files. See #{projectIndexingMarkdownLink} for more details.|]
      , _actions = Just $ J.MessageActionItem . toText . prettyIndexOptions <$> suggestions
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
        getPathFromChosenOption (view J.title -> toString -> chosen)
          | Just path <- gitProject
          , chosen == path = FromGitProject path
          | Just path <- rootProject
          , chosen == path = FromRoot path
          | otherwise = DoNotIndex

handleProjectFileChanged :: (FilePath -> RIO ()) -> J.NormalizedFilePath -> J.FileChangeType -> RIO ()
handleProjectFileChanged directorySpecifyCallback nfp change = do
  -- We update indexing settings variable each time some project file was changed.
  lastActiveFile <- readTVarIO =<< asks reActiveFile
  whenJust lastActiveFile $
    void . getIndexDirectory directorySpecifyCallback . J.fromNormalizedFilePath

  let fp = J.fromNormalizedFilePath nfp
  $Log.debug case change of
    J.FcCreated -> [Log.i|Created #{fp}|]
    J.FcChanged -> [Log.i|Changed #{fp}|]
    J.FcDeleted -> [Log.i|Deleted #{fp}|]
