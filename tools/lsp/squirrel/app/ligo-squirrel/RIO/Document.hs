{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module RIO.Document
  ( FetchEffort (..)

  , forceFetch
  , fetch
  , forceFetchAndNotify

  , invalidate
  , preload
  , load

  , initializeBuildGraph

  , tempDirTemplate
  , getTempPath

  , handleLigoFileChanged

  , wccForFilePath
  , invalidateWccFiles
  ) where

import Algebra.Graph.AdjacencyMap qualified as G
import Control.Lens ((??))
import Data.DList qualified as DList (toList)
import Data.HashSet qualified as HashSet
import Language.LSP.Server qualified as S
import Language.LSP.Types qualified as J
import Language.LSP.VFS qualified as V
import StmContainers.Map qualified as StmMap
import System.FilePath (splitDirectories, takeDirectory, (</>))
import UnliftIO.Async (pooledForConcurrently_)
import UnliftIO.Directory
  (Permissions (writable), createDirectoryIfMissing, doesDirectoryExist, doesFileExist,
  getPermissions, setPermissions)

import AST (ExtractionDepth (..), addShallowScopes, extractIncludedFiles, scanContracts)
import AST.Includes (insertPreprocessorRanges)
import AST.Parser (parsePreprocessed)
import AST.Scope.Common
import ASTMap qualified
import Cli (LigoClientEnv (..), TempDir (..), TempSettings (..), getLigoClientEnv)
import Language.LSP.Util (filePathToNormalizedUri, reverseUriMap, sendWarning)
import Log qualified
import ParseTree (Source (..), pathToSrc)
import Progress (Progress (..), noProgress, (%))
import RIO.Indexing (getIndexDirectory, indexOptionsPath, tryGetIgnoredPaths)
import RIO.Types (IndexOptions (..), LoadEffort (..), OpenDocument (..), RIO, RioEnv (..))
import Util.Graph (traverseAM, traverseAMConcurrently, wccFor)

-- | Represents how much effort a 'fetch' or 'forceFetch' operation should spend
-- trying to load a contract.
data FetchEffort
  = LeastEffort
  -- ^ Return whatever is available even if it's invalid. Fast but may be
  -- innacurate.
  | NormalEffort
  -- ^ Return a reasonably up-to-date document, but does some effort in loading
  -- if it's invalid. Tries to balance on being fast and accurate.
  | BestEffort
  -- ^ Fetch the latest possible document, avoiding invalid documents as much as
  -- possible. Slow but accurate.

fetch, forceFetch :: FetchEffort -> LoadEffort -> J.NormalizedUri -> RIO ContractInfo'
fetch effort loadEffort uri = Log.addContext (Log.sl "uri" $ J.fromNormalizedUri uri) do
  tmap <- asks reCache
  let
    fetchImpl = case effort of
      LeastEffort  -> ASTMap.fetchFast
      NormalEffort -> ASTMap.fetchCurrent
      BestEffort   -> ASTMap.fetchLatest
  fetchImpl uri loadEffort tmap
forceFetch = forceFetchAndNotify (const pass)

forceFetchAndNotify
  :: (ContractInfo' -> RIO ())
  -> FetchEffort
  -> LoadEffort
  -> J.NormalizedUri
  -> RIO ContractInfo'
forceFetchAndNotify notify effort loadEffort uri = Log.addContext (Log.sl "uri" $ J.fromNormalizedUri uri) do
  tmap <- asks reCache
  ASTMap.invalidate uri tmap
  case effort of
    LeastEffort  -> ASTMap.fetchFastAndNotify notify uri loadEffort tmap
    NormalEffort -> do
      v <- ASTMap.fetchCurrent uri loadEffort tmap
      v <$ notify v
    BestEffort   -> do
      v <- ASTMap.fetchLatest uri loadEffort tmap
      v <$ notify v

wccForFilePath :: FilePath -> RIO (G.AdjacencyMap FilePath)
wccForFilePath fp = do
  buildGraph <- readTVarIO =<< asks reBuildGraph
  -- It's possible that the file is ignored, so let's return a graph containing
  -- only this file.
  pure $ fromMaybe (G.vertex fp) $ wccFor fp . getIncludes $ buildGraph

invalidateWccFiles :: FilePath -> RIO ()
invalidateWccFiles fp = do
  tmap <- asks reCache
  -- Invalidate contracts that are in the same group as the deleted one, as
  -- references might have changed.
  void $ wccForFilePath fp >>= traverseAM \fp' ->
    ASTMap.invalidate (filePathToNormalizedUri fp') tmap

delete :: J.NormalizedFilePath -> RIO ()
delete nFp = do
  let fp = J.fromNormalizedFilePath nFp

  invalidateWccFiles fp

  buildGraphVar <- asks reBuildGraph
  atomically $ modifyTVar' buildGraphVar $ Includes . G.removeVertex fp . getIncludes

  tmap <- asks reCache
  void $ ASTMap.delete (J.normalizedFilePathToUri nFp) tmap

getInclusionGraph :: FilePath -> LoadEffort -> RIO (Includes ParsedContractInfo)
getInclusionGraph fp loadEffort = Log.addNamespace "getInclusionGraph" do
  currentComponent <-
    case loadEffort of
      NoLoading -> pure $ G.vertex fp
      DirectLoading -> do
        {- XXX: Since @#include@s get inlined into the file, we can optimize
        -- here and use just the current vertex, like in the case of
        -- 'NoIndexing'. This is also good for @ligo info get-scope@, as it also
        -- considers included files. When we deal with @#import@s, however,
        -- we'll need to think of a better way to handle this.
        --   The code commented below may be used if it's desirable to get all
        -- reachable components instead.
        Includes buildGraph <- readTVarIO =<< asks reBuildGraph
        pure $ reachableComponents fp buildGraph
        -}
        pure $ G.vertex fp
      FullLoading -> wccForFilePath fp
  Includes <$> traverseAMConcurrently parseContract currentComponent
  where
    parseContract :: FilePath -> RIO ParsedContractInfo
    parseContract contract = do
      let toParsedContractInfo = insertPreprocessorRanges <=< loadWithoutScopes
      $Log.debug [Log.i|Processing file: #{contract}|]
      toParsedContractInfo $ J.toNormalizedFilePath contract

updateBuildGraph :: J.NormalizedFilePath -> RIO ()
updateBuildGraph nFp = Log.addNamespace "updateBuildGraph" do
  rootWithoutScopes <- loadWithoutScopes nFp
  (_, DList.toList -> includeEdges) <- extractIncludedFiles DirectInclusions rootWithoutScopes
  let
    addVertexAndAdjacentEdges :: Ord a => a -> [(a, a)] -> G.AdjacencyMap a -> G.AdjacencyMap a
    addVertexAndAdjacentEdges v edgesV graph =
      let
        -- Add the current vertex as well as its inclusions. Calling
        -- `G.vertex v` is needed because `edgesV` will be empty if there are no
        -- inclusions.
        toInsert = G.vertex v `G.overlay` G.edges edgesV
        -- Deal with "back includes": re-insert edges that will be deleted by
        -- `G.removeVertex v graph` that should remain.
        backIncludes = G.edges $ map (, v) $ toList $ G.preSet v graph
      in
      G.removeVertex v graph `G.overlay` toInsert `G.overlay` backIncludes

  filePredicate <- getFilePredicate
  buildGraphVar <- asks reBuildGraph
  atomically $
    modifyTVar' buildGraphVar \(Includes buildGraph) ->
      let
        newBuildGraph =
          addVertexAndAdjacentEdges (J.fromNormalizedFilePath nFp) includeEdges buildGraph
      in
      -- Remove anything that should be ignored.
      Includes $ G.induce filePredicate newBuildGraph

initializeBuildGraph :: FilePath -> RIO ()
initializeBuildGraph root = Log.addNamespace "initializeBuildGraph" do
  S.withProgress "Indexing LIGO project" S.NotCancellable \reportProgress -> do
    filePredicate <- getFilePredicate
    dirContracts <- scanContracts filePredicate root

    let numContracts = length dirContracts
    progressRef <- newMVar 0
    pooledForConcurrently_ dirContracts \contract -> do
      updateBuildGraph $ J.toNormalizedFilePath contract
      progress <- takeMVar progressRef
      reportProgress $ S.ProgressAmount
        (Just (progress % numContracts))
        (Just [Log.i|Scanned #{contract}|])
      putMVar progressRef (progress + 1)

invalidate :: J.NormalizedUri -> RIO ()
invalidate uri = ASTMap.invalidate uri =<< asks reCache

-- | Attempts to load a file from the virtual file text from @lsp@, or from the
-- disk if it's not available. This function checks whether the file exists or
-- not and attempts to persist the file in a temporary directory if it doesn't.
-- This file also reads from 'reOpenDocs' in order to figure out whether the
-- file is dirty or not and set the appropriate value in the 'Source'.
preload :: J.NormalizedFilePath -> RIO Source
preload normFp = Log.addNamespace "preload" do
  let
    uri = J.normalizedFilePathToUri normFp
    fin = J.fromNormalizedFilePath normFp
    mkReadOnly path = do
      p <- getPermissions path
      setPermissions path p { writable = False }
    createTemp new = do
      $Log.warning [Log.i|#{fin} not found. Creating temp file: #{new}|]
      -- We make the file read-only so the user is aware that it should not be
      -- changed. Note we can't make fin read-only, since it doesn't exist in
      -- the disk anymore, and also because LSP has no such request.
      sendWarning
        [Log.i|File #{fin} was removed. The LIGO LSP server may not work as expected. Creating temporary file: #{new}|]
      new <$ mkReadOnly new
    handlePersistedFile = do
      tempMap <- asks reTempFiles
      let nFin = J.toNormalizedFilePath fin
      atomically (StmMap.lookup nFin tempMap) >>= \case
        Nothing -> do
          tempFile <-
            maybe (pure fin) createTemp =<< S.persistVirtualFile mempty uri
          let nTempFile = J.toNormalizedFilePath tempFile
          tempFile <$ atomically (StmMap.insert nTempFile nFin tempMap)
        Just nTempFile -> pure $ J.fromNormalizedFilePath nTempFile

  fin' <- bool handlePersistedFile (pure fin) =<< doesFileExist fin

  mvf <- S.getVirtualFile uri
  case mvf of
    -- FIXME: Opening a file through `virtualFileText` rather than `pathToSrc`
    -- will cause different behavior w.r.t. encodings. For instance, opening
    -- a UTF-16-encoded file that was not yet indexed will not show errors,
    -- while opening it after it was indexed will display various encoding
    -- errors.
    Just vf -> do
      openDocM <- atomically . StmMap.lookup uri =<< asks reOpenDocs
      pure $ Source fin' (maybe False odIsDirty openDocM) (V.virtualFileText vf)
    Nothing -> pathToSrc fin'

tempDirTemplate :: String
tempDirTemplate = ".ligo-work"

getTempPath :: FilePath -> RIO TempSettings
getTempPath fallbackPath = do
  opts <- readTVarIO =<< asks reIndexOpts
  let path = fromMaybe fallbackPath (indexOptionsPath opts)
  let tempDir = path </> tempDirTemplate
  createDirectoryIfMissing False tempDir
  pure $ TempSettings path $ UseDir tempDir

loadWithoutScopes :: J.NormalizedFilePath -> RIO ContractInfo
loadWithoutScopes normFp = Log.addNamespace "loadWithoutScopes" do
  src <- preload normFp
  ligoEnv <- getLigoClientEnv
  $Log.debug [Log.i|Running with path #{_lceClientPath ligoEnv}|]
  temp <- getTempPath $ takeDirectory $ J.fromNormalizedFilePath normFp
  parsePreprocessed temp src

getFilePredicate :: RIO (FilePath -> Bool)
getFilePredicate = do
  ignoredPathsM <- tryGetIgnoredPaths
  let
    notTemporary = not . any (tempDirTemplate `isPrefixOf`) . splitDirectories
    notIgnored = case ignoredPathsM of
      Just ignore
        | not (null ignore) -> not . (`HashSet.member` HashSet.fromList ignore)
      _ -> const True
  pure $ (&&) <$> notTemporary <*> notIgnored

load
  :: forall parser
   . HasScopeForest parser RIO
  => J.NormalizedUri
  -> LoadEffort
  -> RIO ContractInfo'
load uri indexEffort = Log.addNamespace "load" do
  let Just normFp = J.uriToNormalizedFilePath uri  -- FIXME: non-exhaustive
  rootIndex <- getIndexDirectory initializeBuildGraph (takeDirectory $ J.fromNormalizedFilePath normFp)
  let rootM = indexOptionsPath rootIndex
  dirExists <- maybe (pure False) doesDirectoryExist rootM

  -- Here we try to handle the case when the current root path ceased to exist
  -- (e.g.: it was renamed) but the file is still open in the editor.
  -- Both persisted file and dir exist: Normal situation
  -- Persisted file exists, dir doesn't: Root directory renamed
  -- Persisted file doesn't exist, dir does: Root directory restored
  -- Neither exist: something went wrong
  revUri <- reverseUriMap ?? uri
  let
    Just revNormFp = J.uriToNormalizedFilePath revUri  -- FIXME: non-exhaustive
    revFp = J.fromNormalizedFilePath revNormFp
    loadDefault temp = addShallowScopes @parser temp noProgress =<< loadWithoutScopes revNormFp
    loadWithoutIndexing = do
      temp <- getTempPath $ takeDirectory revFp
      loadDefault temp

  -- If we're trying to load an ignored file, then load it, but don't index
  -- anything else.
  shouldIgnoreFile <- maybe False (revFp `elem`) <$> tryGetIgnoredPaths
  if
    | shouldIgnoreFile -> do
      sendWarning [Log.i|Opening an ignored file. It will not be indexed and cross-file operations may not work as inteded. Change your .ligoproject file to index this file.|]
      loadWithoutIndexing
    | Just root <- rootM, dirExists -> do
      time <- ASTMap.getTimestamp

      rawGraph <- getInclusionGraph revFp indexEffort

      temp <- getTempPath root
      graph <- S.withProgress "Scoping project" S.NotCancellable \reportProgress ->
        addScopes @parser temp
          (\Progress{..} -> reportProgress $ S.ProgressAmount (Just pTotal) (Just pMessage))
          rawGraph

      let contracts = (id &&& filePathToNormalizedUri . contractFile) <$> G.vertexList (getIncludes graph)
      tmap <- asks reCache
      for_ contracts \(contract, nuri) ->
        ASTMap.insert nuri contract time tmap

      maybe
        (loadDefault temp)
        pure
        (flip lookupContract graph =<< J.uriToFilePath (J.fromNormalizedUri revUri))
    | otherwise -> do
      case rootIndex of
        IndexChoicePending -> $Log.debug [Log.i|Indexing directory has not been specified yet.|]
        _ -> pass
      loadWithoutIndexing

handleLigoFileChanged :: J.NormalizedFilePath -> J.FileChangeType -> RIO ()
handleLigoFileChanged nFp = \case
  J.FcCreated -> do
    updateBuildGraph nFp
    invalidateWccFiles fp
    void $ forceFetch BestEffort NoLoading uri
    $Log.debug [Log.i|Created #{fp}|]
  J.FcChanged -> do
    openDocs <- asks reOpenDocs
    mOpenDoc <- atomically $ StmMap.lookup uri openDocs
    updateBuildGraph nFp
    invalidateWccFiles fp
    whenNothing_ mOpenDoc do
      void $ forceFetch BestEffort NoLoading uri
      $Log.debug [Log.i|Changed #{fp}|]
  J.FcDeleted -> do
    delete nFp
    $Log.debug [Log.i|Deleted #{fp}|]
  where
    uri = J.normalizedFilePathToUri nFp
    fp = J.fromNormalizedFilePath nFp
