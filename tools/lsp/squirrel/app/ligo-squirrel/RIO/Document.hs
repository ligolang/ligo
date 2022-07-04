{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module RIO.Document
  ( Contract (..)
  , FetchEffort (..)

  , forceFetch
  , fetch
  , forceFetchAndNotify
  , forceFetch'
  , fetch'

  , delete
  , invalidate
  , preload
  , load

  , tempDirTemplate
  , getTempPath

  , handleLigoFileChanged
  ) where

import Algebra.Graph.AdjacencyMap qualified as G hiding (overlays)
import Algebra.Graph.Class qualified as G hiding (overlay, vertex)
import Control.Arrow ((&&&))
import Control.Lens ((??))
import Control.Monad (join, void, (<=<))
import Control.Monad.Reader (asks)
import Data.Bool (bool)
import Data.Foldable (find, for_, toList)
import Data.HashSet qualified as HashSet
import Data.List (isPrefixOf)
import Data.Set qualified as Set
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Duplo.Tree (fastMake)
import Language.LSP.Server qualified as S
import Language.LSP.Types qualified as J
import Language.LSP.VFS qualified as V
import StmContainers.Map qualified as StmMap
import System.FilePath (splitDirectories, takeDirectory, (</>))
import UnliftIO.Directory
  ( Permissions (writable), createDirectoryIfMissing, doesDirectoryExist, doesFileExist
  , getPermissions, setPermissions
  )
import UnliftIO.Exception (tryIO)
import UnliftIO.MVar (modifyMVar, modifyMVar_, newMVar, readMVar, swapMVar, tryReadMVar, withMVar)
import UnliftIO.STM (atomically)
import Witherable (iwither)

import AST
 ( ContractInfo, ContractInfo', pattern FindContract, FindFilepath (..), HasScopeForest
 , Includes (..), ParsedContract (..), ParsedContractInfo, addLigoErrsToMsg, addScopes
 , addShallowScopes, contractFile, lookupContract
 )
import AST.Includes (extractIncludedFiles, includesGraph', insertPreprocessorRanges)
import AST.Parser (loadPreprocessed, parse, parseContracts, parsePreprocessed)
import AST.Skeleton (Error (..), Lang (Caml), SomeLIGO (..))
import ASTMap qualified
import Cli (TempDir (..), TempSettings (..), getLigoClientEnv)
import Diagnostic (Message (..), MessageDetail (FromLanguageServer))
import Language.LSP.Util (sendWarning, reverseUriMap)
import Log qualified
import Parser (emptyParsedInfo)
import ParseTree (Source (..), pathToSrc)
import Progress (Progress (..), noProgress, (%))
import RIO.Indexing (getIndexDirectory, indexOptionsPath, tryGetIgnoredPaths)
import RIO.Types (Contract (..), IndexOptions (..), RIO, RioEnv (..))
import Util.Graph (forAMConcurrently, traverseAMConcurrently, wcc)

-- | Represents how much a 'fetch' or 'forceFetch' operation should spend trying
-- to load a contract.
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

fetch, forceFetch :: FetchEffort -> J.NormalizedUri -> RIO ContractInfo'
fetch effort = fmap cTree . fetch' effort
forceFetch effort = fmap cTree . forceFetch' effort

fetch', forceFetch' :: FetchEffort -> J.NormalizedUri -> RIO Contract
fetch' effort uri = Log.addContext (Log.sl "uri" $ J.fromNormalizedUri uri) do
  tmap <- asks reCache
  case effort of
    LeastEffort  -> ASTMap.fetchFast uri tmap
    NormalEffort -> ASTMap.fetchCurrent uri tmap
    BestEffort   -> ASTMap.fetchLatest uri tmap
forceFetch' = forceFetchAndNotify (const $ pure ())

forceFetchAndNotify :: (Contract -> RIO ()) -> FetchEffort -> J.NormalizedUri -> RIO Contract
forceFetchAndNotify notify effort uri = Log.addContext (Log.sl "uri" $ J.fromNormalizedUri uri) do
  tmap <- asks reCache
  ASTMap.invalidate uri tmap
  case effort of
    LeastEffort  -> ASTMap.fetchFastAndNotify notify uri tmap
    NormalEffort -> do
      v <- ASTMap.fetchCurrent uri tmap
      v <$ notify v
    BestEffort   -> do
      v <- ASTMap.fetchLatest uri tmap
      v <$ notify v

delete :: J.NormalizedUri -> RIO ()
delete uri = do
  imap <- asks reIncludes
  let fpM = J.uriToFilePath $ J.fromNormalizedUri uri
  case fpM of
    Nothing -> pure ()
    Just fp -> do
      let
        -- Dummy
        c = FindContract
          (Source fp "")
          (SomeLIGO Caml $ fastMake emptyParsedInfo (Error (FromLanguageServer "Impossible") []))
          []
      modifyMVar_ imap $ pure . Includes . G.removeVertex c . getIncludes

      buildGraphVar <- asks reBuildGraph
      modifyMVar_ buildGraphVar $ pure . Includes . G.removeVertex fp . getIncludes

  tmap <- asks reCache
  deleted <- ASTMap.delete uri tmap

  -- Invalidate contracts that are in the same group as the deleted one, as
  -- references might have changed.
  for_ deleted \(Contract _ deps) ->
    for_ deps
      (`ASTMap.invalidate` tmap)

invalidate :: J.NormalizedUri -> RIO ()
invalidate uri = ASTMap.invalidate uri =<< asks reCache

preload :: J.NormalizedFilePath -> RIO Source
preload normFp = Log.addNamespace "preload" do
  let
    uri = J.normalizedFilePathToUri normFp
    fin = J.fromNormalizedFilePath normFp
    mkReadOnly path = do
      p <- getPermissions path
      setPermissions path p { writable = False }
    createTemp new = do
      $(Log.warning) [Log.i|#{fin} not found. Creating temp file: #{new}|]
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
    Just vf -> pure $ Source fin' (V.virtualFileText vf)
    Nothing -> pathToSrc fin'

tempDirTemplate :: String
tempDirTemplate = ".ligo-work"

getTempPath :: FilePath -> RIO TempSettings
getTempPath fallbackPath = do
  optsM <- tryReadMVar =<< asks reIndexOpts
  let path = fromMaybe fallbackPath (indexOptionsPath =<< optsM)
  let tempDir = path </> tempDirTemplate
  createDirectoryIfMissing False tempDir
  pure $ TempSettings path $ UseDir tempDir

loadWithoutScopes :: J.NormalizedFilePath -> RIO ContractInfo
loadWithoutScopes normFp = Log.addNamespace "loadWithoutScopes" do
  src <- preload normFp
  ligoEnv <- getLigoClientEnv
  $(Log.debug) [Log.i|running with env #{ligoEnv}|]
  temp <- getTempPath $ takeDirectory $ J.fromNormalizedFilePath normFp
  parsePreprocessed temp src

-- | Like 'loadWithoutScopes', but if an 'IOException' has ocurred, then it will
-- return 'Nothing'.
tryLoadWithoutScopes :: J.NormalizedFilePath -> RIO (Maybe ParsedContractInfo)
tryLoadWithoutScopes =
  fmap (either (const Nothing) Just) . tryIO . (insertPreprocessorRanges <=< loadWithoutScopes)

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

-- | Loads the contracts of the directory, without parsing anything.
--
-- The pipeline will cause the preprocessor to run on each contract (if
-- directives are present), and each 'Source' will then be scanned for line
-- markers, which will be used to build the 'Includes' graph.
--
-- The downside is that momentarily, various files will be present in memory. In
-- the future, we can consider only keeping the line markers and building the
-- graph from this.
loadDirectory
  :: FilePath
  -> FilePath
  -> Includes ParsedContractInfo
  -> RIO (Includes Source, Map Source [Message])
loadDirectory root rootFileName includes = do
  temp <- getTempPath root
  let
    lookupOrLoad src = maybe
      (loadPreprocessed temp src)
      (pure . (_cFile &&& _cMsgs) . _getContract)
      (lookupContract (srcPath src) includes)

  shouldIndexFile <- getFilePredicate

  buildGraphM <- tryReadMVar =<< asks reBuildGraph
  S.withProgress "Indexing directory" S.NotCancellable \reportProgress -> if
    | Just (Includes buildGraph) <- buildGraphM
    , Just group <- find (G.hasVertex rootFileName) (wcc buildGraph) -> do
      let
        group' = G.induce shouldIndexFile group
        total = G.vertexCount group'
      progressVar <- newMVar 0

      msgsVar <- newMVar Map.empty
      loaded <- forAMConcurrently group' \fp -> do
        progress <- withMVar progressVar $ pure . succ
        reportProgress $ S.ProgressAmount (Just $ progress % total) (Just [Log.i|Parsing #{fp}|])
        (src, msg) <- lookupOrLoad =<< pathToSrc fp
        modifyMVar_ msgsVar $ pure . Map.insert src msg
        pure src
      (Includes loaded, ) <$> readMVar msgsVar
    | otherwise -> do
      loaded <- parseContracts
        lookupOrLoad
        (\Progress {..} -> reportProgress $ S.ProgressAmount (Just pTotal) (Just pMessage))
        shouldIndexFile
        root
      Includes graph <- includesGraph' (map fst loaded)
      let filtered = G.induce (shouldIndexFile . srcPath) graph
      pure (Includes filtered, Map.fromListWith (<>) loaded)

getInclusionsGraph
  :: FilePath  -- ^ Directory to look for contracts
  -> J.NormalizedFilePath  -- ^ Open contract to be loaded
  -> RIO (Includes ParsedContractInfo)
getInclusionsGraph root normFp = Log.addNamespace "getInclusionsGraph" do
  rootContract <- loadWithoutScopes normFp
  includesVar <- asks reIncludes
  shouldIndexFile <- getFilePredicate
  modifyMVar includesVar \includes'@(Includes includes) -> do
    let rootFileName = contractFile rootContract
    let groups = Includes <$> wcc includes
    join (,) <$> case find (isJust . lookupContract rootFileName) groups of
      -- Possibly the graph hasn't been initialized yet or a new file was created.
      Nothing -> do
        let fp = J.fromNormalizedFilePath normFp
        $(Log.debug) [Log.i|Can't find #{fp} in inclusions graph, loading #{root}...|]
        (Includes paths, msgs) <- loadDirectory root rootFileName includes'

        let buildGraph = Includes $ G.gmap srcPath paths
        buildGraphVar <- asks reBuildGraph
        void $ swapMVar buildGraphVar buildGraph

        temp <- getTempPath root
        connectedContractsE <-
          maybe
            -- User may have opened a file that's outside the indexing directory.
            -- Load it.
            (fmap Left . loadPreprocessed temp =<< pathToSrc fp)
            (pure . Right)
          $ find (Map.member (_cFile $ _getContract rootContract) . G.adjacencyMap)
          $ wcc paths
        case connectedContractsE of
          Left (src, msgs') -> do
            parsed <- parse src
            Includes . G.vertex <$> insertPreprocessorRanges (addLigoErrsToMsg msgs' parsed)
          Right connectedContracts -> do
            let
              parseCached src = do
                let srcMsgs = Map.lookup src msgs
                parsed <- parse src
                insertPreprocessorRanges $ addLigoErrsToMsg (join $ toList srcMsgs) parsed
            Includes <$> traverseAMConcurrently parseCached connectedContracts
      -- We've cached this contract, incrementally update the inclusions graph.
      Just (Includes oldIncludes) -> do
        (rootContract', toList -> includeEdges) <- extractIncludedFiles True rootContract
        let
          numNewContracts = length includeEdges
          lookupOrLoad fp = maybe
            (tryLoadWithoutScopes $ J.toNormalizedFilePath fp)
            (pure . Just)
            (lookupContract fp (Includes oldIncludes))

        newIncludes <- S.withProgress "Indexing new files" S.NotCancellable \reportProgress ->
          iwither
            (\n (_, fp) -> do
              reportProgress $ S.ProgressAmount (Just $ n % numNewContracts) (Just [Log.i|Loading #{fp}|])
              bool (pure Nothing) (lookupOrLoad fp) (shouldIndexFile fp))
            includeEdges
        let
          newSet = Set.fromList newIncludes
          oldSet = Map.keysSet $ G.adjacencyMap oldIncludes
          newVertices = Set.difference newSet oldSet
          removedVertices = Set.difference oldSet newSet
          groups' = filter (isNothing . lookupContract rootFileName) groups
          -- Replacing a contract with itself looks unintuitive but it works
          -- because ordering is decided purely on the file name.
          newGroup =
            G.overlay (G.fromAdjacencySets [(rootContract', newVertices)])
            $ G.replaceVertex rootContract' rootContract'
            $ foldr (G.removeEdge rootContract') oldIncludes removedVertices
        pure $ G.overlays (Includes newGroup : groups')

load
  :: forall parser
   . HasScopeForest parser RIO
  => J.NormalizedUri
  -> RIO Contract
load uri = Log.addNamespace "load" do
  let Just normFp = J.uriToNormalizedFilePath uri  -- FIXME: non-exhaustive
  rootIndex <- getIndexDirectory (takeDirectory $ J.fromNormalizedFilePath normFp)
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
      temp <- getTempPath $ takeDirectory $ J.fromNormalizedFilePath revNormFp
      Contract <$> loadDefault temp <*> pure [revUri]

  -- If we're trying to load an ignored file, then load it, but don't index
  -- anything else.
  shouldIgnoreFile <- maybe False (revFp `elem`) <$> tryGetIgnoredPaths
  if
    | shouldIgnoreFile -> do
      sendWarning [Log.i|Opening an ignored file. It will not be indexed and cross-file operations may not work as inteded. Change your .ligoproject file to index this file.|]
      loadWithoutIndexing
    | Just root <- rootM, dirExists -> do
      time <- ASTMap.getTimestamp

      revRoot <- if revUri == uri
        then pure root
        else V._vfsTempDir <$> S.getVirtualFiles

      rawGraph <- getInclusionsGraph revRoot revNormFp

      temp <- getTempPath root
      (Includes graph, result) <- S.withProgress "Scoping project" S.NotCancellable \reportProgress -> do
        let
          addScopesWithProgress = addScopes @parser temp
            (\Progress {..} -> reportProgress $ S.ProgressAmount (Just pTotal) (Just pMessage))
        case J.uriToFilePath $ J.fromNormalizedUri revUri of
          Nothing -> (,) <$> addScopesWithProgress rawGraph <*> loadDefault temp
          Just fp -> do
            scoped <- addScopesWithProgress rawGraph
            (scoped, ) <$> maybe (loadDefault temp) pure (lookupContract fp scoped)

      let contracts = (id &&& J.toNormalizedUri . J.filePathToUri . contractFile) <$> G.vertexList graph
      let nuris = snd <$> contracts
      tmap <- asks reCache
      for_ contracts \(contract, nuri) ->
        ASTMap.insert nuri (Contract contract nuris) time tmap

      pure $ Contract result nuris
    | otherwise -> do
      case rootIndex of
        IndexChoicePending -> $(Log.debug) [Log.i|Indexing directory has not been specified yet.|]
        _ -> pure ()
      loadWithoutIndexing

handleLigoFileChanged :: J.NormalizedFilePath -> J.FileChangeType -> RIO ()
handleLigoFileChanged nfp = \case
  J.FcCreated -> do
    $(Log.debug) [Log.i|Created #{fp}|]
    void $ forceFetch' BestEffort uri
  J.FcChanged -> do
    openDocsVar <- asks reOpenDocs
    mOpenDocs <- tryReadMVar openDocsVar
    case mOpenDocs of
      Just openDocs | not $ HashSet.member uri openDocs -> do
        $(Log.debug) [Log.i|Changed #{fp}|]
        void $ forceFetch' BestEffort uri
      _ -> pure ()
  J.FcDeleted -> do
    $(Log.debug) [Log.i|Deleted #{fp}|]
    delete uri
  where
    uri = J.normalizedFilePathToUri nfp
    fp = J.fromNormalizedFilePath nfp
