{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module RIO.Document
  ( FetchEffort (..)

  , forceFetch
  , fetch
  , forceFetchAndNotify
  , forceFetch'
  , fetch'

  , delete
  , invalidate
  , preload
  , load
  ) where

import Algebra.Graph.AdjacencyMap qualified as G hiding (overlays)
import Algebra.Graph.Class qualified as G hiding (overlay)
import Control.Arrow ((&&&))
import Control.Lens ((??))
import Control.Monad (join, (<=<))
import Control.Monad.Reader (asks)
import Data.Bool (bool)
import Data.Foldable (find, for_, toList)
import Data.Set qualified as Set
import Data.Map qualified as Map
import Data.Maybe (isJust, isNothing)
import Duplo.Tree (fastMake)
import Language.LSP.Server qualified as S
import Language.LSP.Types qualified as J
import Language.LSP.VFS qualified as V
import StmContainers.Map qualified as StmMap
import UnliftIO.Directory
  ( Permissions (writable), doesDirectoryExist, doesFileExist, getPermissions
  , setPermissions
  )
import UnliftIO.Exception (catchIO, throwIO)
import UnliftIO.MVar (modifyMVar, modifyMVar_)
import UnliftIO.STM (atomically)
import Witherable (iwither)

import AST
 ( ContractInfo, ContractInfo', ContractNotFoundException (..), pattern FindContract
 , HasScopeForest, Includes (..), ParsedContractInfo, addScopes, addShallowScopes
 , contractFile, lookupContract
 )
import AST.Includes (extractIncludedFiles, insertPreprocessorRanges)
import AST.Parser (loadContractsWithDependencies, loadPreprocessed, parsePreprocessed)
import AST.Skeleton (Error (..), Lang (Caml), SomeLIGO (..))
import ASTMap qualified
import Cli (getLigoClientEnv)
import Language.LSP.Util (sendWarning, reverseUriMap)
import Log qualified
import Parser (emptyParsedInfo)
import ParseTree (Source (..))
import Progress (Progress (..), (%))
import RIO.Types (Contract (..), RIO, RioEnv (..))
import Util.Graph (traverseAMConcurrently, wcc)

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
          (Path fp)
          (SomeLIGO Caml $ fastMake emptyParsedInfo (Error "Impossible" []))
          []
      modifyMVar_ imap $ pure . Includes . G.removeVertex c . getIncludes

  tmap <- asks reCache
  deleted <- ASTMap.delete uri tmap

  -- Invalidate contracts that are in the same group as the deleted one, as
  -- references might have changed.
  for_ deleted \(Contract _ deps) ->
    for_ deps
      (`ASTMap.invalidate` tmap)

invalidate :: J.NormalizedUri -> RIO ()
invalidate uri = ASTMap.invalidate uri =<< asks reCache

preload
  :: J.NormalizedUri
  -> RIO Source
preload uri = Log.addNamespace "preload" do
  let Just fin = J.uriToFilePath $ J.fromNormalizedUri uri  -- FIXME: non-exhaustive
  let
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
          tempFile <- maybe (pure fin) createTemp =<< S.persistVirtualFile uri
          let nTempFile = J.toNormalizedFilePath tempFile
          tempFile <$ atomically (StmMap.insert nTempFile nFin tempMap)
        Just nTempFile -> pure $ J.fromNormalizedFilePath nTempFile

  fin' <- bool handlePersistedFile (pure fin) =<< doesFileExist fin

  mvf <- S.getVirtualFile uri
  return case mvf of
    Just vf -> Text fin' (V.virtualFileText vf)
    Nothing -> Path fin'

loadWithoutScopes :: J.NormalizedUri -> RIO ContractInfo
loadWithoutScopes uri = Log.addNamespace "loadWithoutScopes" do
  src <- preload uri
  ligoEnv <- getLigoClientEnv
  $(Log.debug) [Log.i|running with env #{ligoEnv}|]
  parsePreprocessed src

loadWithoutScopes' :: J.NormalizedUri -> RIO ParsedContractInfo
loadWithoutScopes' = insertPreprocessorRanges <=< loadWithoutScopes

-- | Like 'loadWithoutScopes', but if an 'IOException' has ocurred, then it will
-- return 'Nothing'.
tryLoadWithoutScopes :: J.NormalizedUri -> RIO (Maybe ParsedContractInfo)
tryLoadWithoutScopes uri = (Just <$> loadWithoutScopes' uri) `catchIO` const (pure Nothing)

normalizeFilePath :: FilePath -> J.NormalizedUri
normalizeFilePath = J.toNormalizedUri . J.filePathToUri

-- | Loads the contracts of the directory, without parsing anything.
--
-- The pipeline will cause the preprocessor to run on each contract (if
-- directives are present), and each 'Source' will then be scanned for line
-- markers, which will be used to build the 'Includes' graph.
--
-- The downside is that momentarily, various files will be present in memory. In
-- the future, we can consider only keeping the line markers and building the
-- graph from this.
loadDirectory :: FilePath -> RIO (Includes Source)
loadDirectory root =
  S.withProgress "Indexing directory" S.NotCancellable \reportProgress ->
    loadContractsWithDependencies
      (fmap fst . loadPreprocessed)
      (\Progress {..} -> reportProgress $ S.ProgressAmount (Just pTotal) (Just pMessage))
      root

getInclusionsGraph
  :: FilePath  -- ^ Directory to look for contracts
  -> J.NormalizedUri  -- ^ Open contract to be loaded
  -> RIO (Includes ParsedContractInfo)
getInclusionsGraph root uri = Log.addNamespace "getInclusionsGraph" do
  rootContract <- loadWithoutScopes uri
  includesVar <- asks reIncludes
  modifyMVar includesVar \(Includes includes) -> do
    let rootFileName = contractFile rootContract
    let groups = Includes <$> wcc includes
    join (,) <$> case find (isJust . lookupContract rootFileName) groups of
      -- Possibly the graph hasn't been initialized yet or a new file was created.
      Nothing -> do
        $(Log.debug) [Log.i|Can't find #{uri} in inclusions graph, loading #{root}...|]
        Includes paths <- loadDirectory root
        let exceptionGraph = Includes $ G.gmap srcPath paths
        connectedContracts <-
          maybe
            (throwIO $ ContractNotFoundException rootFileName exceptionGraph)
            pure
          $ find (Map.member rootFileName . G.adjacencyMap)
          $ wcc
          $ G.gmap srcPath paths
        Includes <$> traverseAMConcurrently (loadWithoutScopes' . normalizeFilePath) connectedContracts
      Just (Includes oldIncludes) -> do
        (rootContract', toList -> includeEdges) <- extractIncludedFiles True rootContract
        let
          numNewContracts = length includeEdges
          lookupOrLoad fp = maybe
            (tryLoadWithoutScopes $ normalizeFilePath fp)
            (pure . Just)
            (lookupContract fp (Includes oldIncludes))

        newIncludes <- S.withProgress "Indexing new files" S.NotCancellable \reportProgress ->
          iwither
            (\n (_, fp) -> do
              reportProgress $ S.ProgressAmount (Just $ n % numNewContracts) (Just [Log.i|Loading #{fp}|])
              lookupOrLoad fp)
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
  rootM <- S.getRootPath
  dirExists <- maybe (pure False) doesDirectoryExist rootM

  -- Here we try to handle the case when the current root path ceased to exist
  -- (e.g.: it was renamed) but the file is still open in the editor.
  -- Both persisted file and dir exist: Normal situation
  -- Persisted file exists, dir doesn't: Root directory renamed
  -- Persisted file doesn't exist, dir does: Root directory restored
  -- Neither exist: something went wrong
  revUri <- reverseUriMap ?? uri

  let
    loadDefault = addShallowScopes @parser (const $ pure ()) =<< loadWithoutScopes revUri

  case rootM of
    Just root | dirExists -> do
      time <- ASTMap.getTimestamp

      revRoot <- if revUri == uri
        then pure root
        else V.vfsTempDir <$> S.getVirtualFiles

      rawGraph <- getInclusionsGraph revRoot revUri

      (Includes graph, result) <- S.withProgress "Scoping project" S.NotCancellable \reportProgress -> do
        let
          addScopesWithProgress = addScopes @parser
            (\Progress {..} -> reportProgress $ S.ProgressAmount (Just pTotal) (Just pMessage))
        case J.uriToFilePath $ J.fromNormalizedUri revUri of
          Nothing -> (,) <$> addScopesWithProgress rawGraph <*> loadDefault
          Just fp -> do
            scoped <- addScopesWithProgress rawGraph
            (scoped, ) <$> maybe loadDefault pure (lookupContract fp scoped)

      let contracts = (id &&& J.toNormalizedUri . J.filePathToUri . contractFile) <$> G.vertexList graph
      let nuris = snd <$> contracts
      tmap <- asks reCache
      for_ contracts \(contract, nuri) ->
        ASTMap.insert nuri (Contract contract nuris) time tmap

      pure $ Contract result nuris
    _ -> do
      $(Log.warning) [Log.i|Directory to load #{rootM} doesn't exist or was not set.|]
      Contract <$> loadDefault <*> pure [revUri]
