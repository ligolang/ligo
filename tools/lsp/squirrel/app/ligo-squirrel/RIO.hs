{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module RIO
  ( RIO
  , RioEnv (..)
  , run

  , getCustomConfig
  , fetchCustomConfig
  , updateCustomConfig

  , registerDidChangeConfiguration

  , source
  , clearDiagnostics

  , Contract (..)
  , delete
  , invalidate
  , preload
  , load
  , collectErrors
  , FetchEffort (..)
  , forceFetch
  , fetch
  , forceFetchAndNotify
  , forceFetch'
  , fetch'

  , registerFileWatcher
  ) where

-- TODO: break this module into file loading, diagnostics, lsp wrappers and
-- other parts when it grows too big.

import Prelude hiding (log)

import Algebra.Graph.AdjacencyMap qualified as G hiding (overlays)
import Algebra.Graph.Class qualified as G hiding (overlay)

import Control.Arrow
import Control.Lens ((??))
import Control.Monad
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, asks, mapReaderT, runReaderT)
import Control.Monad.Trans (lift)

import Data.Aeson qualified as Aeson (Result (..), Value, fromJSON)
import Data.Bool (bool)
import Data.Default (def)
import Data.Foldable (find, toList, traverse_)
import Data.Function (on)
import Data.HashSet (HashSet)
import Data.List (groupBy, nubBy, sortOn)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Set qualified as Set
import Data.String.Interpolate.IsString (i)

import Katip (Katip (..), KatipContext (..))

import Language.LSP.Diagnostics qualified as D
import Language.LSP.Server qualified as J
import Language.LSP.Types qualified as J
import Language.LSP.VFS qualified as V

import StmContainers.Map qualified as StmMap

import UnliftIO.Directory
  ( Permissions (writable), doesDirectoryExist, doesFileExist, getPermissions
  , setPermissions
  )
import UnliftIO.Exception (catchIO)
import UnliftIO.MVar (MVar, modifyMVar, modifyMVar_, tryPutMVar, tryReadMVar, tryTakeMVar)
import UnliftIO.STM (atomically)

import Witherable (iwither)

import Duplo.Tree (fastMake)

import AST hiding (cTree)
import ASTMap (ASTMap)
import ASTMap qualified
import Cli
import Config (Config (..), getConfigFromNotification)
import Duplo.Lattice (Lattice (leq))
import Extension (extGlobs)
import Language.LSP.Util (sendWarning, reverseUriMap)
import Log (LogT)
import Log qualified
import Parser
import Progress (Progress (..), (%))
import Range
import Util.Graph (wcc)

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

newtype RIO a = RIO
  { unRio :: ReaderT RioEnv (J.LspT Config (LogT IO)) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader RioEnv
    , MonadUnliftIO
    , J.MonadLsp Config.Config
    )

instance Katip RIO where
  getLogEnv = RIO $ lift $ lift getLogEnv
  localLogEnv f = RIO . mapReaderT (J.LspT . localLogEnv f . J.unLspT) . unRio

instance KatipContext RIO where
  getKatipContext = RIO $ lift $ lift getKatipContext
  localKatipContext f = RIO . mapReaderT (J.LspT . localKatipContext f . J.unLspT) . unRio
  getKatipNamespace = RIO $ lift $ lift getKatipNamespace
  localKatipNamespace f = RIO . mapReaderT (J.LspT . localKatipNamespace f . J.unLspT) . unRio

instance HasLigoClient RIO where
  getLigoClientEnv = fmap (LigoClientEnv . _cLigoBinaryPath) getCustomConfig

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

-- Fetch the configuration from the server and write it to the Config MVar
fetchCustomConfig :: RIO (Maybe Config)
fetchCustomConfig = Log.addNamespace "fetchCustomConfig" do
  void $
    J.sendRequest J.SWorkspaceConfiguration configRequestParams handleResponse
  tryReadMVar =<< asks reConfig
  where
    configRequestParams =
      J.ConfigurationParams (J.List [J.ConfigurationItem Nothing Nothing])

    handleResponse
        :: Either J.ResponseError (J.ResponseResult 'J.WorkspaceConfiguration)
        -> RIO ()
    handleResponse response = do
      config <- parseResponse response
      mConfig <- asks reConfig
      _oldConfig <- tryTakeMVar mConfig
      void $ tryPutMVar mConfig config

    parseResponse
      :: Either J.ResponseError (J.ResponseResult 'J.WorkspaceConfiguration)
      -> RIO Config
    parseResponse (Right (J.List [value])) = do
      case Aeson.fromJSON value of
        Aeson.Success c -> pure c
        Aeson.Error _err -> useDefault
    parseResponse _ = useDefault

    useDefault :: RIO Config
    useDefault = do
      $(Log.warning)
        "Couldn't parse config from server, using default"
      pure def

updateCustomConfig :: Aeson.Value -> RIO ()
updateCustomConfig config = Log.addNamespace "updateCustomConfig" do
  mConfig <- asks reConfig
  tryTakeMVar mConfig >>= \case
    Nothing -> decodeConfig def mConfig
    Just oldConfig -> decodeConfig oldConfig mConfig
  where
    decodeConfig old mConfig = case getConfigFromNotification old config of
      Left err -> do
        $(Log.err) [i|Failed to decode configuration: #{err}|]
        maybe (void $ tryPutMVar mConfig old) (const $ pure ()) =<< fetchCustomConfig
      Right newConfig -> do
        $(Log.debug) [i|Set new configuration: #{newConfig}|]
        void $ tryPutMVar mConfig newConfig

registerDidChangeConfiguration :: RIO ()
registerDidChangeConfiguration = do
  let
    reg = J.Registration "ligoChangeConfiguration" J.SWorkspaceDidChangeConfiguration J.Empty
    params = J.RegistrationParams $ J.List [J.SomeRegistration reg]

  void $ J.sendRequest J.SClientRegisterCapability params (const $ pure ())

registerFileWatcher :: RIO ()
registerFileWatcher = do
  let
    watcher extGlob = J.FileSystemWatcher
      { J._globPattern = extGlob
      , J._kind = Just J.WatchKind
        { J._watchChange = True
        , J._watchCreate = True
        , J._watchDelete = True
        }
      }
    regOpts = J.DidChangeWatchedFilesRegistrationOptions $ J.List $ map watcher extGlobs
    reg = J.Registration "ligoFileWatcher" J.SWorkspaceDidChangeWatchedFiles regOpts
    regParams = J.RegistrationParams $ J.List [J.SomeRegistration reg]

  void $ J.sendRequest J.SClientRegisterCapability regParams (const $ pure ())

source :: Maybe J.DiagnosticSource
source = Just "ligo-lsp"

run :: (J.LanguageContextEnv Config, RioEnv) -> RIO a -> LogT IO a
run (lcEnv, env) (RIO action) = J.runLspT lcEnv $ runReaderT action env

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

diagnostic :: J.TextDocumentVersion -> [(J.NormalizedUri, [J.Diagnostic])] -> RIO ()
diagnostic ver = Log.addNamespace "diagnostic" . traverse_ \(nuri, diags) -> do
  let diags' = D.partitionBySource diags
  maxDiagnostics <- _cMaxNumberOfProblems <$> getCustomConfig
  $(Log.debug) [i|Diags #{diags'}|]
  J.publishDiagnostics maxDiagnostics nuri ver diags'

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
  forM_ deleted \(Contract _ deps) ->
    forM_ deps
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
      $(Log.warning) [i|#{fin} not found. Creating temp file: #{new}|]
      -- We make the file read-only so the user is aware that it should not be
      -- changed. Note we can't make fin read-only, since it doesn't exist in
      -- the disk anymore, and also because LSP has no such request.
      sendWarning
        [i|File #{fin} was removed. The LIGO LSP server may not work as expected. Creating temporary file: #{new}|]
      new <$ mkReadOnly new
    handlePersistedFile = do
      tempMap <- asks reTempFiles
      let nFin = J.toNormalizedFilePath fin
      atomically (StmMap.lookup nFin tempMap) >>= \case
        Nothing -> do
          tempFile <- maybe (pure fin) createTemp =<< J.persistVirtualFile uri
          let nTempFile = J.toNormalizedFilePath tempFile
          tempFile <$ atomically (StmMap.insert nTempFile nFin tempMap)
        Just nTempFile -> pure $ J.fromNormalizedFilePath nTempFile

  fin' <- bool handlePersistedFile (pure fin) =<< doesFileExist fin

  mvf <- J.getVirtualFile uri
  return case mvf of
    Just vf -> Text fin' (V.virtualFileText vf)
    Nothing -> Path fin'

loadWithoutScopes
  :: J.NormalizedUri
  -> RIO ContractInfo
loadWithoutScopes uri = Log.addNamespace "loadWithoutScopes" do
  src <- preload uri
  ligoEnv <- getLigoClientEnv
  $(Log.debug) [i|running with env #{ligoEnv}|]
  parsePreprocessed src

-- | Like 'loadWithoutScopes', but if an 'IOException' has ocurred, then it will
-- return 'Nothing'.
tryLoadWithoutScopes :: J.NormalizedUri -> RIO (Maybe ParsedContractInfo)
tryLoadWithoutScopes uri =
  (Just <$> (insertPreprocessorRanges =<< loadWithoutScopes uri))
  `catchIO` const (pure Nothing)

normalizeFilePath :: FilePath -> J.NormalizedUri
normalizeFilePath = J.toNormalizedUri . J.filePathToUri

loadDirectory :: FilePath -> RIO (Includes ParsedContractInfo)
loadDirectory root = do
  J.withProgress "Indexing directory" J.NotCancellable \reportProgress ->
    parseContractsWithDependencies
      (loadWithoutScopes . normalizeFilePath . srcPath)
      (\Progress {..} -> reportProgress $ J.ProgressAmount (Just pTotal) (Just pMessage))
      root

getInclusionsGraph
  :: FilePath  -- ^ Directory to look for contracts
  -> J.NormalizedUri  -- ^ Open contract to be loaded
  -> RIO (Includes ParsedContractInfo)
getInclusionsGraph root uri = Log.addNamespace "getInclusionsGraph" do
  rootContract <- loadWithoutScopes uri
  includesVar <- asks reIncludes
  modifyMVar includesVar \includes -> do
    let rootFileName = contractFile rootContract
    let groups = Includes <$> wcc (getIncludes includes)
    join (,) <$> case find (isJust . lookupContract rootFileName) groups of
      -- Possibly the graph hasn't been initialized yet or a new file was created.
      Nothing -> do
        $(Log.debug) [i|Can't find #{uri} in inclusions graph, loading #{root}...|]
        loadDirectory root
      Just (Includes oldIncludes) -> do
        (rootContract', toList -> includeEdges) <- extractIncludedFiles True rootContract
        let
          numNewContracts = length includeEdges
          lookupOrLoad fp = maybe
            (tryLoadWithoutScopes $ normalizeFilePath fp)
            (pure . Just)
            (lookupContract fp (Includes oldIncludes))

        newIncludes <- J.withProgress "Indexing new files" J.NotCancellable \reportProgress ->
          iwither
            (\n (_, fp) -> do
              reportProgress $ J.ProgressAmount (Just $ n % numNewContracts) (Just [i|Loading #{fp}|])
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
  rootM <- J.getRootPath
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
        else V.vfsTempDir <$> J.getVirtualFiles

      rawGraph <- getInclusionsGraph revRoot revUri

      (Includes graph, result) <- J.withProgress "Scoping project" J.NotCancellable \reportProgress -> do
        let
          addScopesWithProgress = addScopes @parser
            (\Progress {..} -> reportProgress $ J.ProgressAmount (Just pTotal) (Just pMessage))
        case J.uriToFilePath $ J.fromNormalizedUri revUri of
          Nothing -> (,) <$> addScopesWithProgress rawGraph <*> loadDefault
          Just fp -> do
            let
              raw = fromMaybe rawGraph $ find
                (isJust . lookupContract fp)
                (Includes <$> wcc (getIncludes rawGraph))
            scoped <- addScopesWithProgress raw
            (scoped, ) <$> maybe loadDefault pure (lookupContract fp scoped)

      let contracts = (id &&& J.toNormalizedUri . J.filePathToUri . contractFile) <$> G.vertexList graph
      let nuris = snd <$> contracts
      tmap <- asks reCache
      forM_ contracts \(contract, nuri) ->
        ASTMap.insert nuri (Contract contract nuris) time tmap

      pure $ Contract result nuris
    _ -> do
      $(Log.warning) [i|Directory to load #{rootM} doesn't exist or was not set.|]
      Contract <$> loadDefault <*> pure [revUri]

collectErrors
  :: ContractInfo'
  -> J.TextDocumentVersion
  -> RIO ()
collectErrors contract version = do
  -- Correct the ranges of the error messages to correspond to real locations
  -- instead of locations after preprocessing.
  let errs' = nubBy (leq `on` fst) $ collectAllErrors contract
  let diags = errorToDiag <$> errs'
  let extractGroup :: [[(J.NormalizedUri, J.Diagnostic)]] -> [(J.NormalizedUri, [J.Diagnostic])]
      extractGroup [] = []
      extractGroup ([] : xs) = extractGroup xs
      extractGroup (ys@(y : _) : xs) = (fst y, snd <$> ys) : extractGroup xs
  let grouped = extractGroup $ groupBy ((==) `on` fst) $ sortOn fst diags
  diagnostic version grouped

clearDiagnostics :: Foldable f => f J.NormalizedUri -> RIO ()
clearDiagnostics uris = do
  maxDiagnostics <- _cMaxNumberOfProblems <$> getCustomConfig
  forM_ uris $ \nuri ->
    J.publishDiagnostics maxDiagnostics nuri Nothing (Map.singleton source mempty)

errorToDiag :: (Range, Error a) -> (J.NormalizedUri, J.Diagnostic)
errorToDiag (getRange -> r, Error what _) =
  ( J.toNormalizedUri $ J.filePathToUri $ _rFile r
  , J.Diagnostic
    (toLspRange r)
    (Just J.DsError)
    Nothing
    source
    what
    (Just $ J.List [])
    Nothing
  )
