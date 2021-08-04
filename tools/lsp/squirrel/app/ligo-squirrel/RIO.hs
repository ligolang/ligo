{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans #-}

module RIO
  ( RIO
  , RioEnv
  , run

  , getCustomConfig
  , fetchCustomConfig

  , source
  , clearDiagnostics

  , Contract (..)
  , preload
  , load
  , collectErrors
  , forceFetch
  , fetch
  , forceFetch'
  , fetch'
  ) where

{- TODO: break this module into file loading, diagnostics, haskell-lsp wrappers
         and other parts when it grows too big.
-}

import Prelude hiding (log)

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as G

import Control.Arrow
import Control.Concurrent.MVar
import Control.Exception.Safe (MonadCatch, MonadThrow, catchIO)
import Control.Lens ((^.))
import Control.Monad
import Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, asks, runReaderT)

import Data.Aeson qualified as Aeson (Result (..), fromJSON)
import Data.Default (def)
import Data.Foldable (find, toList, traverse_)
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import Data.List (groupBy, nubBy, sortOn)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, isJust, isNothing)
import Data.Set qualified as Set
import Data.String.Interpolate (i)

import Language.LSP.Diagnostics qualified as D
import Language.LSP.Server qualified as J
import Language.LSP.Types qualified as J
import Language.LSP.VFS qualified as V

import Duplo.Tree (collect)

import AST hiding (cTree, find)
import ASTMap (ASTMap)
import ASTMap qualified
import Cli
import Config (Config (..))
import Duplo.Lattice (Lattice (leq))
import Log qualified
import Product
import Range
import Util.Graph (wcc)

data Contract = Contract
  { cTree :: ContractInfo'
  , cDeps :: [J.NormalizedUri]
  }

type RioEnv =
  Product
    '[ MVar Config
     , ASTMap J.NormalizedUri Contract RIO
     , MVar (HashMap J.NormalizedUri Bool)
     , "includes" := MVar (AdjacencyMap ParsedContractInfo)
     ]

newtype RIO a = RIO
  { _unRio :: ReaderT RioEnv (J.LspM Config) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader RioEnv
    , MonadThrow
    , MonadCatch
    , MonadUnliftIO
    , J.MonadLsp Config.Config
    )

-- FIXME: LspT does not provide these instances, even though it is just a ReaderT.
-- (Do not forget to remove -Wno-orphans.)
deriving newtype instance MonadThrow m => MonadThrow (J.LspT config m)
deriving newtype instance MonadCatch m => MonadCatch (J.LspT config m)

instance HasLigoClient RIO where
  getLigoClientEnv = fmap (LigoClientEnv . _cLigoBinaryPath) getCustomConfig

-- TODO: The lsp library provides no way to update the Config in the LspM monad
-- manually. So we have to maintain our own config to store the result of
-- `workspace/configuration` requests. We should get this fixed by the
-- maintainers, if possible.
getCustomConfig :: RIO Config
getCustomConfig = do
  mConfig <- asks getElem
  contents <- liftIO $ tryReadMVar mConfig
  case contents of
    -- TODO: The lsp library only sends the `workspace/configuration` request
    -- after initialization is complete, so during initialization, there is no
    -- way to know the client configuration. We need to get this fixed by the
    -- library maintainers, if possible.
    Nothing -> do
      Log.debug "getCustomConfig" "No config fetched yet, resorting to default"
      pure def
    Just config -> pure config

-- Fetch the configuration from the server and write it to the Config MVar
fetchCustomConfig :: RIO ()
fetchCustomConfig = do
  void $
    J.sendRequest J.SWorkspaceConfiguration configRequestParams handleResponse
  where
    configRequestParams =
      J.ConfigurationParams (J.List [J.ConfigurationItem Nothing Nothing])

    handleResponse
        :: Either J.ResponseError (J.ResponseResult 'J.WorkspaceConfiguration)
        -> RIO ()
    handleResponse response = do
      config <- parseResponse response
      mConfig <- asks getElem
      void $ liftIO $ tryPutMVar mConfig config

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
      Log.debug
        "fetchCustomConfig"
        "Couldn't parse config from server, using default"
      pure def

source :: Maybe J.DiagnosticSource
source = Just "ligo-lsp"

run :: (J.LanguageContextEnv Config.Config, RioEnv) -> RIO a -> IO a
run (lcEnv, env) (RIO action) = J.runLspT lcEnv $ runReaderT action env

fetch, forceFetch :: J.NormalizedUri -> RIO ContractInfo'
fetch = fmap cTree . fetch'
forceFetch = fmap cTree . forceFetch'

fetch', forceFetch' :: J.NormalizedUri -> RIO Contract
fetch' uri = asks getElem >>= ASTMap.fetchCurrent uri
forceFetch' uri = do
  tmap <- asks getElem
  ASTMap.invalidate uri tmap
  ASTMap.fetchCurrent uri tmap

diagnostic :: J.TextDocumentVersion -> [(J.NormalizedUri, [J.Diagnostic])] -> RIO ()
diagnostic ver = traverse_ \(nuri, diags) -> do
  let diags' = D.partitionBySource diags
  maxDiagnostics <- _cMaxNumberOfProblems <$> getCustomConfig
  Log.debug "LOOP.DIAG" [i|Diags #{diags'}|]
  J.publishDiagnostics maxDiagnostics nuri ver diags'

preload
  :: J.NormalizedUri
  -> RIO Source
preload uri = do
  let Just fin = J.uriToFilePath $ J.fromNormalizedUri uri  -- FIXME: non-exhaustive
  mvf <- J.getVirtualFile uri
  return case mvf of
    Just vf -> Text fin (V.virtualFileText vf)
    Nothing -> Path fin

loadWithoutScopes
  :: J.NormalizedUri
  -> RIO ContractInfo
loadWithoutScopes uri = do
  src <- preload uri
  ligoEnv <- getLigoClientEnv
  Log.debug "LOAD" [i|running with env #{ligoEnv}|]
  parsePreprocessed src

-- | Like 'loadWithoutScopes', but if an 'IOException' has ocurred, then it will
-- return 'Nothing'.
tryLoadWithoutScopes :: J.NormalizedUri -> RIO (Maybe ParsedContractInfo)
tryLoadWithoutScopes uri = (Just . insertPreprocessorRanges <$> loadWithoutScopes uri) `catchIO` const (pure Nothing)

scopes :: ParsedContractInfo -> RIO ContractInfo'
scopes = fmap (head . G.vertexList) . addScopes @Standard . G.vertex

load
  :: J.NormalizedUri
  -> RIO Contract
load uri = J.getRootPath >>= \case
  Nothing -> Contract <$> loadDefault <*> pure [uri]
  Just root -> do
    imap <- asks $ getTag @"includes"
    includes <- liftIO $ takeMVar imap
    tmap <- asks getElem

    rootContract <- loadWithoutScopes uri
    let rootFileName = contractFile rootContract
    let groups = wcc includes
    rawGraph <- case find (isJust . lookupContract rootFileName) groups of
      -- Possibly the graph hasn't be initialized yet or a new file was created.
      Nothing -> parseContractsWithDependencies (loadWithoutScopes . sourceToUri) root
      Just oldIncludes -> do
        let (rootContract', includeEdges) = extractIncludedFiles True rootContract
        let lookupOrLoad fp = maybe
              (tryLoadWithoutScopes (normalizeFilePath fp))
              (pure . Just)
              (lookupContract fp oldIncludes)
        newIncludes <- catMaybes <$> traverse (lookupOrLoad . snd) (toList includeEdges)
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
        pure $ G.overlays (newGroup : groups')

    fullGraph <- addScopes @Standard rawGraph
    (graph, result) <- case J.uriToFilePath $ J.fromNormalizedUri uri of
      Nothing -> (fullGraph, ) <$> loadDefault
      Just fp -> case find (isJust . lookupContract fp) (wcc rawGraph) of
        Nothing ->
          (fullGraph, ) <$> loadDefault
        Just graph' -> do
          scoped <- addScopes @Standard graph'
          (scoped, ) <$> maybe loadDefault pure (lookupContract fp scoped)

    let contracts = (id &&& normalizeFilePath . contractFile) <$> G.vertexList graph
    let nuris = snd <$> contracts
    forM_ contracts \(contract, nuri) ->
      ASTMap.insert nuri (Contract contract nuris) tmap

    liftIO $ putMVar imap rawGraph

    pure (Contract result nuris)
  where
    sourceToUri = normalizeFilePath . srcPath
    normalizeFilePath = J.toNormalizedUri . J.filePathToUri
    loadParsed = fmap insertPreprocessorRanges . loadWithoutScopes
    loadDefault = scopes =<< loadParsed uri

collectErrors
  :: (J.NormalizedUri -> RIO ContractInfo')
  -> J.NormalizedUri
  -> Maybe Int
  -> RIO ()
collectErrors fetcher uri version = fetcher uri >>= \(FindContract _ tree errs) -> do
  let errs' = nubBy (leq `on` fst) $ errs <> collectTreeErrors tree
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
    J.publishDiagnostics maxDiagnostics nuri Nothing mempty

errorToDiag :: (Range, Error a) -> (J.NormalizedUri, J.Diagnostic)
errorToDiag (getRange -> (Range (sl, sc, _) (el, ec, _) f), Error what _) =
  ( J.toNormalizedUri $ J.filePathToUri f
  , J.Diagnostic
    (J.Range begin end)
    (Just J.DsError)
    Nothing
    source
    what
    (Just $ J.List [])
    Nothing
  )
  where
    begin = J.Position (sl - 1) (sc - 1)
    end   = J.Position (el - 1) (ec - 1)

collectTreeErrors :: SomeLIGO Info' -> [Msg]
collectTreeErrors = map (getElem *** void) . collect . (^. nestedLIGO)
