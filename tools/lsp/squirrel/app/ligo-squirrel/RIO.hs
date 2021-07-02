{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans #-}

module RIO
  ( RIO
  , RioEnv
  , run

  , source
  , maxDiagnostics
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

import Algebra.Graph.AdjacencyMap qualified as G (vertex, vertexList)
import Control.Arrow
import Control.Concurrent.MVar (MVar)
import Control.Exception.Safe (MonadCatch, MonadThrow)
import Control.Lens ((^.))
import Control.Monad
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, asks, runReaderT)

import Data.Foldable (find, traverse_)
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import Data.List (groupBy, nubBy, sortOn)
import Data.Maybe (isJust)
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
    '[ ASTMap J.NormalizedUri Contract RIO
     , MVar (HashMap J.NormalizedUri Bool)
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
  getLigoClientEnv = fmap (LigoClientEnv . _cLigoBinaryPath) J.getConfig

source :: Maybe J.DiagnosticSource
source = Just "ligo-lsp"

maxDiagnostics :: Int
maxDiagnostics = 100

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
  Log.debug "LOOP.DIAG" [i|Diags #{diags'}|]
  J.publishDiagnostics maxDiagnostics nuri ver diags'

preload
  :: J.Uri
  -> RIO Source
preload uri = do
  let Just fin = J.uriToFilePath uri  -- FIXME: non-exhaustive
  mvf <- J.getVirtualFile (J.toNormalizedUri uri)
  return case mvf of
    Just vf -> Text fin (V.virtualFileText vf)
    Nothing -> Path fin

loadWithoutScopes
  :: J.Uri
  -> RIO ContractInfo
loadWithoutScopes uri = do
  src <- preload uri
  ligoEnv <- getLigoClientEnv
  Log.debug "LOAD" [i|running with env #{ligoEnv}|]
  parse src

scopes :: ContractInfo -> RIO ContractInfo'
scopes = fmap (head . G.vertexList) . addScopes @Standard . G.vertex

load
  :: J.Uri
  -> RIO Contract
load uri = J.getRootPath >>= \case
  Nothing -> Contract <$> loadDefault <*> pure [J.toNormalizedUri uri]
  Just root -> do
    rawGraph <- parseContractsWithDependencies (loadWithoutScopes . J.filePathToUri . srcPath) root
    fullGraph <- addScopes @Standard rawGraph
    tmap <- asks getElem
    (graph, result) <- case J.uriToFilePath uri of
      Nothing -> (fullGraph, ) <$> loadDefault
      Just fp -> case find (isJust . lookupContract fp) (wcc rawGraph) of
        Nothing ->
          (fullGraph, ) <$> loadDefault
        Just graph' -> do
          scoped <- addScopes @Standard graph'
          (scoped, ) <$> maybe loadDefault pure (lookupContract fp scoped)

    let contracts = (id &&& J.toNormalizedUri . J.filePathToUri . contractFile) <$> G.vertexList graph
    let nuris = snd <$> contracts
    forM_ contracts \(contract, nuri) ->
      ASTMap.insert nuri (Contract contract nuris) tmap

    pure (Contract result nuris)
  where
    loadDefault = scopes =<< loadWithoutScopes uri

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
clearDiagnostics = mapM_ \nuri -> J.publishDiagnostics maxDiagnostics nuri Nothing mempty

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
