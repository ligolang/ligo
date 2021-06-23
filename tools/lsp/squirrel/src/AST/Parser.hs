module AST.Parser
  ( Source (..)
  , parse
  , parseWithScopes
  , parseContracts
  , parseContractsWithDependencies
  , parseContractsWithDependenciesScopes
  ) where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as G
import Control.Arrow ((&&&))
import Control.Exception.Safe (throwM)
import Control.Lens ((%~))
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor.Const (Const (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Maybe (isJust)
import Data.Text (pack, replace, unpack)
import Data.Traversable (for)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), takeDirectory)

import Duplo.Pretty ((<.>), pp, ppToText)
import Duplo.Tree (layer, match)

import AST.Parser.Camligo qualified as Caml
import AST.Parser.Pascaligo qualified as Pascal
import AST.Parser.Reasonligo qualified as Reason
import AST.Scope hiding (find)
import AST.Skeleton

import Extension
import ParseTree (Source (..), toParseTree)
import Parser
import Product (Contains)
import Range (Range, getRange)
import Util (removeDots)

parse :: MonadIO m => Source -> m ContractInfo
parse src = liftIO do
  (recogniser, dialect) <- onExt ElimExt
    { eePascal = (Pascal.recognise, Pascal)
    , eeCaml   = (Caml.recognise,   Caml)
    , eeReason = (Reason.recognise, Reason)
    } (srcPath src)
  uncurry (FindContract src) <$> (runParserM . recogniser =<< toParseTree dialect src)

parseWithScopes
  :: forall impl m. HasScopeForest impl m => Source -> m ContractInfo'
parseWithScopes src = do
  let fp = srcPath src
  graph <- parseContractsWithDependenciesScopes @impl parse (takeDirectory fp)
  maybe (throwM $ ContractNotFoundException fp graph) pure (lookupContract fp graph)

-- | Parse the whole directory for LIGO contracts and collect the results.
-- This ignores every other file which is not a contract.
parseContracts
  :: MonadIO m
  => (Source -> m contract)
  -> FilePath
  -> m [contract]
parseContracts parser top = do
  let exclude p = p /= "." && p /= ".."
  ds <- liftIO $ getDirectoryContents top
  contracts <- for (filter exclude ds) \d -> do
    let p = top </> d
    exists <- liftIO $ doesDirectoryExist p
    if exists
      then parseContracts parser p
      else if isJust (getExt p)
        then pure <$> parser (Path p)
        else pure []
  pure $ concat contracts

-- TODO: Use FilePath
parseContractsWithDependencies
  :: MonadIO m
  => (Source -> m ContractInfo)
  -> FilePath
  -> m (AdjacencyMap ContractInfo)
parseContractsWithDependencies parser = fmap includesGraph . parseContracts parser

parseContractsWithDependenciesScopes
  :: forall impl m. HasScopeForest impl m
  => (Source -> m ContractInfo)
  -> FilePath
  -> m (AdjacencyMap ContractInfo')
parseContractsWithDependenciesScopes parser =
  addScopes @impl <=< parseContractsWithDependencies parser

-- *** Utilities

-- | Given some language, returns the filepath of its includes and the range
-- where the include was made.
extractIncludedFiles
  :: Contains Range info
  => FindFilepath (SomeLIGO info)
  -> Map FilePath (NonEmpty Range)
extractIncludedFiles (FindContract file tree _) = getConst . loopM_ (go Map.empty) $ getLIGO tree
  where
    pwd = takeDirectory $ srcPath file
    go includes = Const . \case
      (layer -> Just (BInclude (match -> Just (r, String filename)))) ->
        -- TODO: fix quotes appearing in includes
        Map.insertWith (<>) (removeDots $ pwd </> unpack (replace "\"" "" filename)) (pure $ getRange r) includes
      _ -> includes

-- | Given a list of contracts, builds a graph that represents how they are
-- included.
includesGraph :: [ContractInfo] -> AdjacencyMap ContractInfo
includesGraph vertices = graphWithCycleErrors
  where
    knownContracts = Map.fromList ((contractFile &&& (id &&& extractIncludedFiles)) <$> vertices)
    mkEdges l = Map.foldrWithKey
      (\fpR (NE.toList -> locsInL) -> case Map.lookup fpR knownContracts of
        -- The included contract doesn't exist. Append an error to the contract
        -- that included it and return a dummy one.
        Nothing ->
          let
            notFoundErr = Error ("File not found: " <> pack fpR) []
            mkMissing locInL = insertErr notFoundErr locInL l
          in G.overlay (G.vertices $ mkMissing <$> locsInL)
        Just (r, _) ->
          G.overlay (G.edge l r))
      G.empty
    graph = foldr (\(pc, edges) -> G.overlay (mkEdges pc edges)) (G.vertices vertices) knownContracts

    graphWithCycleErrors :: AdjacencyMap ContractInfo
    graphWithCycleErrors = foldr
      (\cycle' graph' ->
        let
          cyclicIncludeErr = Error (ppToText $ ppCycle cycle') []
          cycleSet = Set.fromList (map contractFile $ NE.toList cycle')
        in
        G.gmap
          (\vertex -> foldr
            (flip (foldr (insertErr cyclicIncludeErr)))
            vertex
            (Map.restrictKeys (maybe Map.empty snd (Map.lookup (contractFile vertex) knownContracts)) cycleSet)
          )
          graph')
      graph
      (findCycles graph)

    insertErr err loc = getContract . cMsgs %~ ((loc, err) :)

    ppCycle cycle'@(top :| _) =
      "Cyclic include detected:\n"
      <.> foldMap (\f -> ppFile f <.> "\n-> ") cycle'
      <.> ppFile top
      where
        ppFile = pp . pack . contractFile
