module AST.Parser
  ( Source (..)
  , Progress (..)
  , ParserCallback
  , parsePreprocessed
  , parseWithScopes
  , parseContracts
  , scanContracts
  , parseContractsWithDependencies
  , parseContractsWithDependenciesScopes
  , collectAllErrors
  ) where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Control.Exception.Safe (Handler (..), catches, throwM)
import Control.Lens ((%~))
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Bifunctor (second)
import Data.List (find)
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as Text (lines, unlines)
import Data.Traversable (for)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), takeDirectory)
import UnliftIO.Async (pooledMapConcurrently)

import Duplo.Lattice (Lattice (leq))

import AST.Includes (includesGraph)
import AST.Parser.Camligo qualified as Caml
import AST.Parser.Pascaligo qualified as Pascal
import AST.Parser.Reasonligo qualified as Reason
import AST.Scope
import AST.Skeleton
import Cli
  ( HasLigoClient, LigoDecodedExpectedClientFailureException (..)
  , LigoErrorNodeParseErrorException (..), fromLigoErrorToMsg, preprocess
  )
import Extension
import Log (i)
import ParseTree (Source (..), srcToText, toParseTree)
import Parser
import Progress (Progress (..), ProgressCallback, noProgress, (%))
import Util.Graph (wcc)

type ParserCallback m contract = Source -> m contract

parse :: MonadIO m => Source -> m ContractInfo
parse src = liftIO do
  (recogniser, dialect) <- onExt ElimExt
    { eePascal = (Pascal.recognise, Pascal)
    , eeCaml   = (Caml.recognise,   Caml)
    , eeReason = (Reason.recognise, Reason)
    } (srcPath src)
  uncurry (FindContract src) <$> (runParserM . recogniser =<< toParseTree dialect src)

parsePreprocessed :: forall m. HasLigoClient m => Source -> m ContractInfo
parsePreprocessed src = do
  src' <- liftIO $ deleteExtraMarkers <$> srcToText src
  (src'', err) <- (second (const Nothing) <$> preprocess src') `catches`
    [ Handler \(LigoDecodedExpectedClientFailureException err) ->
      pure (src', Just $ fromLigoErrorToMsg err)
    , Handler \(LigoErrorNodeParseErrorException _) ->
      pure (src', Nothing)
    , Handler \(_ :: IOError) ->
      pure (src', Nothing)
    ]
  maybe id addLigoErrToMsg err <$> parse src''
  where
    addLigoErrToMsg err = getContract . cMsgs %~ (`rewriteAt` err)

    -- | Rewrite error message at the most local scope or append it to the end.
    rewriteAt :: [Msg] -> Msg -> [Msg]
    rewriteAt at what@(from, _) = filter (not . (from `leq`) . fst) at <> [what]

    -- If the user has hand written any line markers, they will get removed here.
    deleteExtraMarkers =
      Text (srcPath src) . Text.unlines . map (\l -> maybe l mempty $ parseLineMarkerText l) . Text.lines

parseWithScopes
  :: forall impl m
   . (HasScopeForest impl m, MonadUnliftIO m)
  => Source
  -> m ContractInfo'
parseWithScopes src = do
  let fp = srcPath src
  graph <- parseContractsWithDependencies parsePreprocessed noProgress (takeDirectory fp)
  scoped <- addScopes @impl noProgress $ fromMaybe graph $ find (isJust . lookupContract fp) (wcc graph)
  maybe (throwM $ ContractNotFoundException fp scoped) pure (lookupContract fp scoped)

-- | Parse the whole directory for LIGO contracts and collect the results.
-- This ignores every other file which is not a contract.
parseContracts
  :: MonadUnliftIO m
  => ParserCallback m contract
  -> ProgressCallback m
  -> FilePath
  -> m [contract]
parseContracts parser reportProgress top = do
  input <- scanContracts top
  let
    numContracts = length input
    mkMsg contract = [i|Parsing #{contract}|]
  pooledMapConcurrently
    (\(n, c) -> do
      reportProgress (Progress (n % numContracts) (mkMsg c))
      parser (Path c))
    (zip [(0 :: Int) ..] input)

-- | Scan the whole directory for LIGO contracts.
-- This ignores every other file which is not a contract.
scanContracts
  :: MonadIO m
  => FilePath
  -> m [FilePath]
scanContracts top = do
  let exclude p = p /= "." && p /= ".."
  ds <- liftIO $ getDirectoryContents top
  contracts <- for (filter exclude ds) \d -> do
    let p = top </> d
    exists <- liftIO $ doesDirectoryExist p
    if exists
      then scanContracts p
      else if isJust (getExt p)
        then pure [p]
        else pure []
  pure $ concat contracts

parseContractsWithDependencies
  :: MonadUnliftIO m
  => ParserCallback m ContractInfo
  -> ProgressCallback m
  -> FilePath
  -> m (AdjacencyMap ParsedContractInfo)
parseContractsWithDependencies parser reportProgress =
  includesGraph <=< parseContracts parser reportProgress

parseContractsWithDependenciesScopes
  :: forall impl m
   . (HasScopeForest impl m, MonadUnliftIO m)
  => ParserCallback m ContractInfo
  -> ProgressCallback m
  -> FilePath
  -> m (AdjacencyMap ContractInfo')
parseContractsWithDependenciesScopes parser reportProgress =
  addScopes @impl reportProgress <=< parseContractsWithDependencies parser reportProgress

collectAllErrors :: ContractInfo' -> [Msg]
collectAllErrors (FindContract _ tree errs) =
   errs <> collectTreeErrors tree
