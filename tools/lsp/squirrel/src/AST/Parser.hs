{-# LANGUAGE OverloadedLists #-}

module AST.Parser
  ( Source (..)
  , parse
  , parsePreprocessed
  , parseWithScopes
  , parseContracts
  , scanContracts
  , parseContractsWithDependencies
  , parseContractsWithDependenciesScopes

  , extractIncludedFiles
  , insertPreprocessorRanges
  , fromOriginalPoint
  ) where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as G
import Control.Arrow ((&&&))
import Control.Exception.Safe (Handler (..), catches, throwM)
import Control.Lens ((&), (.~), (%~), (+~), (-~), (^.), _1)
import Control.Monad ((<=<), join, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.RWS.Strict (RWS, gets, modify, runRWS, tell)
import Data.Bifunctor (bimap, second)
import Data.Bool (bool)
import Data.DList (DList, toList)
import Data.Foldable (for_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as Text (lines, pack, unlines)
import Data.Traversable (for)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), takeDirectory)
import UnliftIO.Async qualified as Async

import Duplo.Lattice (Lattice (leq))
import Duplo.Pretty (Pretty)
import Duplo.Tree (Cofree (..), inject)

import AST.Parser.Camligo qualified as Caml
import AST.Parser.Pascaligo qualified as Pascal
import AST.Parser.Reasonligo qualified as Reason
import AST.Scope hiding (find)
import AST.Skeleton
import Cli (HasLigoClient, LigoDecodedExpectedClientFailureException (..), fromLigoErrorToMsg, preprocess)

import Extension
import ParseTree (Source (..), srcToText, toParseTree)
import Parser
import Product (Product (..), getElem, modElem, putElem)
import Range (PreprocessedRange (..), Range, getRange, point, rangeLines, rFile, rFinish, rStart)
import Util (removeDots)
import Util.Graph (wcc)

parse :: MonadIO m => Source -> m ContractInfo
parse src = liftIO do
  (recogniser, dialect) <- onExt ElimExt
    { eePascal = (Pascal.recognise, Pascal)
    , eeCaml   = (Caml.recognise,   Caml)
    , eeReason = (Reason.recognise, Reason)
    } (srcPath src)
  uncurry (FindContract src) <$> (runParserM . recogniser =<< toParseTree dialect src)

parsePreprocessed :: HasLigoClient m => Source -> m ContractInfo
parsePreprocessed src = do
  src' <- liftIO $ deleteExtraMarkers <$> srcToText src
  (src'', err) <- (second (const Nothing) <$> preprocess src') `catches`
    [ Handler \(LigoDecodedExpectedClientFailureException err) ->
      pure (src', Just $ fromLigoErrorToMsg err)
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
  graph <- parseContractsWithDependencies parsePreprocessed (takeDirectory fp)
  scoped <- addScopes @impl $ fromMaybe graph $ find (isJust . lookupContract fp) (wcc graph)
  maybe (throwM $ ContractNotFoundException fp scoped) pure (lookupContract fp scoped)

-- | Parse the whole directory for LIGO contracts and collect the results.
-- This ignores every other file which is not a contract.
parseContracts
  :: MonadUnliftIO m
  => (Source -> m contract)
  -> FilePath
  -> m [contract]
parseContracts parser top = do
  input <- scanContracts top
  Async.mapConcurrently (parser . Path) input

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

-- TODO: Use FilePath
parseContractsWithDependencies
  :: MonadUnliftIO m
  => (Source -> m ContractInfo)
  -> FilePath
  -> m (AdjacencyMap ParsedContractInfo)
parseContractsWithDependencies parser = fmap includesGraph . parseContracts parser

parseContractsWithDependenciesScopes
  :: forall impl m
   . (HasScopeForest impl m, MonadUnliftIO m)
  => (Source -> m ContractInfo)
  -> FilePath
  -> m (AdjacencyMap ContractInfo')
parseContractsWithDependenciesScopes parser =
  addScopes @impl <=< parseContractsWithDependencies parser

-- *** Utilities

-- Traverse the tree top-down, applying some transformation after the recursion
-- has backtracked (bottom-up).
loopM'
  :: (Monad m, Traversable f)
  => (Cofree f a -> m (Cofree f a))
  -> (Cofree f a -> m (Cofree f a))
  -> (Cofree f a -> m (Cofree f a))
loopM' before' after' = aux
  where
    aux tree = do
      r :< fs <- before' tree
      fs' <- traverse aux fs
      after' (r :< fs')

type ParsingInfo = OriginalRange ': ParsedInfo

newtype OriginalRange
  = OriginalRange Range
  deriving newtype (Eq, Lattice, Ord, Pretty, Show)

insertOriginalRange :: Product ParsedInfo -> Product ParsingInfo
insertOriginalRange infos = OriginalRange (getRange infos) :> infos

deleteOriginalRange :: Product ParsingInfo -> Product ParsedInfo
deleteOriginalRange (_ :> infos) = infos

fromOriginalPoint :: Product Info -> Product ParsedInfo
fromOriginalPoint infos = PreprocessedRange (getRange infos) :> infos

data MarkerInfo = MarkerInfo
  { miMarkers   :: LineMarker
  , miLastRange :: Range
  , miDepth     :: Int
  } deriving stock (Show)

insertPreprocessorRanges :: ContractInfo -> ParsedContractInfo
insertPreprocessorRanges = fst . extractIncludedFiles False

-- | Given some contract A, returns a new version of that contract such that:
-- * When a line marker is found, all 'Range's will be adjusted in a manner such
--   that everything in the current document will stay as it is, but ranges from
--   included parts will be "squashed" to the 'Range' where the inclusion was
--   made. In other words, the ranges will be restored to the 'Range's that the
--   user sees.
-- * Additionally, 'PreprocessedRange's will be included, and they will have the
--   actual 'Range's of the files. That is, if A includes B then the ranges will
--   actually point to B. These are the actual ranges which are mapped according
--   to the line markers present in the document.
--
-- Also returns a difference list containing the edges of how to include files.
-- That is, if A includes B, then this list will contain a tuple (A, B).
extractIncludedFiles
  :: Bool  -- ^ Whether to only extract directly included files ('True') or all of them ('False').
  -> ContractInfo  -- ^ The contract to scan for includes.
  -> (ParsedContractInfo, DList (FilePath, FilePath))
extractIncludedFiles directIncludes (FindContract file (SomeLIGO dialect ligo) msgs) =
  let
    (info :< tree, markers, edges) =
      runRWS (loopM' before' after' $ insertOriginalRange . fromOriginalPoint <$> ligo) () mempty
    -- We still want RawContract to start at line 1:
    ligo' = bool (modElem (rStart . _1 +~ 1) info :< tree) (info :< tree) (IntMap.null markers)
  in
  (FindContract file (SomeLIGO dialect $ deleteOriginalRange <$> ligo') msgs, edges)
  where
    pwd :: FilePath
    pwd = takeDirectory $ srcPath file

    withPwd :: FilePath -> FilePath
    withPwd = removeDots . (pwd </>)

    implies :: Bool -> Bool -> Bool
    prerequisite `implies` conclusion = not prerequisite || conclusion
    infixr 1 `implies`

    before', after'
      :: LIGO ParsingInfo
      -> RWS () (DList (FilePath, FilePath)) (IntMap MarkerInfo) (LIGO ParsingInfo)
    before' (info :< tree) = do
      -- For the actual ranges, we pretend everything is fused into the line
      -- where the include would be located.
      -- For the preprocessed ranges, we use the line markers to map the range
      -- to the file and line they represent.
      for_ (getElem @[LineMarker] info) \lm@(LineMarker (withPwd -> n) f _ r) ->
        let line = r ^. rStart . _1 in
        case f of
          RootFile     -> modify $ IntMap.insert line $ MarkerInfo lm r 0
          IncludedFile -> gets (IntMap.lookupLT line) >>= \case
            Nothing -> pure ()
            Just (_, MarkerInfo prev lr d) -> do
              modify $ IntMap.insert
                line
                (MarkerInfo lm (bool lr (r & rStart . _1 -~ lmLoc prev ^. rStart . _1) $ d == 0) (d + 1))
              when (directIncludes `implies` d == 0) $
                tell [(withPwd $ lmFile prev, n)]
          ReturnToFile -> modify $ IntMap.lookupLT line >>= \case
            Nothing -> id
            Just (_, MarkerInfo _ lr d) -> IntMap.insert line $ MarkerInfo lm lr (d - 1)

      let range = getRange info
      let line = range ^. rStart . _1
      prev <- gets $ IntMap.lookupLE line
      case prev of
        Nothing -> pure $ info :< tree
        Just (_, MarkerInfo marker lastRange depth) -> do
          let newRange = range &
                if depth > 0 then
                  rStart .~ lastRange ^. rStart
                else
                  rStart . _1 -~ lmLoc marker ^. rStart . _1 - lmLine marker + 1
          let preRange = range
                & rangeLines -~ (lmLoc marker ^. rStart . _1 - lmLine marker + 1)
                & rFile .~ withPwd (lmFile marker)
          pure $ putElem (PreprocessedRange preRange) (putElem newRange info) :< tree
    after' (info :< tree) = do
      let OriginalRange range = getElem info
      let line = range ^. rFinish . _1
      prev <- gets $ IntMap.lookupLE line
      case prev of
        Nothing -> pure $ info :< tree
        Just (_, MarkerInfo marker lastRange depth) -> do
          -- In case we are at depth 0, we need to subtract the accumulated range at
          -- the finish as well, since we may see new includes while traversing the
          -- tree.
          let mkNewRange =
                if depth > 0 then
                  rFinish .~ lastRange ^. rStart
                else
                  rFinish . _1 -~ lmLoc marker ^. rStart . _1 - lmLine marker + 1
          pure $ modElem mkNewRange info :< tree

-- | Given a list of contracts, builds a graph that represents how they are
-- included.
includesGraph :: [ContractInfo] -> AdjacencyMap ParsedContractInfo
includesGraph contracts =
  uncurry G.overlay $ bimap (G.edges . toList) G.vertices $ foldr go ([], []) contracts
  where
    knownContracts :: Map FilePath (ParsedContractInfo, DList (FilePath, FilePath))
    knownContracts = Map.fromList ((contractFile &&& extractIncludedFiles False) <$> contracts)

    go
      :: ContractInfo
      -> (DList (ParsedContractInfo, ParsedContractInfo), [ParsedContractInfo])
      -> (DList (ParsedContractInfo, ParsedContractInfo), [ParsedContractInfo])
    go contract (edges, vertices) =
      let
        (vertex', edges') = findContract $ contractFile contract
        edges'' = join bimap (fst . findContract) <$> edges'
      in
      (edges'' <> edges, vertex' : vertices)

    findContract :: FilePath -> (ParsedContractInfo, DList (FilePath, FilePath))
    findContract contract =
      Map.findWithDefault (emptyContract contract, []) contract knownContracts

    emptyContract :: FilePath -> ParsedContractInfo
    emptyContract name =
      let
        p = point (-1) (-1)
        info = PreprocessedRange p :> [] :> [] :> p :> N :> CodeSource "" :> Nil
      in
      FindContract
        (Path name)
        (SomeLIGO Caml (info :< inject (Error ("Missing contract: " <> Text.pack name) [])))
        []
