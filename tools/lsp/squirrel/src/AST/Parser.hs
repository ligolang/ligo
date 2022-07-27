module AST.Parser
  ( Source (..)
  , ParserCallback
  , parse
  , loadPreprocessed
  , parsePreprocessed
  , parseWithScopes
  , parseContracts
  , scanContracts
  , collectAllErrors
  ) where

import Control.Monad.IO.Unlift (MonadIO (liftIO), MonadUnliftIO)
import Data.Foldable (toList)
import Data.List (find, isPrefixOf)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as Text (lines, unlines)
import System.FilePath (splitDirectories, takeDirectory, takeFileName, (</>))
import Text.Regex.TDFA ((=~))
import UnliftIO.Async (pooledMapConcurrently)
import UnliftIO.Directory (doesDirectoryExist, listDirectory)
import UnliftIO.Exception (Handler (..), catches, displayException, fromEither)

import AST.Includes (includesGraph)
import AST.Parser.Camligo qualified as Caml
import AST.Parser.Pascaligo qualified as Pascal
import AST.Parser.Reasonligo qualified as Reason
import AST.Parser.Jsligo qualified as Js
import AST.Skeleton
import AST.Scope
  ( ContractInfo, ContractInfo', pattern FindContract, HasScopeForest, Includes (..)
  , addLigoErrsToMsg, addScopes, contractNotFoundException, lookupContract
  )
import Cli
  ( HasLigoClient, LigoDecodedExpectedClientFailureException (..)
  , SomeLigoException (..), TempDir (..), TempSettings (..), fromLigoErrorToMsg, preprocess
  )
import Diagnostic (Message)
import Extension
import Log (Log, NoLoggingT (..), i)
import Log qualified
import ParseTree (Source (..), pathToSrc, toParseTree)
import Parser (collectTreeErrors, parseLineMarkerText, runParserM)
import Progress (Progress (..), ProgressCallback, noProgress, (%))
import Util.Graph (wcc)

type ParserCallback m contract = Source -> m contract

parse :: Log m => ParserCallback m ContractInfo
parse src = do
  (recogniser, dialect) <- fromEither $ onExt ElimExt
    { eePascal = (Pascal.recognise, Pascal)
    , eeCaml   = (Caml.recognise,   Caml)
    , eeReason = (Reason.recognise, Reason)
    , eeJs     = (Js.recognise,     Js)
    } (srcPath src)
  tree <- toParseTree dialect src
  uncurry (FindContract src) <$> runParserM (recogniser tree)


loadPreprocessed
  :: (HasLigoClient m, Log m)
  => TempSettings
  -> Source
  -> m (Source, [Message])
loadPreprocessed tempSettings src = do
  let (src', needsPreprocessing) = prePreprocess $ srcText src
  if needsPreprocessing
    then
      ((, []) <$> preprocess tempSettings src') `catches`
        [ Handler \(LigoDecodedExpectedClientFailureException errs warns _) ->
          pure (src', fromLigoErrorToMsg <$> toList errs <> warns)
        , Handler \(_ :: SomeLigoException) ->
          pure (src', [])
        , Handler \(e :: IOError) -> do
          -- Likely LIGO isn't installed or was not found.
          $(Log.err) [i|Couldn't call LIGO, failed with #{displayException e}|]
          pure (src', [])
        ]
    else
      pure (src', [])
  where
    -- If the user has hand written any line markers, they will get removed here.
    -- Also query whether we need to do any preprocessing at all in the first place.
    prePreprocess :: Text -> (Source, Bool)
    prePreprocess contents =
      let
        hasPreprocessor = contents =~ ("^#[ \t]*[a-z]+" :: Text)
        prepreprocessed = (\l -> maybe (l, False) (const (mempty, True)) $ parseLineMarkerText l) <$> Text.lines contents
        shouldPreprocess = hasPreprocessor || any snd prepreprocessed
      in
      (Source (srcPath src) $ Text.unlines $ map fst prepreprocessed, shouldPreprocess)

parsePreprocessed :: (HasLigoClient m, Log m) => TempSettings -> Source -> m ContractInfo
parsePreprocessed tempSettings src = do
  (src', msgs) <- loadPreprocessed tempSettings src
  addLigoErrsToMsg msgs <$> parse src'

-- | Parse the given source and all other contracts in the same directory,
-- adding scopes to it. This function is intended to be used by tests and for
-- interactive debugging in GHCi.
parseWithScopes
  :: forall impl m
   . (HasScopeForest impl (NoLoggingT m), MonadIO m)
  => FilePath
  -> m ContractInfo'
parseWithScopes fp = runNoLoggingT do
  let
    top = takeDirectory fp
    template = ".ligo-test-"
    temp = TempSettings top $ GenerateDir $ template <> takeFileName fp
    ignore = not . any (template `isPrefixOf`) . splitDirectories
  graph <- includesGraph =<< parseContracts (parsePreprocessed temp) noProgress ignore top
  let group = find (isJust . lookupContract fp) $ Includes <$> wcc (getIncludes graph)
  scoped <- addScopes @impl temp noProgress $ fromMaybe graph group
  maybe (contractNotFoundException fp scoped) pure (lookupContract fp scoped)

-- | Parse the whole directory for LIGO contracts and collect the results.
-- This ignores every other file which is not a contract.
parseContracts
  :: (Log m, MonadUnliftIO m)
  => ParserCallback m contract
  -> ProgressCallback m
  -> (FilePath -> Bool)
  -> FilePath
  -> m [contract]
parseContracts parser reportProgress predicate top = do
  input <- scanContracts predicate top
  let
    numContracts = length input
    mkMsg contract = [i|Parsing #{contract}|]
  pooledMapConcurrently
    (\(n, c) -> do
      reportProgress (Progress (n % numContracts) (mkMsg c))
      src <- pathToSrc c
      parser src)
    (zip [(0 :: Int) ..] input)

-- | Scan the whole directory for LIGO contracts.
-- This ignores every other file which is not a contract.
scanContracts :: MonadIO m => (FilePath -> Bool) -> FilePath -> m [FilePath]
scanContracts predicate = liftIO . scanContractsImpl [] predicate

scanContractsImpl :: [FilePath] -> (FilePath -> Bool) -> FilePath -> IO [FilePath]
scanContractsImpl seen predicate top
  | predicate top = do
    ds <- listDirectory top
    flip foldMap ds \d -> do
      let p = top </> d
      if predicate p
        then do
          exists <- doesDirectoryExist p
          if
            | exists -> scanContractsImpl seen predicate p
            | isLigoFile p -> pure $ p : seen
            | otherwise -> pure seen
        else pure seen
  | otherwise = pure seen

collectAllErrors :: ContractInfo' -> [Message]
collectAllErrors (FindContract _ tree errs) =
  errs <> collectTreeErrors tree
