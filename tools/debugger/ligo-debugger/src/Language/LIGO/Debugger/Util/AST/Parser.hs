module Language.LIGO.Debugger.Util.AST.Parser
  ( Source (..)
  , ParserCallback
  , parse
  , loadPreprocessed
  , parsePreprocessed
  -- , parseContracts
  , scanContracts
  ) where

import System.FilePath ((</>))
import Text.Regex.TDFA ((=~))
import UnliftIO.Directory (doesDirectoryExist, listDirectory)
import UnliftIO.Exception (Handler (..), catches, fromEither)

-- import AST.Includes (includesGraph)
import Language.LIGO.Debugger.Util.AST.Common (ContractInfo, addLigoErrsToMsg, pattern FindContract)
import Language.LIGO.Debugger.Util.AST.Parser.Camligo qualified as Caml
import Language.LIGO.Debugger.Util.AST.Parser.Jsligo qualified as Js
import Language.LIGO.Debugger.Util.AST.Skeleton
import Language.LIGO.Debugger.Util.Cli
  (HasLigoClient, LigoDecodedExpectedClientFailureException (..), LigoIOException,
  SomeLigoException (..), TempSettings (..), fromLigoErrorToMsg, preprocess)
import Language.LIGO.Debugger.Util.Diagnostic (Message)
import Language.LIGO.Debugger.Util.Extension
import Language.LIGO.Debugger.Util.Log (Log, i)
import Language.LIGO.Debugger.Util.Log qualified as Log
import Language.LIGO.Debugger.Util.ParseTree (Source (..), toParseTree)
import Language.LIGO.Debugger.Util.Parser (parseLineMarkerText, runParserM)

type ParserCallback m contract = Source -> m contract

parse :: Log m => ParserCallback m ContractInfo
parse src = do
  (recogniser, dialect) <- fromEither $ onExt ElimExt
    { eeCaml = (Caml.recognise,   Caml)
    , eeJs   = (Js.recognise,     Js)
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
        , Handler \(e :: LigoIOException) -> do
          $Log.err [i|#{displayException e}|]
          pure (src', [])
        , Handler \(_ :: SomeLigoException) ->
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
        prepreprocessed = (\l -> maybe (l, False) (const (mempty, True)) $ parseLineMarkerText l) <$> lines contents
        shouldPreprocess = hasPreprocessor || any snd prepreprocessed
      in
      (src{srcText = unlines $ map fst prepreprocessed}, shouldPreprocess)

parsePreprocessed :: (HasLigoClient m, Log m) => TempSettings -> Source -> m ContractInfo
parsePreprocessed tempSettings src = do
  (src', msgs) <- loadPreprocessed tempSettings src
  addLigoErrsToMsg msgs <$> parse src'

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
