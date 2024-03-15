module Bench.Diagnostics (waitDiagnostics, bench_simple_diagnostics, bench_diagnostics_edits) where

import Universum hiding ((^.))

import Criterion
import Data.Default
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Test qualified as LSP

import Bench.Util

data ExpectedDiagnosticsNumber = ExpectedDiagnosticsNumber
  { expectedErrors :: Int
  , expectedWarnings :: Int
  , expectedInfos :: Int
  , expectedHints :: Int
  } deriving (Eq, Show)

instance Default ExpectedDiagnosticsNumber
  where
  def = ExpectedDiagnosticsNumber 0 0 0 0

data DiagnosticsRequest = DiagnosticsRequest
  { drProject :: FilePath
    -- ^ Absolute file path to ligo project
  , drFile :: Doc
    -- ^ We'll open a doc so LSP server will process us and give us diagnostics
  , drExpected :: Maybe ExpectedDiagnosticsNumber
    -- ^ How many errors/warninbgs/etc we expect? @Nothing@ if we don't want to make assertions
  } deriving Show

waitDiagnostics :: DiagnosticsPullingMethod -> DiagnosticsRequest -> LSP.Session [LSP.Diagnostic]
waitDiagnostics pullingMethod dr@DiagnosticsRequest {..} = do
  tdi <- getDoc drFile -- We just want to trigger TextDocumentDidOpen in case the doc was not opened before
  diagnostics <- pullDiagnostics tdi pullingMethod
  whenJust drExpected $ \expectedNumbers ->
    let diagnosticTypes :: [(String, ExpectedDiagnosticsNumber -> Int, LSP.DiagnosticSeverity)]
        diagnosticTypes =
          [ ("error", expectedErrors, LSP.DiagnosticSeverity_Error),
            ("warning", expectedWarnings, LSP.DiagnosticSeverity_Warning),
            ("info", expectedInfos, LSP.DiagnosticSeverity_Information),
            ("hint", expectedHints, LSP.DiagnosticSeverity_Hint)
          ]
     in for_ diagnosticTypes $ \(name, f, s) ->
          let actual = length $ filter (\x -> x._severity == Just s) diagnostics
              expected = f expectedNumbers
           in unless (actual == expected) $
                fail $
                  "Request "
                    <> show dr
                    <> " returned unexpected amount of "
                    <> name
                    <> "s: "
                    <> show actual
  return diagnostics

simpleDiagnosticsBench :: DiagnosticsRequest -> Benchmark
simpleDiagnosticsBench dr@DiagnosticsRequest {..} =
  -- Pulling method in these benchmarks has non effect
  -- since we don't do any modifications in the target file
  benchLspSession
    OnDocumentLink
    (show drFile)
    drProject
    do waitDiagnostics OnDocumentLink dr

bench_simple_diagnostics :: [Benchmark]
bench_simple_diagnostics =
  [ bgroup "Diagnostics/simple"
    [ simpleDiagnosticsBench dr
    | dr <-
      [ DiagnosticsRequest
        { drProject = projectWithOneSmallFile
        , drFile = FileDoc "one_small_file.mligo"
        , drExpected = Just $ def {expectedErrors = 1} -- i_am_type_error
        }
      , DiagnosticsRequest
        { drProject = projectWithOneBigFile
        , drFile = FileDoc "one_big_file.mligo"
        , drExpected = Just def
        }
      , DiagnosticsRequest
        { drProject = projectChecker
        , drFile = FileDoc "sliceList.mligo"
        , drExpected = Just def
        }
      ]
    ]
  ]

-- Sorry for so long lines
benchDiagnosticsOneSmallFileEditsOnDoc, benchDiagnosticsOneSmallFileKeystrokesOnDoc, benchDiagnosticsOneSmallFileEditsOnDocumentLink, benchDiagnosticsOneSmallFileKeystrokesOnDocumentLink :: Benchmark
(benchDiagnosticsOneSmallFileEditsOnDoc, benchDiagnosticsOneSmallFileKeystrokesOnDoc, benchDiagnosticsOneSmallFileEditsOnDocumentLink, benchDiagnosticsOneSmallFileKeystrokesOnDocumentLink) =
  withBothEditsAndKeystrokes benchDiagnosticsOneSmallFile
  where
  benchDiagnosticsOneSmallFile pullingMethod insertFuncName insert =
    benchLspSession pullingMethod ("one_small_file.mligo/" <> insertFuncName <> "/" <> show pullingMethod) projectWithOneSmallFile $ do
    doc <- OpenedDoc <$> openLigoDoc "one_small_file.mligo"
    let drFile = doc
        drProject = projectWithOneSmallFile
    diags1 <- waitDiagnostics pullingMethod DiagnosticsRequest {drFile, drProject, drExpected = Just $ def {expectedErrors = 1}} -- i_am_type_error
    insert doc (8,1) "let the_type_error = (2 : string)"
    diags2 <- waitDiagnostics pullingMethod DiagnosticsRequest {drFile, drProject, drExpected = Just $ def {expectedErrors = 2}}
    insert doc (4,1) "let another_type_error = (3 : string)"
    diags3 <- waitDiagnostics pullingMethod DiagnosticsRequest {drFile, drProject, drExpected = Just $ def {expectedErrors = 3}}
    pure (diags1, diags2, diags3)

benchDiagnosticsOneBigFileEditsOnDoc, benchDiagnosticsOneBigFileKeystrokesOnDoc, benchDiagnosticsOneBigFileEditsOnDocumentLink, benchDiagnosticsOneBigFileKeystrokesOnDocumentLink :: Benchmark
(benchDiagnosticsOneBigFileEditsOnDoc, benchDiagnosticsOneBigFileKeystrokesOnDoc, benchDiagnosticsOneBigFileEditsOnDocumentLink, benchDiagnosticsOneBigFileKeystrokesOnDocumentLink) =
  withBothEditsAndKeystrokes benchDiagnosticsOneSmallFile
  where
  benchDiagnosticsOneSmallFile pullingMethod insertFuncName insert =
    benchLspSession pullingMethod ("one_big_file.mligo/" <> insertFuncName <> "/" <> show pullingMethod) projectWithOneBigFile $ do
    doc <- OpenedDoc <$> openLigoDoc "one_big_file.mligo"
    let drFile = doc
        drProject = projectWithOneBigFile
    diags1 <- waitDiagnostics pullingMethod DiagnosticsRequest {drFile, drProject, drExpected = Just $ def {expectedErrors = 0}}
    insert doc (8,1) "let the_type_error = (2 : string)"
    diags2 <- waitDiagnostics pullingMethod DiagnosticsRequest {drFile, drProject, drExpected = Just $ def {expectedErrors = 1}}
    insert doc (4,1) "let another_type_error = (3 : string)"
    diags3 <- waitDiagnostics pullingMethod DiagnosticsRequest {drFile, drProject, drExpected = Just $ def {expectedErrors = 2}}
    pure (diags1, diags2, diags3)

benchDiagnosticsCheckerEditsOnDocumentLink, benchDiagnosticsCheckerKeystrokesOnDocumentLink :: Benchmark
(benchDiagnosticsCheckerEditsOnDocumentLink, benchDiagnosticsCheckerKeystrokesOnDocumentLink) =
  let (_, _, edits, keystrokes) = withBothEditsAndKeystrokes benchDiagnosticsChecker
  in (edits, keystrokes)
  where
    benchDiagnosticsChecker _ insertFuncName insert =
      benchLspSession OnDocumentLink ("avl.mligo/" <> insertFuncName) projectChecker do
        let pullingMethod = OnDocumentLink
        doc <- OpenedDoc <$> openLigoDoc "avl.mligo"
        let drFile = doc
            drProject = projectChecker
        diags1 <- waitDiagnostics pullingMethod DiagnosticsRequest {drFile, drProject, drExpected = Just $ def {expectedErrors = 0}}
        insert doc (703,1) "let the_type_error = (2 : string)"
        diags2 <- waitDiagnostics pullingMethod DiagnosticsRequest {drFile, drProject, drExpected = Just $ def {expectedErrors = 1}}
        insert doc (342,1) "let another_type_error = (3 : string)"
        diags3 <- waitDiagnostics pullingMethod DiagnosticsRequest {drFile, drProject, drExpected = Just $ def {expectedErrors = 2}}
        pure (diags1, diags2, diags3)


bench_diagnostics_edits :: [Benchmark]
bench_diagnostics_edits =
  [ bgroup
      "Diagnostics/edits"
      [ benchDiagnosticsOneSmallFileEditsOnDoc
      , benchDiagnosticsOneSmallFileKeystrokesOnDoc
      , benchDiagnosticsOneBigFileEditsOnDoc
      , benchDiagnosticsOneBigFileKeystrokesOnDoc
      , benchDiagnosticsOneSmallFileEditsOnDocumentLink
      , benchDiagnosticsOneSmallFileKeystrokesOnDocumentLink
      , benchDiagnosticsOneBigFileEditsOnDocumentLink
      , benchDiagnosticsOneBigFileKeystrokesOnDocumentLink
      , benchDiagnosticsCheckerEditsOnDocumentLink
      , benchDiagnosticsCheckerKeystrokesOnDocumentLink
      ]
  ]
