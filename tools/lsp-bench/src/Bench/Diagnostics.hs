module Bench.Diagnostics (waitDiagnostics, bench_simple_diagnostics, bench_diagnostics_edits) where

import Universum hiding ((^.))

import Criterion
import Data.Default
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Test qualified as LSP

import Bench.Util
import Data.Aeson qualified as Aeson
import Language.LSP.Protocol.Lens qualified as LspLens
import Language.LSP.Protocol.Message qualified as LSP
import Lens.Micro ((^.))

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

waitDiagnostics :: DiagnosticsRequest -> LSP.Session [LSP.Diagnostic]
waitDiagnostics dr@DiagnosticsRequest {..} = do
  _ <- getDoc drFile -- We just want to trigger TextDocumentDidOpen in case the doc was not opened before
  _ <- let params = Aeson.Null in LSP.sendRequest (LSP.SMethod_CustomMethod $ Proxy @"DebugEcho") params
    -- After we edited a document multiple times, the LSP server might produce multiple publish diagnostics
    -- notifications
  firstMessage <- LSP.waitForDiagnostics
  otherDiagnosticMessages <- many LSP.publishDiagnosticsNotification
  echoResponse <- LSP.response (LSP.SMethod_CustomMethod $ Proxy @"DebugEcho")
  unless (echoResponse ^. LspLens.result == Right (Aeson.String "DebugEchoResponse"))
    $ fail "Expected DebugEchoResponse"
  let diagnostics = case nonEmpty otherDiagnosticMessages of
        Nothing -> firstMessage
        Just messages -> last messages ^. (LspLens.params . LspLens.diagnostics)
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
  benchLspSession (show drFile) drProject $ waitDiagnostics dr

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
      ]
    ]
  ]

benchDiagnosticsOneSmallFileEdits, benchDiagnosticsOneSmallFileKeystrokes :: Benchmark
(benchDiagnosticsOneSmallFileEdits, benchDiagnosticsOneSmallFileKeystrokes) =
  withBothEditsAndKeystrokes benchDiagnosticsOneSmallFile
  where
  benchDiagnosticsOneSmallFile insertFuncName insert =
    benchLspSession ("one_small_file.mligo/" <> insertFuncName) projectWithOneSmallFile $ do
    doc <- OpenedDoc <$> openLigoDoc "one_small_file.mligo"
    let drFile = doc
        drProject = projectWithOneSmallFile
    diags1 <- waitDiagnostics DiagnosticsRequest {drFile, drProject, drExpected = Just $ def {expectedErrors = 1}} -- i_am_type_error
    insert doc (8,1) "let the_type_error = (2 : string)"
    diags2 <- waitDiagnostics DiagnosticsRequest {drFile, drProject, drExpected = Just $ def {expectedErrors = 2}}
    insert doc (4,1) "let another_type_error = (3 : string)"
    diags3 <- waitDiagnostics DiagnosticsRequest {drFile, drProject, drExpected = Just $ def {expectedErrors = 3}}
    pure (diags1, diags2, diags3)

benchDiagnosticsOneBigFileEdits, benchDiagnosticsOneBigFileKeystrokes :: Benchmark
(benchDiagnosticsOneBigFileEdits, benchDiagnosticsOneBigFileKeystrokes) =
  withBothEditsAndKeystrokes benchDiagnosticsOneSmallFile
  where
  benchDiagnosticsOneSmallFile insertFuncName insert =
    benchLspSession ("one_big_file.mligo/" <> insertFuncName) projectWithOneBigFile $ do
    doc <- OpenedDoc <$> openLigoDoc "one_big_file.mligo"
    let drFile = doc
        drProject = projectWithOneBigFile
    diags1 <- waitDiagnostics DiagnosticsRequest {drFile, drProject, drExpected = Just $ def {expectedErrors = 0}}
    insert doc (8,1) "let the_type_error = (2 : string)"
    diags2 <- waitDiagnostics DiagnosticsRequest {drFile, drProject, drExpected = Just $ def {expectedErrors = 1}}
    insert doc (4,1) "let another_type_error = (3 : string)"
    diags3 <- waitDiagnostics DiagnosticsRequest {drFile, drProject, drExpected = Just $ def {expectedErrors = 2}}
    pure (diags1, diags2, diags3)

bench_diagnostics_edits :: [Benchmark]
bench_diagnostics_edits =
  [ bgroup
      "Diagnostics/edits"
      [ benchDiagnosticsOneSmallFileEdits
      , benchDiagnosticsOneSmallFileKeystrokes
      , benchDiagnosticsOneBigFileEdits
      , benchDiagnosticsOneBigFileKeystrokes
      ]
  ]
