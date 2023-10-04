module Bench.Util
  (
    benchLspSession,
    getDoc,
    Doc (..),
    insertToDoc,
    insertToDocKeystrokes,
    openLigoDoc,
    projectWithOneBigFile,
    projectWithOneSmallFile,
    projectChecker,
    DiagnosticsPullingMethod(..),
    pullDiagnostics,
    withBothEditsAndKeystrokes,
    BenchmarkSequence(..),
    benchSequence,
  )
where

import Prelude qualified
import Universum hiding ((^.))

import Criterion
import Data.Aeson qualified as Aeson
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Row.Records ((.+), (.==))
import Language.LSP.Protocol.Capabilities qualified as LSP
import Language.LSP.Protocol.Lens qualified as LspLens
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Test (SessionConfig (logStdErr, messageTimeout), lspConfig)
import Language.LSP.Test qualified as LSP
import Lens.Micro ((^.))
import System.Directory (canonicalizePath)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)


-- one can set LIGO_LSP_TEST_EXE env var before testing/benchmarking to test a different lsp server
-- with our requests
serverCommand :: String
serverCommand =
  unsafePerformIO $
    fromMaybe "ligo lsp --disable-lsp-requests-logging all-capabilities"
      <$> lookupEnv "LIGO_LSP_TEST_EXE"
{-# NOINLINE serverCommand #-}

projectWithOneBigFile :: FilePath
projectWithOneBigFile = unsafePerformIO $ canonicalizePath "projects/one_big_file/"
{-# NOINLINE projectWithOneBigFile #-}

projectWithOneSmallFile :: FilePath
projectWithOneSmallFile = unsafePerformIO $ canonicalizePath "projects/one_small_file/"
{-# NOINLINE projectWithOneSmallFile #-}

projectChecker :: FilePath
projectChecker = unsafePerformIO $ canonicalizePath "projects/submodules/checker/src/"
{-# NOINLINE projectChecker #-}

openLigoDoc :: FilePath -> LSP.Session LSP.TextDocumentIdentifier
openLigoDoc fp = LSP.openDoc fp "ligo"

benchLspSession ::
  (NFData a) =>
  DiagnosticsPullingMethod ->
  String ->
  FilePath ->
  LSP.Session a ->
  Benchmark
benchLspSession pullingMethod description project session =
  bench description $
    nfIO $
      LSP.runSessionWithConfig
        LSP.defaultConfig
          { logStdErr = True
            -- 10 minutes should be enough
          , messageTimeout = 600
          , lspConfig
          }
        serverCommand
        (LSP.capsForVersion $ LSP.LSPVersion 3 0)
        project
        session
  where
    lspConfig =
      case pullingMethod of
        OnDoc -> Just
          [aesonQQ|
          {
            "ligoLanguageServer": {
              "diagnosticsPullMode": "on doc update (can be slow)"
            }
          }
          |]
        OnDocumentLink -> Nothing

getDoc :: Doc -> LSP.Session LSP.TextDocumentIdentifier
getDoc (OpenedDoc tdi) = pure tdi
getDoc (FileDoc path) = openLigoDoc path

data Doc
  = -- | Document on disc to be opened (path relative to project)
    FileDoc String
  | -- | Document that was already opened in this session
    OpenedDoc LSP.TextDocumentIdentifier
  deriving Eq

instance Prelude.Show Doc where
  show (FileDoc s) = s
  show (OpenedDoc uri) = show uri


insertToDoc, insertToDocKeystrokes :: Doc -> (LSP.UInt, LSP.UInt) -> Text -> LSP.Session ()
insertToDoc doc (line, col) text =
  getDoc doc >>= \tdi ->
    LSP.changeDoc tdi [LSP.TextDocumentContentChangeEvent $ LSP.InL $ #range .== LSP.Range pos pos .+ #rangeLength .== Nothing .+ #text .== text]
  where
    pos = LSP.Position (line - 1) (col - 1)

-- | Like insertToDoc but simulates typing
insertToDocKeystrokes doc (line, col) text =
  getDoc doc >>= \tdi ->
    for_ (Universum.zip (toString text) [col ..]) $
      \(char, activeCol) -> insertToDoc (OpenedDoc tdi) (line, activeCol) (one char)

data DiagnosticsPullingMethod
  = OnDoc
  | OnDocumentLink

instance Prelude.Show DiagnosticsPullingMethod where
  show = \case
    OnDoc -> "on_doc"
    OnDocumentLink -> "on_document_link"

{-# ANN pullDiagnostics ("HLint: ignore Redundant fmap" :: String) #-}
pullDiagnostics :: LSP.TextDocumentIdentifier -> DiagnosticsPullingMethod -> LSP.Session [LSP.Diagnostic]
pullDiagnostics tdi = \case
  OnDoc -> do
    _ <- let params = Aeson.Null in LSP.sendRequest (LSP.SMethod_CustomMethod $ Proxy @"DebugEcho") params
    -- After we edited a document multiple times, the LSP server might produce multiple publish diagnostics
    -- notifications
    firstMessage <- LSP.waitForDiagnostics
    otherDiagnosticMessages <- many LSP.publishDiagnosticsNotification
    echoResponse <- LSP.response (LSP.SMethod_CustomMethod $ Proxy @"DebugEcho")
    unless (echoResponse ^. LspLens.result == Right (Aeson.String "DebugEchoResponse"))
      $ fail "Expected DebugEchoResponse"
    case nonEmpty otherDiagnosticMessages of
      Nothing -> pure firstMessage
      Just messages -> pure $ last messages ^. (LspLens.params . LspLens.diagnostics)
  OnDocumentLink -> do
    _ <- let params = LSP.DocumentLinkParams Nothing Nothing tdi in LSP.sendRequest LSP.SMethod_TextDocumentDocumentLink params
    firstMessage <- LSP.waitForDiagnostics
    fmap nonEmpty (many LSP.publishDiagnosticsNotification) >>= \case
      Nothing -> pure firstMessage
      Just messages -> pure $ last messages ^. (LspLens.params . LspLens.diagnostics)

withBothEditsAndKeystrokes
  :: ( DiagnosticsPullingMethod
    -> String
    -> (Doc -> (LSP.UInt, LSP.UInt) -> Text -> LSP.Session ())
    -> a
     ) -> (a,a,a,a)
withBothEditsAndKeystrokes f =
  ( f OnDoc "edits" insertToDoc
  , f OnDoc "keystrokes" insertToDocKeystrokes
  , f OnDocumentLink "edits" insertToDoc
  , f OnDocumentLink "keystrokes" insertToDocKeystrokes
  )

data BenchmarkSequence :: Type -> Type where
  BenchmarkSequence :: NFData a =>
    { bsName :: String
    , bsRequests :: [request]
    , bsGetRequestDoc :: request -> Doc
    , bsSetRequestDoc :: Doc -> request -> request
    , bsRunRequest :: request -> LSP.Session a
    , bsProject :: FilePath
    } -> BenchmarkSequence request

-- Runs multiple similar requests for one file (opening this file only once).
benchSequence :: BenchmarkSequence request -> Benchmark
benchSequence BenchmarkSequence {..} =
  -- Picking "OnDocumentLink" here because it's faster. Other pulling methods
  -- would be measured in diagnostics benches.
  benchLspSession OnDocumentLink (bsName <> "/" <> file) bsProject $ do
    doc <- getDoc (FileDoc file)
    mapM (bsRunRequest . bsSetRequestDoc (OpenedDoc doc)) bsRequests
  where
  file = case map bsGetRequestDoc bsRequests of
    FileDoc f : otherFiles | all (== FileDoc f) otherFiles -> f
    _ -> error "Expected a same FileDoc for all files"
