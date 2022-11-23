module RIO.Diagnostic
  ( source
  , collectErrors
  , clearDiagnostics
  ) where

import Data.List (groupBy)
import Data.Map qualified as Map
import Language.LSP.Diagnostics qualified as D
import Language.LSP.Server qualified as S
import Language.LSP.Types qualified as J
import Witherable (ordNubOn)

import AST (ContractInfo')
import AST.Parser (collectAllErrors)
import Config (Config (..))
import Diagnostic (Message (..), Severity (..), filterDiagnostics)
import Duplo.Pretty (ppToText)
import Log qualified
import Range (Range (..), toLspRange)
import RIO.Types (RIO)

source :: Maybe J.DiagnosticSource
source = Just "ligo-lsp"

diagnostic :: J.TextDocumentVersion -> [(J.NormalizedUri, [J.Diagnostic])] -> RIO ()
diagnostic ver = Log.addNamespace "diagnostic" . traverse_ \(nuri, diags) -> do
  let diags' = D.partitionBySource diags
  maxDiagnostics <- _cMaxNumberOfProblems <$> S.getConfig
  S.publishDiagnostics maxDiagnostics nuri ver diags'

collectErrors :: ContractInfo' -> J.TextDocumentVersion -> RIO ()
collectErrors contract version = do
  -- Filter out recognizer errors (unless there are parser errors).
  -- FIXME (LIGO-507): Remove duplicated diagnostics.
  let errs' = ordNubOn mRange $ filterDiagnostics $ collectAllErrors contract
  let diags = errorToDiag <$> errs'
  let extractGroup :: [[(J.NormalizedUri, J.Diagnostic)]] -> [(J.NormalizedUri, [J.Diagnostic])]
      extractGroup [] = []
      extractGroup ([] : xs) = extractGroup xs
      extractGroup (ys@(y : _) : xs) = (fst y, snd <$> ys) : extractGroup xs
  let grouped = extractGroup $ groupBy ((==) `on` fst) $ sortWith fst diags
  diagnostic version grouped

clearDiagnostics :: J.NormalizedUri -> RIO ()
clearDiagnostics nuri = do
  maxDiagnostics <- _cMaxNumberOfProblems <$> S.getConfig
  S.publishDiagnostics maxDiagnostics nuri Nothing (Map.singleton source mempty)

errorToDiag :: Message -> (J.NormalizedUri, J.Diagnostic)
errorToDiag (Message what severity r) =
  ( J.toNormalizedUri $ J.filePathToUri $ _rFile r
  , J.Diagnostic
    (toLspRange r)
    (Just dsSeverity)
    Nothing
    source
    (ppToText what)
    (Just $ J.List [])
    Nothing
  )
  where
    dsSeverity = case severity of
      SeverityError   -> J.DsError
      SeverityWarning -> J.DsWarning
