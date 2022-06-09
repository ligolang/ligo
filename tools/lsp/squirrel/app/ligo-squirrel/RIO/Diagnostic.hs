module RIO.Diagnostic
  ( source
  , collectErrors
  , clearDiagnostics
  ) where

import Data.Foldable (for_, traverse_)
import Data.Function (on)
import Data.List (groupBy, nubBy, sortOn)
import Data.Map qualified as Map
import Duplo.Lattice (Lattice (leq))
import Language.LSP.Diagnostics qualified as D
import Language.LSP.Server qualified as S
import Language.LSP.Types qualified as J

import AST (ContractInfo', Message (..), Severity (..))
import AST.Parser (collectAllErrors)
import Config (Config (..))
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
  -- Correct the ranges of the error messages to correspond to real locations
  -- instead of locations after preprocessing.
  let errs' = nubBy (leq `on` mRange) $ collectAllErrors contract
  let diags = errorToDiag <$> errs'
  let extractGroup :: [[(J.NormalizedUri, J.Diagnostic)]] -> [(J.NormalizedUri, [J.Diagnostic])]
      extractGroup [] = []
      extractGroup ([] : xs) = extractGroup xs
      extractGroup (ys@(y : _) : xs) = (fst y, snd <$> ys) : extractGroup xs
  let grouped = extractGroup $ groupBy ((==) `on` fst) $ sortOn fst diags
  diagnostic version grouped

clearDiagnostics :: Foldable f => f J.NormalizedUri -> RIO ()
clearDiagnostics uris = do
  maxDiagnostics <- _cMaxNumberOfProblems <$> S.getConfig
  for_ uris \nuri ->
    S.publishDiagnostics maxDiagnostics nuri Nothing (Map.singleton source mempty)

errorToDiag :: Message -> (J.NormalizedUri, J.Diagnostic)
errorToDiag (Message what severity r) =
  ( J.toNormalizedUri $ J.filePathToUri $ _rFile r
  , J.Diagnostic
    (toLspRange r)
    (Just dsSeverity)
    Nothing
    source
    what
    (Just $ J.List [])
    Nothing
  )
  where
    dsSeverity = case severity of
      SeverityError   -> J.DsError
      SeverityWarning -> J.DsWarning
