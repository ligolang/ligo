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

import AST (ContractInfo')
import AST.Parser (collectAllErrors)
import AST.Skeleton (Error (..))
import Config (Config (..))
import Log qualified
import Range (Range (..), getRange, toLspRange)
import RIO.Types (RIO, getCustomConfig)

source :: Maybe J.DiagnosticSource
source = Just "ligo-lsp"

diagnostic :: J.TextDocumentVersion -> [(J.NormalizedUri, [J.Diagnostic])] -> RIO ()
diagnostic ver = Log.addNamespace "diagnostic" . traverse_ \(nuri, diags) -> do
  let diags' = D.partitionBySource diags
  maxDiagnostics <- _cMaxNumberOfProblems <$> getCustomConfig
  S.publishDiagnostics maxDiagnostics nuri ver diags'

collectErrors :: ContractInfo' -> J.TextDocumentVersion -> RIO ()
collectErrors contract version = do
  -- Correct the ranges of the error messages to correspond to real locations
  -- instead of locations after preprocessing.
  let errs' = nubBy (leq `on` fst) $ collectAllErrors contract
  let diags = errorToDiag <$> errs'
  let extractGroup :: [[(J.NormalizedUri, J.Diagnostic)]] -> [(J.NormalizedUri, [J.Diagnostic])]
      extractGroup [] = []
      extractGroup ([] : xs) = extractGroup xs
      extractGroup (ys@(y : _) : xs) = (fst y, snd <$> ys) : extractGroup xs
  let grouped = extractGroup $ groupBy ((==) `on` fst) $ sortOn fst diags
  diagnostic version grouped

clearDiagnostics :: Foldable f => f J.NormalizedUri -> RIO ()
clearDiagnostics uris = do
  maxDiagnostics <- _cMaxNumberOfProblems <$> getCustomConfig
  for_ uris \nuri ->
    S.publishDiagnostics maxDiagnostics nuri Nothing (Map.singleton source mempty)

errorToDiag :: (Range, Error a) -> (J.NormalizedUri, J.Diagnostic)
errorToDiag (getRange -> r, Error what _) =
  ( J.toNormalizedUri $ J.filePathToUri $ _rFile r
  , J.Diagnostic
    (toLspRange r)
    (Just J.DsError)
    Nothing
    source
    what
    (Just $ J.List [])
    Nothing
  )
