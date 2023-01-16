-- | Document formatting capability

module AST.Capabilities.Format
  ( formatDocument
  , formatAt
  ) where

import Prelude hiding (Product (..))

import Language.LSP.Types qualified as J
import UnliftIO.Exception (Handler (..), catches)

import AST.Scope (ContractInfo, pattern FindContract)
import AST.Skeleton (SomeLIGO (..))
import Cli (HasLigoClient, LigoIOException, SomeLigoException, TempSettings, callForFormat)
import Duplo.Lattice (leq)
import Duplo.Tree (extract, spineTo)
import Log (Log)
import Log qualified
import Parser (CodeSource (..), Info)
import ParseTree (Source (..))
import Product (Product, getElem)
import Range (Range (..), toLspRange)

formatImpl
  :: (HasLigoClient m, Log m)
  => TempSettings
  -> Source
  -> Product Info
  -> m (J.List J.TextEdit)
formatImpl tempSettings src info = do
  let CodeSource source = getElem info
  let r@Range{_rFile} = getElem info
  out <- callForFormat tempSettings (Source _rFile (srcIsDirty src) source) `catches`
    [ Handler \(e :: LigoIOException) ->
      source <$ $Log.err [Log.i|#{displayException e}|]
    , Handler \(_ :: SomeLigoException) -> pure source
    ]
  pure $ J.List [J.TextEdit (toLspRange r) out]

formatDocument
  :: (HasLigoClient m, Log m)
  => TempSettings
  -> ContractInfo
  -> m (J.List J.TextEdit)
formatDocument tempSettings (FindContract src (SomeLIGO _lang (extract -> info)) _) =
  formatImpl tempSettings src info

formatAt
  :: (HasLigoClient m, Log m)
  => TempSettings
  -> Range
  -> ContractInfo
  -> m (J.List J.TextEdit)
formatAt tempSettings at (FindContract src (SomeLIGO _lang tree) _) =
  case spineTo (leq at . getElem) tree of
    [] -> pure $ J.List []
    node : _ -> formatImpl tempSettings src $ extract node
