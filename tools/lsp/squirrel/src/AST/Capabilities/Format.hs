-- | Document formatting capability

module AST.Capabilities.Format
  ( formatDocument
  , formatAt
  ) where

import Language.LSP.Types qualified as J
import UnliftIO.Exception (Handler (..), catches, displayException)

import AST.Scope (Info')
import AST.Skeleton (SomeLIGO (..))
import Cli (HasLigoClient, SomeLigoException, TempSettings, callForFormat)
import Duplo.Lattice (leq)
import Duplo.Tree (extract, spineTo)
import Log (Log)
import Log qualified
import ParseTree (Source (..))
import Parser (CodeSource (..))
import Product (Product, getElem)
import Range (Range (..), toLspRange)

formatImpl
  :: (HasLigoClient m, Log m)
  => TempSettings
  -> Product Info'
  -> m (J.List J.TextEdit)
formatImpl tempSettings info = do
  let CodeSource source = getElem info
  let r@Range{_rFile} = getElem info
  out <- callForFormat tempSettings (Source _rFile source) `catches`
    [ Handler \(_ :: SomeLigoException) -> pure source
    -- Likely LIGO isn't installed or was not found.
    , Handler \(e :: IOError) ->
      source <$ $(Log.err) [Log.i|Couldn't call LIGO, failed with #{displayException e}|]
    ]
  pure $ J.List [J.TextEdit (toLspRange r) out]

formatDocument
  :: (HasLigoClient m, Log m)
  => TempSettings
  -> SomeLIGO Info'
  -> m (J.List J.TextEdit)
formatDocument tempSettings (SomeLIGO _lang (extract -> info)) = formatImpl tempSettings info

formatAt
  :: (HasLigoClient m, Log m)
  => TempSettings
  -> Range
  -> SomeLIGO Info'
  -> m (J.List J.TextEdit)
formatAt tempSettings at (SomeLIGO _lang tree) = case spineTo (leq at . getElem) tree of
  [] -> return $ J.List []
  node : _ -> formatImpl tempSettings $ extract node
