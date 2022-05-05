-- | Document formatting capability

module AST.Capabilities.Format
  ( formatDocument
  , formatAt
  ) where

import Language.LSP.Types qualified as J

import AST.Scope (Info')
import AST.Skeleton (SomeLIGO (..))
import Cli (HasLigoClient, callForFormat)
import Duplo.Lattice (leq)
import Duplo.Tree (extract, spineTo)
import Log (Log)
import ParseTree (Source (..))
import Parser (CodeSource (..))
import Product (getElem)
import Range (Range (..), toLspRange)

formatDocument :: (HasLigoClient m, Log m) => SomeLIGO Info' -> m (J.List J.TextEdit)
formatDocument (SomeLIGO _lang (extract -> info)) = do
  let CodeSource source = getElem info
  let r@Range{_rFile} = getElem info
  out <- callForFormat (Source _rFile source)
  return . J.List $
    maybe [] (\out' -> [J.TextEdit (toLspRange r) out']) out

formatAt :: (HasLigoClient m, Log m) => Range -> SomeLIGO Info' -> m (J.List J.TextEdit)
formatAt at (SomeLIGO _lang tree) = case spineTo (leq at . getElem) tree of
  [] -> return $ J.List []
  (node:_) -> do
    let
      info = extract node
      CodeSource source = getElem info
      r@Range{_rFile} = getElem info
    out <- callForFormat (Source _rFile source)
    return . J.List $
      maybe [] (\out' -> [J.TextEdit (toLspRange r) out']) out
