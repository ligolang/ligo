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
import Product (Product, getElem)
import Range (Range (..), toLspRange)

formatImpl :: (HasLigoClient m, Log m) => FilePath -> Product Info' -> m (J.List J.TextEdit)
formatImpl projDir info = do
  let CodeSource source = getElem info
  let r@Range{_rFile} = getElem info
  out <- callForFormat projDir (Source _rFile source)
  return . J.List $
    maybe [] (\out' -> [J.TextEdit (toLspRange r) out']) out

formatDocument :: (HasLigoClient m, Log m) => FilePath -> SomeLIGO Info' -> m (J.List J.TextEdit)
formatDocument projDir (SomeLIGO _lang (extract -> info)) = formatImpl projDir info

formatAt :: (HasLigoClient m, Log m) => FilePath -> Range -> SomeLIGO Info' -> m (J.List J.TextEdit)
formatAt projDir at (SomeLIGO _lang tree) = case spineTo (leq at . getElem) tree of
  [] -> return $ J.List []
  node : _ -> formatImpl projDir $ extract node
