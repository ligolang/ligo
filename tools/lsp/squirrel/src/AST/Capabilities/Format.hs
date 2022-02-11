-- | Document formatting capability

module AST.Capabilities.Format
  ( formatDocument
  , formatAt
  ) where

import Control.Exception.Safe (catchAny)
import Data.Text (Text)

import Language.LSP.Types qualified as J

import AST.Scope
import AST.Skeleton
import Cli
import Duplo.Lattice
import Duplo.Tree
import ParseTree
import Parser
import Product
import Range

callForFormat :: HasLigoClient m => Source -> m (Maybe Text)
callForFormat source =
    (Just . fst <$> getResult) `catchAny` \_ -> return Nothing
  where
    path = srcPath source

    getResult = callLigo
      ["print", "pretty", path]
      source

formatDocument :: HasLigoClient m => SomeLIGO Info' -> m (J.List J.TextEdit)
formatDocument (SomeLIGO _lang (extract -> info)) = do
  let CodeSource source = getElem info
  let r@Range{_rFile} = getElem info
  out <- callForFormat (Text _rFile source)
  return . J.List $
    maybe [] (\out' -> [J.TextEdit (toLspRange r) out']) out

formatAt :: HasLigoClient m => Range -> SomeLIGO Info' -> m (J.List J.TextEdit)
formatAt at (SomeLIGO _lang tree) = case spineTo (leq at . getElem) tree of
  [] -> return $ J.List []
  (node:_) -> do
    let
      info = extract node
      CodeSource source = getElem info
      r@Range{_rFile} = getElem info
    out <- callForFormat (Text _rFile source)
    return . J.List $
      maybe [] (\out' -> [J.TextEdit (toLspRange r) out']) out
