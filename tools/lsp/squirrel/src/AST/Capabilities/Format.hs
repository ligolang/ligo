-- | Document formatting capability

module AST.Capabilities.Format where

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

callForFormat :: HasLigoClient m => Lang -> Source -> m (Maybe Text)
callForFormat lang source =
    (Just . fst <$> getResult) `catchAny` \_ -> return Nothing
  where
    syntax = case lang of
      Reason -> "reasonligo"
      Pascal -> "pascaligo"
      Caml -> "cameligo"

    getResult = callLigo
      ["pretty-print", "/dev/stdin", "--syntax=" <> syntax]
      source

formatDocument :: HasLigoClient m => SomeLIGO Info' -> m (J.List J.TextEdit)
formatDocument (SomeLIGO lang (extract -> info)) = do
  let CodeSource source = getElem info
  let r@Range{rFile} = getElem info
  out <- callForFormat lang (Text rFile source)
  return . J.List $
    maybe [] (\out' -> [J.TextEdit (toLspRange r) out']) out

formatAt :: HasLigoClient m => Range -> SomeLIGO Info' -> m (J.List J.TextEdit)
formatAt at (SomeLIGO lang tree) = case spineTo (leq at . getElem) tree of
  [] -> return $ J.List []
  (node:_) -> do
    let
      info = extract node
      CodeSource source = getElem info
      r@Range{rFile} = getElem info
    out <- callForFormat lang (Text rFile source)
    return . J.List $
      maybe [] (\out' -> [J.TextEdit (toLspRange r) out']) out
