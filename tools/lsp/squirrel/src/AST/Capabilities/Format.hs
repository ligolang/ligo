-- | Document formatting capability

module AST.Capabilities.Format where

import Control.Exception.Safe
import Data.Functor ((<&>))
import Data.Text

import qualified Language.LSP.Types as J

import AST.Scope
import AST.Skeleton
import Cli
import Duplo.Lattice
import Duplo.Tree
import Extension (getExt)
import ParseTree
import Parser
import Product
import Range

callForFormat :: HasLigoClient m => Source -> m (Maybe Text)
callForFormat source = do
  ext <- getExt (srcPath source)
  let
    syntax = case ext of
      Reason -> "reasonligo"
      Pascal -> "pascaligo"
      Caml -> "cameligo"
  callLigo
    ["pretty-print", "/dev/stdin", "--syntax=" <> syntax]
    source <&> Just . fst
  `catchAny` \_ -> return Nothing

formatDocument :: HasLigoClient m => LIGO Info' -> m (J.List J.TextEdit)
formatDocument (extract -> info) = do
  let CodeSource source = getElem info
  let r@Range{rFile} = getElem info
  out <- callForFormat (Text rFile source)
  return . J.List $
    maybe [] (\out' -> [J.TextEdit (toLspRange r) out']) out

formatAt :: HasLigoClient m => Range -> LIGO Info' -> m (J.List J.TextEdit)
formatAt at tree = case spineTo (leq at . getElem) tree of
  [] -> return $ J.List []
  (node:_) -> do
    let
      info = extract node
      CodeSource source = getElem info
      r@Range{rFile} = getElem info
    out <- callForFormat (Text rFile source)
    return . J.List $
      maybe [] (\out' -> [J.TextEdit (toLspRange r) out']) out
