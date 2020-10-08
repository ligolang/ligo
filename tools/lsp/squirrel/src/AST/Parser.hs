
module AST.Parser
  ( Source(..)
  , parse
  , parseWithScopes
  ) where

import Control.Monad.IO.Class (liftIO)

import qualified AST.Parser.Pascaligo as Pascal
import qualified AST.Parser.Reasonligo as Reason
import qualified AST.Parser.Camligo as CAML
import           AST.Skeleton
import           AST.Scope

import ParseTree
import Parser
import Extension

parse :: Source -> IO (LIGO Info, [Msg])
parse src = do
  recogniser <- onExt ElimExt
    { eePascal = Pascal.recognise
    , eeCaml   = CAML.recognise
    , eeReason = Reason.recognise
    } (srcPath src)
  toParseTree src >>= runParserM . recogniser

parseWithScopes :: forall impl m. HasScopeForest impl m => Source -> m (LIGO Info', [Msg])
parseWithScopes src = do
  recogniser <- liftIO do
    onExt ElimExt
      { eePascal = Pascal.recognise
      , eeCaml   = CAML.recognise
      , eeReason = Reason.recognise
      } (srcPath src)

  (ast, msg) <- liftIO do
    toParseTree src >>= runParserM . recogniser

  ast' <- addLocalScopes @impl (srcPath src) ast
  return (ast', msg)
