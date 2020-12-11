module AST.Parser
  ( Source(..)
  , parse
  , parseWithScopes
  ) where

import Control.Lens (_1, view, (%~), (<&>), (^.))
import Control.Monad.IO.Class (liftIO)

import qualified AST.Parser.Camligo as CAML
import qualified AST.Parser.Pascaligo as Pascal
import qualified AST.Parser.Reasonligo as Reason
import AST.Scope
import AST.Skeleton

import Extension
import ParseTree (Source (..), toParseTree)
import Parser

parse :: Source -> IO (LIGO Info, [Msg])
parse src = do
  recogniser <- onExt ElimExt
    { eePascal = Pascal.recognise
    , eeCaml   = CAML.recognise
    , eeReason = Reason.recognise
    } (srcPath src)
  toParseTree src
    >>= (runParserM . recogniser)
    <&> (_1 %~ view nestedLIGO)

-- | Parse with arbitrary parser.
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

  (ast', msg') <- addLocalScopes @impl src ast msg
  return (ast' ^. nestedLIGO, msg')

