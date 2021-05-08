module AST.Parser
  ( Source(..)
  , parse
  , parseWithScopes
  ) where

import Control.Monad.IO.Class (liftIO)

import qualified AST.Parser.Camligo as CAML
import qualified AST.Parser.Pascaligo as Pascal
import qualified AST.Parser.Reasonligo as Reason
import AST.Scope
import AST.Skeleton

import Extension
import ParseTree (Source (..), toParseTree)
import Parser

parse :: Source -> IO (SomeLIGO Info, [Msg])
parse src = do
  recogniser <- onExt ElimExt
    { eePascal = Pascal.recognise
    , eeCaml   = CAML.recognise
    , eeReason = Reason.recognise
    } (srcPath src)
  toParseTree src
    >>= (runParserM . recogniser)

-- | Parse with arbitrary parser.
parseWithScopes
  :: forall impl m. HasScopeForest impl m => Source -> m (SomeLIGO Info', [Msg])
parseWithScopes src = do
  (ast, msg) <- liftIO $ parse src
  addLocalScopes @impl src ast msg
