
module AST.Parser
  ( Source(..)
  , parse
  ) where

import qualified AST.Parser.Pascaligo as Pascal
import qualified AST.Parser.Reasonligo as Reason
import qualified AST.Parser.Camligo as CAML
import           AST.Skeleton

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
