
module AST.Parser
  ( Source(..)
  , parse
  ) where

import qualified AST.Pascaligo.Parser as Pascal
import qualified AST.Reasonligo.Parser as Reason
import qualified AST.Camligo.Parser as CAML
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
