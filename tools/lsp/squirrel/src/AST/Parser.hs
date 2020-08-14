
module AST.Parser
  ( Source(..)
  , parse
  ) where

import Control.Monad.Catch

import System.FilePath

import qualified AST.Pascaligo.Parser as Pascal
import qualified AST.Reasonligo.Parser as Reason
import           AST.Skeleton

import ParseTree
import Parser

data UnsupportedExtension = UnsupportedExtension String
  deriving stock Show
  deriving anyclass Exception

parse :: Source -> IO (LIGO Info, [Msg])
parse src = do
  case takeExtension $ srcPath src of
    "religo" -> mkRawTreeReason src >>= runParserM . Reason.recognise
    "ligo"   -> mkRawTreePascal src >>= runParserM . Pascal.recognise
    ext      -> throwM $ UnsupportedExtension ext
