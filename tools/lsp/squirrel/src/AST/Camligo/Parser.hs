
module AST.Camligo.Parser where

import Data.Maybe (isJust)

import AST.Skeleton

import Duplo.Error
import Duplo.Tree
import Duplo.Pretty

import Product
import Parser
import ParseTree

-- import Debug.Trace

example :: FilePath
example = "../../../src/test/contracts/address.mligo"

raw :: IO ()
raw = toParseTree (Path example)
  >>= print . pp

-- sample :: IO ()
-- sample
--   =   toParseTree (Path example)
--   >>= runParserM . recognise
--   >>= print . pp . fst
