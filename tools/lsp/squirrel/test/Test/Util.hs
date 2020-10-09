module Test.Util
  ( readContract
  , readContractWithScopes
  ) where

import Control.Monad.Reader (runReaderT)
import Data.Default (def)
import Data.Function ((&))
import Data.Functor ((<&>))

import AST (Standard, parse)
import AST.Parser (Source (Path), parseWithScopes)
import AST.Scope.Common (Info')
import AST.Skeleton (LIGO)
import Cli.Types (LigoClientEnv)
import Parser (Info)
import Product (Product (..))

readContract :: FilePath -> IO (LIGO Info)
readContract filepath = fst <$> parse (Path filepath)

readContractWithScopes :: FilePath -> IO (LIGO Info')
readContractWithScopes filepath
  = parseWithScopes @Standard (Path filepath)
  <&> fst
