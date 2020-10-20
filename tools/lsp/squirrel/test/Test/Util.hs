module Test.Util
  ( readContract
  , readContractWithScopes
  ) where

import AST (Standard, parse)
import AST.Parser (Source (Path), parseWithScopes)
import AST.Scope.Common (Info')
import AST.Skeleton (LIGO)
import Parser (Info)

readContract :: FilePath -> IO (LIGO Info)
readContract filepath = fst <$> parse (Path filepath)

readContractWithScopes :: FilePath -> IO (LIGO Info')
readContractWithScopes filepath
  = fst <$> parseWithScopes @Standard (Path filepath)
