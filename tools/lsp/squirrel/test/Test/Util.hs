module Test.Util
  ( readContract
  , readContractWithScopes
  ) where

import AST (parse)
import AST.Parser (Source (Path), parseWithScopes)
import AST.Scope.Common (HasScopeForest, Info')
import AST.Skeleton (LIGO)
import Parser (Info)

readContract :: FilePath -> IO (LIGO Info)
readContract filepath = fst <$> parse (Path filepath)

readContractWithScopes
  :: forall parser. HasScopeForest parser IO
  => FilePath -> IO (LIGO Info')
readContractWithScopes filepath
  = fst <$> parseWithScopes @parser (Path filepath)
