module Test.Util
  ( readContract
  , readContractWithMessages
  , readContractWithScopes
  ) where

import AST (parse)
import AST.Parser (Source (Path), parseWithScopes)
import AST.Scope.Common (HasScopeForest, Info')
import AST.Skeleton (LIGO, SomeLIGO)
import Parser (Info, Msg)

readContract :: FilePath -> IO (SomeLIGO Info)
readContract filepath = fst <$> parse (Path filepath)

readContractWithMessages :: FilePath -> IO (SomeLIGO Info, [Msg])
readContractWithMessages filepath = parse (Path filepath)

readContractWithScopes
  :: forall parser. HasScopeForest parser IO
  => FilePath -> IO (SomeLIGO Info')
readContractWithScopes filepath
  = fst <$> parseWithScopes @parser (Path filepath)
