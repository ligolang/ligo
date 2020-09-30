module Test.Util
  ( readContract
  ) where

import AST (parse)
import AST.Skeleton (LIGO)
import Parser (Info)
import ParseTree (Source (Path))

readContract :: FilePath -> IO (LIGO Info)
readContract filepath = fst <$> parse (Path filepath)
