module AST.Scope.Standard where

import Control.Arrow (Arrow (second))
import Control.Exception.Safe

import AST.Scope.Common (HasScopeForest (..), mergeScopeForest)
import AST.Scope.Fallback (Fallback)
import AST.Scope.FromCompiler (FromCompiler)
import AST.Skeleton (Error (Error))

import Cli.Impl
import Cli.Json (fromLigoErrorToMsg)
import Cli.Types (HasLigoClient)

import Duplo.Lattice (Lattice (leq))
import Parser (Msg)
import Range (point)

data Standard

instance HasLigoClient m => HasScopeForest Standard m where
  scopeForest fname ast msg = do
    (lgForest, lgMsg) <- scopeForest @FromCompiler fname ast msg `catches`
      [ Handler \case
          -- catch only errors that we expect from ligo and try to use fallback parser
          LigoDecodedExpectedClientFailureException err -> addLigoErrToMsg $ fromLigoErrorToMsg err
      , Handler \case
          LigoUnexpectedCrashException err -> addLigoErrToMsg (point 1 1, Error err [])
      , Handler \case
          -- all other errors such as "Not found in $PATH" and other exceptions are ignored
          (_ :: SomeException) -> fallbackForest
      ]
    (fbForest, fbMsg) <- fallbackForest
    pure (mergeScopeForest lgForest fbForest, lgMsg <> fbMsg)
    where
      fallbackForest = scopeForest @Fallback fname ast msg

      addLigoErrToMsg err = second (`rewriteAt` err) <$> fallbackForest

      -- | Rewrite error message at the most local scope or append it to the end.
      rewriteAt :: [Msg] -> Msg -> [Msg]
      rewriteAt at what@(from, _) = filter (not . (from `leq`) . fst) at <> [what]
