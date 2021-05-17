
module AST.Scope.Standard where

import Control.Exception.Safe

import AST.Scope.Common (HasScopeForest (..))
import AST.Scope.Fallback (Fallback)
import AST.Scope.FromCompiler (FromCompiler)

import Cli.Impl (LigoBinaryCallError (DecodedExpectedClientFailure, UnexpectedCrash))
import Cli.Json (fromLigoErrorToMsg)
import Cli.Types (HasLigoClient)

import Control.Monad.Catch (catchAll)

import AST.Skeleton (Error (Error))
import Duplo.Lattice (Lattice (leq))
import Parser (Msg)
import Range (point)

data Standard

instance HasLigoClient m => HasScopeForest Standard m where
  scopeForest fname ast msg =
    scopeForest @FromCompiler fname ast msg `catches`
      [ Handler \case
          (DecodedExpectedClientFailure err) -> addLigoErrToMsg err
          (UnexpectedCrash err) -> addErrMsg (point 1 1, Error err [])
          (_ :: LigoBinaryCallError) -> scopeForest @Fallback fname ast msg
      ] `catchAll` pure (scopeForest @Fallback fname ast msg)
    where
      addLigoErrToMsg = addErrMsg . fromLigoErrorToMsg

      addErrMsg err = do
        (fbAst, fbMsg) <- scopeForest @Fallback fname ast msg
        return (fbAst, fbMsg `rewriteAt` err)

      -- | Rewrite error message at the most local scope or append it to the end.
      rewriteAt :: [Msg] -> Msg -> [Msg]
      rewriteAt at what@(from, _) = filter (not . (from `leq`) . fst) at <> [what]
