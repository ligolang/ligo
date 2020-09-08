
module AST.Scope.Standard where

import Control.Monad.Catch

import AST.Scope.Common
import AST.Scope.Fallback
import AST.Scope.FromCompiler

import Cli.Types

data Standard

instance HasLigoClient m => HasScopeForest Standard m where
  scopeForest fname ast =
    scopeForest @FromCompiler fname ast `catchAll` \e -> do
      scopeForest @Fallback fname ast
