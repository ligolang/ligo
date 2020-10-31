
module AST.Scope.Standard where

import Control.Exception.Safe (catchAny)

import AST.Scope.Common
import AST.Scope.Fallback
import AST.Scope.FromCompiler

import Cli.Types

data Standard

instance HasLigoClient m => HasScopeForest Standard m where
  scopeForest fname ast =
    scopeForest @FromCompiler fname ast `catchAny` \_ -> do
      scopeForest @Fallback fname ast
