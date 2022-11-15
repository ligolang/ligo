{- | /The/ scope resolution system.
-}

module AST.Scope
  ( module M
  , addShallowScopes
  )
  where

import Algebra.Graph.Class qualified as G (vertex)
import Algebra.Graph.ToGraph qualified as G (vertexList)

import AST.Includes (insertPreprocessorRanges)
import AST.Scope.Common as M
import AST.Scope.Fallback as M
import AST.Scope.FromCompiler as M
import AST.Scope.Standard as M
import Unsafe

import Cli.Types (TempSettings)
import Progress (ProgressCallback)

-- | Like 'addScopes', but doesn't visit includes. That is, this function only
-- sees the scopes for the given contract, and doesn't try to visit includes.
addShallowScopes
  :: forall parser m
   . HasScopeForest parser m
  => TempSettings
  -> ProgressCallback m
  -> ContractInfo
  -> m ContractInfo'
addShallowScopes tempSettings reportProgress =
  (fmap (Unsafe.head . G.vertexList) . addScopes @parser tempSettings reportProgress . G.vertex)
  <=< insertPreprocessorRanges
