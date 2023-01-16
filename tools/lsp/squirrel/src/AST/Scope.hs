{- | /The/ scope resolution system.
-}

module AST.Scope
  ( module M
  , addShallowScopes
  , ScopingSystem (..)
  , KnownScopingSystem (..)
  )
  where

import Algebra.Graph.Class qualified as G (vertex)
import Algebra.Graph.ToGraph qualified as G (vertexList)
import Data.Aeson
import Text.Show qualified
import Unsafe qualified

import AST.Includes (insertPreprocessorRanges)
import AST.Scope.Common as M
import AST.Scope.Fallback as M
import AST.Scope.FromCompiler as M
import AST.Scope.Standard as M

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

data ScopingSystem = CompilerScopes | FallbackScopes | StandardScopes
  deriving stock Eq

instance Show ScopingSystem where
  show FallbackScopes = "Fallback"
  show StandardScopes = "Standard"
  show CompilerScopes = "FromCompiler"

type instance PrettyShow ScopingSystem = ()

instance ToJSON ScopingSystem where
  toJSON = String . show

instance FromJSON ScopingSystem where
  parseJSON = withText "Scoping system" $ \case
    "Fallback" -> pure FallbackScopes
    "Standard" -> pure StandardScopes
    "FromCompiler" -> pure CompilerScopes
    bad -> fail $ "Unknown scoping system: " <> toString bad

class KnownScopingSystem impl where
  knownScopingSystem :: ScopingSystem

instance KnownScopingSystem FromCompiler where
  knownScopingSystem = CompilerScopes
instance KnownScopingSystem Fallback where
  knownScopingSystem = FallbackScopes
instance KnownScopingSystem Standard where
  knownScopingSystem = StandardScopes
