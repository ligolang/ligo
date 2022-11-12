-- | Helpers for handling LIGO functions.

module Language.LIGO.Debugger.Functions
  ( LambdaMeta (..)
  , internalStackFrameName
  , embedFunctionNames
  , embedFunctionNameIntoLambda
  , getLambdaMeta
  ) where

import Control.Lens (lens, makeLensesWith)
import Data.Default (Default (..))
import Data.List.NonEmpty qualified as NE
import Data.Vinyl (Rec (RNil, (:&)))
import Fmt (Buildable (..))
import Text.Interpolation.Nyan

import Morley.Michelson.Interpret (StkEl (StkEl))
import Morley.Michelson.Typed qualified as T
import Morley.Util.Lens (postfixLFields)

import Language.LIGO.Debugger.CLI.Types

-- | A meta that we embed into @LAMBDA@ values when
-- interpreting a contract.
newtype LambdaMeta = LambdaMeta
  { lmVariables :: NonEmpty (LigoVariable 'Unique)
    -- ^ In this list we store names for stack frames
    -- that we should create when executing a lambda with this meta.
    -- The order of these names is reversed (e.g. if it is @["addImpl", "add"]@
    -- the next stack frames would be created: @["add", "addImpl"]@).
  } deriving stock (Show, Generic)
    deriving anyclass (NFData)

makeLensesWith postfixLFields ''LambdaMeta

instance Buildable LambdaMeta where
  build LambdaMeta{..} =
    [int||
    LambdaMeta
      variables: #{toList lmVariables}|]

instance Default LambdaMeta where
  def = LambdaMeta (LigoVariable (Name internalStackFrameName) :| [])

-- | A lens for accessing the meta of a lambda.
--
-- Returns @Nothing@ when the lambda is unwrapped.
lambdaMetaL :: Lens' (T.Value ('T.TLambda i o)) (Maybe LambdaMeta)
lambdaMetaL = lens
  do \(T.VLam lam) -> case T.rfAnyInstr lam of
       T.ConcreteMeta meta _ -> Just meta
       _ -> Nothing
  do let replaceMeta mMeta instr =
          let pureInstr = case instr of
                T.ConcreteMeta (_ :: LambdaMeta) i -> i
                i -> i
          in maybe id (T.Meta . T.SomeMeta) mMeta pureInstr
     \(T.VLam lam) mMeta -> T.VLam $ T.rfMapAnyInstr (replaceMeta mMeta) lam

-- | Variation of 'lambdaMetaL' that can look into arbitrary value,
-- doing nothing if it is not a lambda.
mLambdaMetaL :: Traversal' (T.Value t) (Maybe LambdaMeta)
mLambdaMetaL f = \case{ v@T.VLam{} -> lambdaMetaL f v; v -> pure v }

embedFunctionNameIntoLambda
  :: Maybe (LigoVariable 'Unique)
  -> T.Value t
  -> T.Value t
embedFunctionNameIntoLambda mVar =
  mLambdaMetaL %~ \mLambdaMeta ->
    let
      lambdaMeta = mLambdaMeta ?: def
      LigoVariable topLambdaName :| others = lambdaMeta ^. lmVariablesL
      lambdaVar@(LigoVariable lambdaName) = fromMaybe (LigoVariable (Name internalStackFrameName)) mVar

    in Just $ lambdaMeta
          & lmVariablesL %~
              if | topLambdaName `compareUniqueNames` Name internalStackFrameName -> const (lambdaVar :| others)
                  | lambdaName `compareUniqueNames` topLambdaName -> id
                  | otherwise -> NE.cons lambdaVar

tryToEmbedEnvIntoLambda :: (LigoStackEntry 'Unique, StkEl t) -> StkEl t
tryToEmbedEnvIntoLambda (LigoStackEntry LigoExposedStackEntry{..}, stkEl@(StkEl val)) =
  case leseType of
    LTArrow{} -> StkEl $ embedFunctionNameIntoLambda leseDeclaration val
    _ -> stkEl
tryToEmbedEnvIntoLambda (_, stkEl) = stkEl

embedFunctionNames :: Rec StkEl t -> LigoStack 'Unique -> Rec StkEl t
embedFunctionNames (x :& xs) (y : ys) = tryToEmbedEnvIntoLambda (y, x) :& embedFunctionNames xs ys
embedFunctionNames stack [] = stack
embedFunctionNames RNil _ = RNil

getLambdaMeta :: T.Value ('T.TLambda i o) -> LambdaMeta
getLambdaMeta = fromMaybe def . view lambdaMetaL
