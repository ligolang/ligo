-- | Helpers for handling LIGO functions.

module Language.LIGO.Debugger.Functions
  ( LambdaMeta (..)
  , internalStackFrameName
  , embedFunctionNames
  , embedFunctionNameIntoLambda
  , getLambdaMeta
  ) where

import Control.Lens (makeLensesWith)
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


embedFunctionNameIntoLambda
  :: Maybe (LigoVariable 'Unique)
  -> T.Value t
  -> T.Value t
embedFunctionNameIntoLambda mVar (T.VLam rf) = T.VLam $ embedIntoRemFail rf
  where
    embedIntoRemFail :: T.RemFail T.Instr i o -> T.RemFail T.Instr i o
    embedIntoRemFail = T.rfMapAnyInstr \case
      T.ConcreteMeta (lambdaMeta :: LambdaMeta) instr ->
        let
          LigoVariable topLambdaName :| others = lambdaMeta ^. lmVariablesL

          updatedMeta =
            lambdaMeta
              & lmVariablesL %~
                  if | topLambdaName `compareUniqueNames` Name internalStackFrameName -> const (lambdaVar :| others)
                      | lambdaName `compareUniqueNames` topLambdaName -> id
                      | otherwise -> NE.cons lambdaVar

        in T.Meta (T.SomeMeta updatedMeta) instr

      instr -> T.Meta (T.SomeMeta $ LambdaMeta (lambdaVar :| [])) instr

    lambdaVar@(LigoVariable lambdaName) = fromMaybe (LigoVariable (Name internalStackFrameName)) mVar
embedFunctionNameIntoLambda _ val = val

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
getLambdaMeta (T.VLam (T.rfAnyInstr -> instr)) =
  case instr of
    T.ConcreteMeta meta _ -> meta
    _ -> def
