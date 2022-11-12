-- | Helpers for handling LIGO functions.

module Language.LIGO.Debugger.Functions
  ( LambdaMeta (..)
  , matchesUniqueLambdaName
  , lambdaMetaL
  , internalStackFrameName
  , embedFunctionNames
  , embedFunctionNameIntoLambda
  , getLambdaMeta
  ) where

import Control.Lens (AsEmpty (..), lens, makeLensesWith, non', prism)
import Data.Default (Default (..))
import Data.List.NonEmpty qualified as NE
import Data.Singletons (SingI)
import Data.Vinyl (Rec (RNil, (:&)))
import Fmt (Buildable (..))
import Text.Interpolation.Nyan

import Morley.Michelson.Interpret (StkEl (StkEl))
import Morley.Michelson.Typed qualified as T
import Morley.Util.Lens (postfixLFields)

import Language.LIGO.Debugger.CLI.Types

-- | Registered name of a lambda.
data LambdaName u
  = LName (Name u)
    -- ^ We know a particular name.
  | LNameUnknown
    -- ^ Name is yet unknown.
    -- It be figured out later, or this lambda may be some LIGO-internal thing.
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

deriving stock instance Eq (LambdaName 'Concise)

instance SingI u => Buildable (LambdaName u) where
  build = \case
    LName n -> build n
    LNameUnknown -> build internalStackFrameName

matchesUniqueLambdaName :: Name 'Unique -> LambdaName 'Unique -> Bool
matchesUniqueLambdaName n1 = \case
  LName n2 -> n1 `compareUniqueNames` n2
  LNameUnknown -> False

-- | A meta that we embed into @LAMBDA@ values when
-- interpreting a contract.
newtype LambdaMeta = LambdaMeta
  { lmVariables :: NonEmpty (LambdaName 'Unique)
    -- ^ In this list we store names for stack frames
    -- that we should create when executing a lambda with this meta.
    -- The order of these names is reversed (e.g. if it is @["addImpl", "add"]@
    -- the next stack frames would be created: @["add", "addImpl"]@).
    --
    -- Sometimes the name remains unknown, this is represented by @Nothing@.
  } deriving stock (Show, Generic)
    deriving anyclass (NFData)

makeLensesWith postfixLFields ''LambdaMeta

instance Buildable LambdaMeta where
  build LambdaMeta{..} =
    [int||
    LambdaMeta
      variables: #{toList lmVariables}|]

instance Default LambdaMeta where
  def = LambdaMeta (LNameUnknown :| [])

instance AsEmpty LambdaMeta where
  _Empty = prism
    (def)
    \case{ LambdaMeta (LNameUnknown :| []) -> Right (); other -> Left other }

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
  :: LigoVariable 'Unique
  -> T.Value t
  -> T.Value t
embedFunctionNameIntoLambda (LigoVariable newName) =
  mLambdaMetaL . non' _Empty %~ \lambdaMeta ->
    let
      mLastPresentName :| others = lambdaMeta ^. lmVariablesL

    in lambdaMeta
          & lmVariablesL %~ case mLastPresentName of
              LNameUnknown -> const (LName newName :| others)
              LName lastPresentName
                | lastPresentName `compareUniqueNames` newName -> id
                | otherwise -> NE.cons (LName newName)

tryToEmbedEnvIntoLambda :: (LigoStackEntry 'Unique, StkEl t) -> StkEl t
tryToEmbedEnvIntoLambda (LigoStackEntry LigoExposedStackEntry{..}, stkEl@(StkEl val)) =
  case leseType of
    LTArrow{} -> StkEl $ maybe id embedFunctionNameIntoLambda leseDeclaration val
    _ -> stkEl
tryToEmbedEnvIntoLambda (_, stkEl) = stkEl

embedFunctionNames :: Rec StkEl t -> LigoStack 'Unique -> Rec StkEl t
embedFunctionNames (x :& xs) (y : ys) = tryToEmbedEnvIntoLambda (y, x) :& embedFunctionNames xs ys
embedFunctionNames stack [] = stack
embedFunctionNames RNil _ = RNil

getLambdaMeta :: T.Value ('T.TLambda i o) -> LambdaMeta
getLambdaMeta = fromMaybe def . view lambdaMetaL
