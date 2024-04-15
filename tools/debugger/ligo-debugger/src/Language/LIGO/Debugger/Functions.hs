-- | Helpers for handling LIGO functions.

module Language.LIGO.Debugger.Functions
  ( LambdaMeta
  , LambdaMeta' (..)
  , matchesUniqueLambdaName
  , LambdaArg (..)
  , LambdaNamedInfo (..)
  , ApplicationMeta (..)
  , lmApplicationMetaL
  , lmAllFuncNames
  , lmOriginalFuncName
  , lmActualFuncName
  , lambdaMetaL
  , internalStackFrameName
  , embedFunctionNames
  , embedFunctionNameIntoLambda
  , setAppliedArgs
  , getLambdaMeta
  , makeConciseApplicationMeta
  , extractApplicationMeta
  ) where

import Control.Lens (AsEmpty (..), lens, makeLensesWith, non', prism)
import Data.Default (Default (..))
import Data.Singletons (SingI)
import Data.Vinyl (Rec (RNil, (:&)))
import Fmt.Buildable (Buildable, build, pretty)
import Text.Interpolation.Nyan hiding (rmode')
import Util

import Morley.Michelson.Interpret (StkEl (MkStkEl))
import Morley.Michelson.Typed qualified as T
import Morley.Util.Lens (postfixLFields)

import Language.LIGO.Debugger.CLI

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

-- | An argument applied to lambda.
data LambdaArg u = LambdaArg
  { laValue :: T.SomeValue
  , laType :: LigoTypeF u
  } deriving stock (Generic)

deriving stock instance (Show (LigoTypeF u)) => Show (LambdaArg u)
deriving anyclass instance (NFData (LigoTypeF u)) => NFData (LambdaArg u)
deriving stock instance Eq (LambdaArg 'Concise)

makeLensesWith postfixLFields 'LambdaArg

instance (ForInternalUse, SingI u) => Buildable (LambdaArg u) where
  build (LambdaArg value ty) =
    [int||Value #{value} of type #{buildLigoTypeF @u ty}|]

-- | Information that comes along with a lambda being named.
data LambdaNamedInfo u = LambdaNamedInfo
  { lniName :: Name u
  , lniType :: LigoType
  } deriving stock (Show, Generic)
    deriving anyclass (NFData)

deriving stock instance Eq (LambdaNamedInfo 'Concise)

instance (SingI u, ForInternalUse) => Buildable (LambdaNamedInfo u) where
  build (LambdaNamedInfo name typ) = [int||Named as "#{name}" of type #{typ}|]

-- | A meta for partial application.
data ApplicationMeta u = ApplicationMeta
  { amFunctionName :: Maybe (Name u)
    -- ^ A name of applied function.
    -- It is @Nothing@ if applied expression isn't a function name.
    -- E.g. @(f a) b@.
  , amAppliedArguments :: [LambdaArg u]
  , amPreviousApplication :: Maybe (ApplicationMeta u)
    -- ^ We don't want to lose an information about previously applied
    -- functions.
    --
    -- E.g. we had the next execution:
    --
    -- @
    -- ...
    -- let foo = f a in
    -- let bar = foo b in
    -- let baz = bar c in
    -- ...
    -- @
    -- Take a @baz@ function. We want to show children for @bar@ and @foo@ in these cases.
  } deriving stock (Generic)

deriving stock instance (Show (LigoTypeF u)) => Show (ApplicationMeta u)
deriving anyclass instance (ForInternalUse, Buildable (LigoTypeF u), SingI u) => Buildable (ApplicationMeta u)
deriving stock instance Eq (ApplicationMeta 'Concise)
deriving anyclass instance (NFData (LigoTypeF u)) => NFData (ApplicationMeta u)

makeLensesWith postfixLFields 'ApplicationMeta

-- | This type is a stepping stone to getting meta which we will carry along
-- with lambdas - 'LambdaMeta'.
--
-- Unlike 'LambdaMeta', this type allows for different variables naming.
data LambdaMeta' u = LambdaMeta
  { lmNames :: [LambdaNamedInfo u]
    -- ^ Function names that related lambda has, last name goes first.
    --
    -- Considering a general case, usually lifetime of a lambda with many
    -- arguments looks like this: it gets applied those arguments
    -- one by one, and periodically it is assigned a new name (e.g. when
    -- is assigned to a variable).
    --
    -- For instance, if we have the code like this
    --
    -- @
    -- let add(a: int)(b: int) = a + b
    -- let add5(a: int) = add 5
    -- @
    --
    -- then variable named @add5@ is expected to have the following names:
    -- @["add5", "add"]@
    --
    -- Normally the first name (the last in the list) should be
    -- the original one (without partial applications), but
    -- we don't rely on this. For instance, the original function can be some
    -- LIGO-internal function that is first applied arguments and then gains
    -- a name from the user's scope; in such a situation, the arguments applied
    -- before the first naming are usually not interesting for us.
    --
    -- It can be that several namings occur (this case is possible for code like
    -- @op = add@).
    --
    -- Invariant: no subsequent namings should introduce the same name
    -- (names that are same string literals but come from different scopes are
    -- treated as different here). I.e. the fact the the lambda repeatedly
    -- gets assigned the same variable name should add only one name.

  , lmApplicationMeta :: Maybe (ApplicationMeta u)
    -- ^ If a lambda is created by partial application then
    -- this field will store all the information regarding this
    -- application.
  } deriving stock (Generic)

deriving stock instance (Show (LigoTypeF u)) => Show (LambdaMeta' u)
deriving anyclass instance (NFData (LigoTypeF u)) => NFData (LambdaMeta' u)

makeLensesWith postfixLFields ''LambdaMeta'

instance (SingI u, ForInternalUse) => Buildable (LambdaMeta' u) where
  build LambdaMeta{..} =
    [int||
    LambdaMeta
      variables: #{toList lmNames}|]

instance Default (LambdaMeta' u) where
  def = LambdaMeta [] Nothing

instance AsEmpty (LambdaMeta' u) where
  _Empty = prism def \case{ LambdaMeta [] Nothing -> Right (); other -> Left other }

-- | All function names, the most recent one goes first.
lmAllFuncNames :: LambdaMeta' u -> [Name u]
lmAllFuncNames = fmap lniName . lmNames

-- | The last known name of the lambda.
lmActualFuncName :: LambdaMeta' u -> LambdaName u
lmActualFuncName = maybe LNameUnknown LName . safeHead . reverse . lmAllFuncNames

-- | The original name of the lambda.
lmOriginalFuncName :: LambdaMeta' u -> LambdaName u
lmOriginalFuncName = maybe LNameUnknown LName . safeHead . lmAllFuncNames

-- | A meta that we carry along with lambda values when
-- interpreting a contract.
--
-- This type is pretty raw on itself, but we provide numerious smart getters.
type LambdaMeta = LambdaMeta' 'Unique

-- | A lens for accessing the meta of a lambda.
--
-- Returns @Nothing@ when the lambda is unwrapped.
lambdaMetaL :: Lens' (T.Value ('T.TLambda i o)) (Maybe LambdaMeta)
lambdaMetaL = lens
  -- TODO [LIGO-986]: check LambdaCodeRec is handled properly
  do let extractMeta = \case
           T.ConcreteMeta meta _ -> Just meta
           _ -> Nothing
     \case
       (T.VLam (T.LambdaCode lam)) -> extractMeta $ T.rfAnyInstr lam
       (T.VLam (T.LambdaCodeRec lam)) -> extractMeta $ T.rfAnyInstr lam

  do let replaceMeta mMeta instr =
          let pureInstr = case instr of
                T.ConcreteMeta (_ :: LambdaMeta) i -> i
                i -> i
          in maybe id (T.Meta . T.SomeMeta) mMeta pureInstr

     \(T.VLam lamVal) mMeta -> T.VLam $ case lamVal of
       T.LambdaCode lam ->
         T.LambdaCode $ T.rfMapAnyInstr (replaceMeta mMeta) lam
       T.LambdaCodeRec lam ->
         T.LambdaCodeRec $ T.rfMapAnyInstr (replaceMeta mMeta) lam

-- | Variation of 'lambdaMetaL' that can look into arbitrary value,
-- doing nothing if it is not a lambda.
mLambdaMetaL :: Traversal' (T.Value t) (Maybe LambdaMeta)
mLambdaMetaL f = \case{ v@T.VLam{} -> lambdaMetaL f v; v -> pure v }

embedFunctionNameIntoLambda
  :: LigoVariable 'Unique
  -> LigoType
  -> T.Value t
  -> T.Value t
embedFunctionNameIntoLambda (LigoVariable newName) newType =
  mLambdaMetaL . non' _Empty . lmNamesL %~ \case
    events@(namedInfo : _)
      | lniName namedInfo `compareUniqueNames` newName -> events
    events ->
      LambdaNamedInfo newName newType : events

tryToEmbedEnvIntoLambda :: LigoTypesVec -> (LigoStackEntry 'Unique, StkEl meta t) -> StkEl meta t
tryToEmbedEnvIntoLambda vec (LigoStackEntry LigoExposedStackEntry{..}, stkEl@(MkStkEl m val)) =
  case (leseDeclaration, vec `readLigoType` leseType) of
    (Just name, typ@(LigoTypeResolved LigoTypeExpression { _lteTypeContent = LTCArrow{} }))
      -> MkStkEl m $ embedFunctionNameIntoLambda name typ val
    _ -> stkEl
tryToEmbedEnvIntoLambda _ (_, stkEl) = stkEl

embedFunctionNames :: LigoTypesVec -> Rec (StkEl meta) t -> LigoStack 'Unique -> Rec (StkEl meta) t
embedFunctionNames vec (x :& xs) (y : ys) = tryToEmbedEnvIntoLambda vec (y, x) :& embedFunctionNames vec xs ys
embedFunctionNames _ stack [] = stack
embedFunctionNames _ RNil _ = RNil

-- | Set applied arguments in the next way:
-- 1. If the name of applied function is unknown (e.g. in @(f a) b@ cases) then
--    add the arguments to the previous one.
-- 2. Otherwise create a new meta and set into @amPreviousApplication@
--    field the previous one,
setAppliedArgs :: Maybe (Name 'Unique) -> [LambdaArg 'Unique] -> LambdaMeta -> LambdaMeta
setAppliedArgs functionNameMb args (LambdaMeta evs oldArgs) =
  let
    newArgs = mergeMetas oldArgs
  in LambdaMeta evs (Just newArgs)
  where
    defaultMeta = ApplicationMeta functionNameMb args oldArgs

    mergeMetas :: Maybe (ApplicationMeta 'Unique) -> ApplicationMeta 'Unique
    mergeMetas (Just inner@ApplicationMeta{..}) =
      case functionNameMb of
        Just{} -> defaultMeta
        Nothing -> inner{ amAppliedArguments = amAppliedArguments <> args }
    mergeMetas Nothing = defaultMeta

getLambdaMeta :: T.Value ('T.TLambda i o) -> LambdaMeta
getLambdaMeta = fromMaybe def . view lambdaMetaL

makeConciseLambdaArg :: LigoTypesVec -> LambdaArg 'Unique -> LambdaArg 'Concise
makeConciseLambdaArg vec arg =
  let
    laType = readLigoType vec (arg ^. laTypeL)
    laValue = arg ^. laValueL
  in LambdaArg{..}

makeConciseApplicationMeta :: LigoTypesVec -> ApplicationMeta 'Unique -> ApplicationMeta 'Concise
makeConciseApplicationMeta vec appMeta =
  let
    amFunctionName = pretty <$> appMeta ^. amFunctionNameL
    amAppliedArguments = makeConciseLambdaArg vec <$> appMeta ^. amAppliedArgumentsL
    amPreviousApplication = makeConciseApplicationMeta vec <$> appMeta ^. amPreviousApplicationL
  in ApplicationMeta{..}

extractApplicationMeta :: T.SomeValue -> Maybe (ApplicationMeta 'Unique)
extractApplicationMeta = \case
  T.SomeValue val@T.VLam{} -> lmApplicationMeta =<< val ^. lambdaMetaL
  _ -> Nothing
