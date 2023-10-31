module Test.Util
  ( -- * Shared helpers
    (</>)
  , (<.>)
  , contractsDir
  , getCompilerContractsDir
  , AST.Lang (..)
  , AST.allLangs
  , AST.langExtension
  , pattern SomeLorentzValue
  , rmode'
  , legacyMode

    -- * Test utilities
  , (@?=)
  , (@@?=)
  , (@?)
  , (@@?)
  , (@~=?)
  , HUnit.testCase
  , HUnit.testCaseSteps
  , HUnit.assertFailure
  , HUnit.assertBool
  , getStackFrameNames
  , getVariableNamesFromStackFrame
  , renderNoLineLengthLimit
    -- * Generators
  , genStepGranularity
    -- * Helpers for breakpoints
  , goToNextBreakpoint
  , goToNextBreakpointLine'
  , goesAfter
  , goesBetween
  , isAtLine
    -- * Snapshot unilities
  , ContractRunData (..)
  , TestCtx (..)
  , mkSnapshotsFor
  , withSnapshots
  , testWithSnapshotsImpl
  , testWithSnapshots
  , checkSnapshot
  , unexpectedSnapshot
  , extractConvertInfos
    -- * Lower-lever interface
  , mkSnapshotsForImpl
  , dummyLoggingFunction
  -- * LIGO types construct helpers
  , mkTypeExpression
  , mkConstantType
  , mkArrowType
  , (~>)
  , mkRecordType
  , mkSumType
    -- * Common snippets
  , mkSimpleConstantType
  , mkPairType
  , mkOptionType
  , intType'
  , unitType'
  , boolType'
  , intType
  , unitType
  , boolType
  , twoElemTreeLayout
  , combLayout
  ) where

import Control.Lens (each)
import Data.Default (def)
import Data.HashMap.Strict qualified as HM
import Data.Singletons (demote)
import Data.Singletons.Decide (decideEquality)
import Fmt (Buildable (..), blockListF', pretty)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((<.>), (</>))
import Test.HUnit (Assertion)
import Test.HUnit.Lang qualified as HUnit
import Test.Tasty.HUnit qualified as HUnit
import Text.Interpolation.Nyan hiding (rmode')
import Text.Interpolation.Nyan.Core (RMode (..))
import Text.Show qualified

import Morley.Debugger.Core.Common (SrcLoc (..))
import Morley.Debugger.Core.Navigate
  (BreakpointId, DebuggerState (..), Direction, FrozenPredicate (FrozenPredicate), HistoryReplay,
  HistoryReplayM, MovementResult (..), NavigableSnapshot (getExecutedPosition), SourceLocation,
  SourceLocation' (..), curSnapshot, evalWriterT, frozen, moveTill)
import Morley.Michelson.Interpret (RemainingSteps)
import Morley.Michelson.Runtime (ContractState (..))
import Morley.Michelson.Runtime.Dummy (dummyMaxSteps)
import Morley.Michelson.Typed (SingI (sing))
import Morley.Michelson.Typed qualified as T
import Morley.Tezos.Core (tz)
import Morley.Util.Typeable

import Duplo hiding (int, (<.>))

import Language.LIGO.AST.Skeleton qualified as AST
import Language.LIGO.Debugger.CLI
import Language.LIGO.Debugger.Common
import Language.LIGO.Debugger.Handlers.Helpers
import Language.LIGO.Debugger.Handlers.Impl
import Language.LIGO.Debugger.Michelson
import Language.LIGO.Debugger.Navigate
import Language.LIGO.Debugger.Snapshots
import "ligo-debugger" Util

contractsDir :: FilePath
contractsDir = "test" </> "contracts"

getCompilerContractsDir :: IO FilePath
getCompilerContractsDir = lookupEnv "LIGO_TEST_CONTRACTS_DIR" >>= \case
  Nothing -> pure $ ".." </> ".." </> ".." </> "src" </> "test" </> "contracts"
  Just dir -> pure dir

renderNoLineLengthLimit :: Doc -> Text
renderNoLineLengthLimit = toText . renderStyle style{lineLength = maxBound}

newtype ShowThroughBuild a = STB
  { unSTB :: a
  } deriving newtype (Eq, Ord)

instance Buildable (TestBuildable a) => Show (ShowThroughBuild a) where
  show = pretty . TB . unSTB

newtype TestBuildable a = TB
  { unTB :: a
  } deriving newtype (Eq, Ord)

-- | Provide @tb@ rendering mode for nyan-interpolators.
rmode'tb :: Buildable (TestBuildable a) => RMode a
rmode'tb = RMode (pretty . TB)

instance {-# OVERLAPPABLE #-} (ForInternalUse => Buildable a) => Buildable (TestBuildable a) where
  build = itIsForInternalUse $ build . unTB

instance Buildable (TestBuildable a) => Buildable (TestBuildable [a]) where
  build (TB l) = pretty $ blockListF' "-" (build . TB) l

instance (Buildable (TestBuildable e), Buildable (TestBuildable a)) =>
         Buildable (TestBuildable (Either e a)) where
  build (TB res) = case res of
    Right a -> build (TB a)
    Left e -> "Failure: " <> build (TB e)

instance (Buildable (TestBuildable a), Buildable (TestBuildable b)) =>
         Buildable (TestBuildable (a, b)) where
  build (TB (a, b)) =
    [int||
      ( #{TB a}
      , #{TB b}
      )
     |]

instance Buildable (TestBuildable LigoValue) where
  build (TB ligoValue) = buildLigoValue (LigoType Nothing) ligoValue

(@?=)
  :: (Eq a, Buildable (TestBuildable a), MonadIO m, HasCallStack)
  => a -> a -> m ()
(@?=) a b = liftIO $ STB a HUnit.@?= STB b
infix 1 @?=

-- | Similar to '@?=' but checks monadic value.
(@@?=)
  :: (Eq a, Buildable (TestBuildable a), MonadIO m, HasCallStack)
  => m a -> a -> m ()
(@@?=) am b = am >>= \a -> a @?= b
infix 1 @@?=

-- | Check that value matches certain predicate.
(@?)
  :: (Buildable (TestBuildable a), MonadIO m, HasCallStack)
  => a -> (a -> Bool) -> m ()
(@?) a p
  | p a = pass
  | otherwise = liftIO $ HUnit.assertFailure [int||Unexpected value: #tb{a}|]
infix 1 @?

-- | Similar to '@?' but checks monadic value.
(@@?)
  :: (Buildable (TestBuildable a), MonadIO m, HasCallStack)
  => m a -> (a -> Bool) -> m ()
(@@?) am p = am >>= \a -> a @? p
infix 1 @@?

-- | Check that the first list is permutation of the second one.
(@~=?)
  :: (Ord a, Buildable (TestBuildable a), MonadIO m, HasCallStack)
  => [a] -> [a] -> m ()
(@~=?) xs ys = liftIO $
  HUnit.assertBool
    [int||Expected #tb{xs} to be a permutation of #tb{ys}|]
    (xs `isPermutationOf` ys)

compareWithCurLocation
  :: (MonadState (DebuggerState (InterpretSnapshot u)) m)
  => SourceLocation -> FrozenPredicate (DebuggerState (InterpretSnapshot u)) m ()
compareWithCurLocation oldSrcLoc = FrozenPredicate do
  Just pos <- getExecutedPosition
  guard (pos /= oldSrcLoc)

compareWithCurLine
  :: (MonadState (DebuggerState (InterpretSnapshot u)) m)
  => SourceLocation -> FrozenPredicate (DebuggerState (InterpretSnapshot u)) m ()
compareWithCurLine oldSrcLoc = FrozenPredicate do
  Just pos <- getExecutedPosition
  guard . not $ and
    [ _slPath pos == _slPath oldSrcLoc
    , slLine (_slStart pos) == slLine (_slStart oldSrcLoc)
    ]

goesAfter
  :: (MonadState (DebuggerState is) m, NavigableSnapshot is)
  => SrcLoc -> FrozenPredicate (DebuggerState is) m ()
goesAfter loc = FrozenPredicate do
  Just (SourceLocation _ startPos _) <- getExecutedPosition
  guard (startPos >= loc)

goesBefore
  :: (MonadState (DebuggerState is) m, NavigableSnapshot is)
  => SrcLoc -> FrozenPredicate (DebuggerState is) m ()
goesBefore loc = FrozenPredicate do
  Just (SourceLocation _ _ endPos) <- getExecutedPosition
  guard (endPos <= loc)

goesBetween
  :: (MonadState (DebuggerState is) m, NavigableSnapshot is)
  => SrcLoc -> SrcLoc -> FrozenPredicate (DebuggerState is) m ()
goesBetween left right = goesAfter left && goesBefore right

isAtLine
  :: (MonadState (DebuggerState is) m, NavigableSnapshot is)
  => Word -> FrozenPredicate (DebuggerState is) m ()
isAtLine line =
  goesBetween
    (SrcLoc line 0)
    (SrcLoc (line + 1) 0)

goToNextBreakpoint'
  :: (HistoryReplay (InterpretSnapshot u) m, HasCallStack)
  => Direction -> m BreakpointId
goToNextBreakpoint' dir = do
  oldSrcLocMb <- frozen getExecutedPosition
  outcome <- moveTill dir do
    maybe true compareWithCurLocation oldSrcLocMb
    isAtBreakpoint
  case outcome of
    MovedSuccessfully reason -> pure reason
    _ -> error "Unexpectedly end of execution"

goToNextBreakpoint
  :: (HistoryReplay (InterpretSnapshot u) m, HasCallStack)
  => Direction -> m ()
goToNextBreakpoint dir = void (goToNextBreakpoint' dir)

goToNextBreakpointLine'
  :: (HistoryReplay (InterpretSnapshot u) m, HasCallStack)
  => Direction -> m BreakpointId
goToNextBreakpointLine' dir = do
  oldSrcLocMb <- frozen getExecutedPosition
  outcome <- moveTill dir do
    maybe true compareWithCurLine oldSrcLocMb
    isAtBreakpoint
  case outcome of
    MovedSuccessfully reason -> pure reason
    _ -> error "Unexpectedly end of execution"

-- Yet unused, but will likely be helpful, and leaving it is more consistent
-- with 'goToNextBreakpoint'
_goToNextBreakpointLine
  :: (HistoryReplay (InterpretSnapshot u) m, HasCallStack)
  => Direction -> m ()
_goToNextBreakpointLine dir = void (goToNextBreakpointLine' dir)

data ContractRunData =
  forall param st.
  ( T.IsoValue param, T.IsoValue st
  , SingI (T.ToT param), SingI (T.ToT st)
  )
  => ContractRunData
  { crdProgram :: FilePath
  , crdModuleName :: Maybe Text
  , crdParam :: param
  , crdStorage :: st
  }

data TestCtx = TestCtx
  { tcLigoTypesVec :: LigoTypesVec
  , tcEntrypointType :: LigoType
  }

-- | Doesn't log anything.
dummyLoggingFunction :: (Monad m) => Text -> m ()
dummyLoggingFunction = const $ pure ()

mkSnapshotsForImpl
  :: HasCallStack
  => (Text -> IO ())
  -> Maybe RemainingSteps
  -> ContractRunData
  -> IO (Set SourceLocation, InterpretHistory (InterpretSnapshot 'Unique), LigoType, LigoTypesVec)
mkSnapshotsForImpl logger maxStepsMb (ContractRunData file mModuleName (param :: param) (st :: st)) = do
  let moduleName = mModuleName ?: "$main"
  ligoMapper <- compileLigoContractDebug (mkModuleName $ toText moduleName) file
  LigoMapperResult
    exprLocs
    (T.SomeContract (contract@T.Contract{} :: T.Contract cp' st'))
    allFiles
    lambdaLocs
    entrypointType
    ligoTypesVec
    argumentRanges <-
      case readLigoMapper ligoMapper of
        Right v -> pure v
        Left err -> HUnit.assertFailure $ pretty err

  Refl <- sing @cp' `decideEquality` sing @(T.ToT param)
    & maybe
      do HUnit.assertFailure
          [int||
            Parameter type mismatch.
            Expected: #{demote @cp'}
            Got: #{demote @(T.ToT param)}
          |]
      do pure

  Refl <- sing @st' `decideEquality` sing @(T.ToT st)
    & maybe
      do HUnit.assertFailure
          [int||
            Storage type mismatch.
            Expected: #{demote @st'}
            Got: #{demote @(T.ToT st)}
          |]
      do pure

  parsedContracts <- parseContracts allFiles

  let statementLocs = getStatementLocs (getAllSourceLocations exprLocs) parsedContracts
  let allLocs = getInterestingSourceLocations parsedContracts exprLocs <> statementLocs

  let contractState = ContractState
        { csBalance = [tz|0u|]
        , csContract = contract
        , csStorage = T.toVal st
        , csDelegate = Nothing
        }

  scopes <- fmap HM.fromList $
    forM allFiles \fileName -> do
      scope <- fromLigoDefinitions <$> getScopes fileName
      pure (fileName, scope)

  contractEnv <- initContractEnv contractState def (maxStepsMb ?: dummyMaxSteps)

  his <-
    collectInterpretSnapshots
      file
      contract
      T.unsafeEpcCallRoot
      (T.toVal param)
      (T.toVal st)
      contractEnv
      parsedContracts
      logger
      lambdaLocs
      (isJust maxStepsMb)
      ligoTypesVec
      scopes
      argumentRanges

  return (allLocs, his, entrypointType, ligoTypesVec)

-- | Make snapshots history for simple contract.
mkSnapshotsFor
  :: HasCallStack
  => ContractRunData -> IO (Set SourceLocation, InterpretHistory (InterpretSnapshot 'Unique), LigoType, LigoTypesVec)
mkSnapshotsFor = mkSnapshotsForImpl dummyLoggingFunction Nothing

-- | Same as @mkSnapshotsFor@ but prints
-- snapshots collection logs into the console.
{-# WARNING _mkSnapshotsForLogging "'mkSnapshotsForLogging' remains in code" #-}
_mkSnapshotsForLogging
  :: HasCallStack
  => ContractRunData -> IO (Set SourceLocation, InterpretHistory (InterpretSnapshot 'Unique), LigoType, LigoTypesVec)
_mkSnapshotsForLogging = mkSnapshotsForImpl putStrLn Nothing

withSnapshots
  :: (Monad m)
  => (Set SourceLocation, InterpretHistory (InterpretSnapshot u), LigoType, LigoTypesVec)
  -> ReaderT TestCtx (HistoryReplayM (InterpretSnapshot u) m) a
  -> m a
withSnapshots (allLocs, his, ep, ligoTypesVec) action =
  evalWriterT $ evalStateT (runReaderT action $ TestCtx ligoTypesVec ep) (initDebuggerState his allLocs)

testWithSnapshotsImpl
  :: (Text -> IO ())
  -> Maybe RemainingSteps
  -> ContractRunData
  -> ReaderT TestCtx (HistoryReplayM (InterpretSnapshot 'Unique) IO) ()
  -> Assertion
testWithSnapshotsImpl logger maxStepsMb runData action = do
  locsHisEpAndLigoTypes <- mkSnapshotsForImpl logger maxStepsMb runData
  withSnapshots locsHisEpAndLigoTypes action

testWithSnapshots
  :: ContractRunData
  -> (ReaderT TestCtx (HistoryReplayM (InterpretSnapshot 'Unique) IO) ())
  -> Assertion
testWithSnapshots = testWithSnapshotsImpl dummyLoggingFunction Nothing

{-# WARNING _testWithSnapshotsLogging "'testWithSnapshotsLogging' remains in code" #-}
_testWithSnapshotsLogging
  :: ContractRunData
  -> ReaderT TestCtx (HistoryReplayM (InterpretSnapshot 'Unique) IO) ()
  -> Assertion
_testWithSnapshotsLogging = testWithSnapshotsImpl putStrLn Nothing

checkSnapshot
  :: (MonadState (DebuggerState (InterpretSnapshot 'Unique)) m, MonadReader TestCtx m, MonadIO m)
  => (InterpretSnapshot 'Concise -> Assertion)
  -> m ()
checkSnapshot check =
  asks tcLigoTypesVec >>= \vec -> frozen curSnapshot >>= liftIO . check . makeConciseSnapshots vec

unexpectedSnapshot
  :: HasCallStack => (SingI u) => InterpretSnapshot u -> Assertion
unexpectedSnapshot sp =
  HUnit.assertFailure $ "Unexpected snapshot:\n" <> pretty sp

fromValCasting :: forall a t. (T.IsoValue a, SingI t) => T.Value t -> Maybe a
fromValCasting v = do
  Refl <- sing @(T.ToT a) `decideEquality` sing @t
  return $ T.fromVal v

pattern SomeLorentzValue :: T.IsoValue v => v -> T.SomeValue
pattern SomeLorentzValue v <- T.SomeValue (fromValCasting -> Just v)
  where SomeLorentzValue v =  T.SomeValue (T.toVal v)

isPermutationOf :: (Ord a) => [a] -> [a] -> Bool
isPermutationOf xs ys = sort xs == sort ys

getStackFrameNames :: InterpretSnapshot u -> [Text]
getStackFrameNames snap =
  snap ^.. isStackFramesL . each . sfNameL

-- These functions are needed to strip hashes from variables
-- E.g. varName#123 -> varName

getVariableNamesFromStackFrame :: (SingI u) => StackFrame u -> [Text]
getVariableNamesFromStackFrame stackFrame = maybe unknownVariable pretty <$> variablesMb
  where
    variablesMb = stackFrame ^.. sfStackL . each . siLigoDescL . _LigoStackEntry . leseDeclarationL

genStepGranularity :: Gen LigoStepGranularity
genStepGranularity = Gen.frequency do
  gran <- allLigoStepGranularities
  weight <- pure case gran of
    GExpExt -> 5
    GExp -> 2
    GStmt -> 3
  return (weight, pure gran)

mkOptionType :: LigoTypeExpression -> LigoTypeExpression
mkOptionType typ = mkSumType (twoElemTreeLayout "None" "Some")
  [ ("Some", typ)
  , ("None", unitType')
  ]

intType' :: LigoTypeExpression
intType' = mkSimpleConstantType "Int"

unitType' :: LigoTypeExpression
unitType' = mkSimpleConstantType "Unit"

boolType' :: LigoTypeExpression
boolType' = mkSumType (twoElemTreeLayout "True" "False")
  [ ("True", unitType')
  , ("False", unitType')
  ]

unitType :: LigoType
unitType = LigoTypeResolved unitType'

intType :: LigoType
intType = LigoTypeResolved intType'

boolType :: LigoType
boolType = LigoTypeResolved boolType'

extractConvertInfos :: LigoTypesVec -> InterpretSnapshot 'Unique -> [PreLigoConvertInfo]
extractConvertInfos ligoTypesVec snap =
  let stackItems = sfStack $ head $ isStackFrames snap in
  catMaybes $ flip map stackItems \stackItem -> do
    StackItem desc michVal <- pure stackItem
    LigoStackEntry (LigoExposedStackEntry _ typRef _) <- pure desc
    let typ = readLigoType ligoTypesVec typRef
    pure $ PreLigoConvertInfo michVal typ

twoElemTreeLayout :: Text -> Text -> LigoLayout
twoElemTreeLayout a b = LLInner
  [ LLField smaller
  , LLField larger
  ]
  where
    (smaller, larger)
      | a <= b = (a, b)
      | otherwise = (b, a)

combLayout :: [Text] -> LigoLayout
combLayout = LLInner . fmap LLField

-- | LIGO now has @layout:comb for types by default.
-- This combinator allows to run tests in legacy mode
-- with @tree@ layout by default.
legacyMode :: IO a -> IO a
legacyMode act = bracket_
  (setEnv "LIGO_LEGACY_LAYOUT_TREE" "1")
  (unsetEnv "LIGO_LEGACY_LAYOUT_TREE")
  act
