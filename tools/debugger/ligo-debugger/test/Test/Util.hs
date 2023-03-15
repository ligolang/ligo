{-# LANGUAGE UndecidableInstances #-}

module Test.Util
  ( -- * Shared helpers
    (</>)
  , (<.>)
  , contractsDir
  , hasLigoExtension
  , LSP.Lang (..)
  , LSP.allLangs
  , LSP.langExtension
  , pattern SomeLorentzValue

    -- * Test utilities
  , ShowThroughBuild (..)
  , TestBuildable (..)
  , rmode'tb
  , (@?=)
  , (@@?=)
  , (@?)
  , (@@?)
  , (@?==)
  , (@~=?)
  , HUnit.testCase
  , HUnit.testCaseSteps
  , HUnit.assertFailure
  , HUnit.assertBool
  , getStackFrameNames
  , getVariableNamesFromStackFrame
    -- * Generators
  , genStepGranularity
    -- * Helpers for breakpoints
  , goToNextBreakpoint
  , goToPreviousBreakpoint
  , goesAfter
  , goesBefore
  , goesBetween
  , isAtLine
    -- * Snapshot unilities
  , ContractRunData (..)
  , mkSnapshotsFor
  , mkSnapshotsForLogging
  , withSnapshots
  , testWithSnapshotsImpl
  , testWithSnapshots
  , testWithSnapshotsLogging
  , checkSnapshot
  , unexpectedSnapshot
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
  , intType
  , unitType
  ) where

import Control.Lens (each)
import Data.Aeson (Value (Null))
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty (singleton)
import Data.Singletons (demote)
import Data.Singletons.Decide (decideEquality)
import Fmt (Buildable (..), blockListF', pretty)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import System.FilePath (takeExtension, (<.>), (</>))
import Test.HUnit (Assertion)
import Test.HUnit.Lang qualified as HUnit
import Test.Tasty.HUnit qualified as HUnit
import Text.Interpolation.Nyan
import Text.Interpolation.Nyan.Core (RMode (..))
import Text.Show qualified

import Morley.Debugger.Core.Breakpoint
  (BreakpointSelector (NextBreak), continueUntilBreakpoint, reverseContinue)
import Morley.Debugger.Core.Common (SrcLoc (..))
import Morley.Debugger.Core.Navigate
  (DebuggerState (..), Direction (Backward, Forward), FrozenPredicate (FrozenPredicate),
  HistoryReplay, HistoryReplayM, NavigableSnapshot (getExecutedPosition), SourceLocation,
  SourceLocation' (SourceLocation), curSnapshot, evalWriterT, frozen, moveTill)
import Morley.Michelson.Runtime.Dummy (dummyContractEnv)
import Morley.Michelson.Typed (SingI (sing))
import Morley.Michelson.Typed qualified as T
import Morley.Util.Typeable

import AST.Skeleton qualified as LSP
import Cli.Json
  (LigoRange (LRVirtual), LigoTableField (..), LigoTypeArrow (..), LigoTypeConstant (..),
  LigoTypeContent (LTCArrow, LTCConstant, LTCRecord, LTCSum), LigoTypeExpression (..),
  LigoTypeTable (LigoTypeTable), _ltcInjection, _ltcParameters)

import Language.LIGO.Debugger.CLI.Call
import Language.LIGO.Debugger.CLI.Types
import Language.LIGO.Debugger.Common
import Language.LIGO.Debugger.Handlers.Helpers
import Language.LIGO.Debugger.Handlers.Impl
import Language.LIGO.Debugger.Michelson
import Language.LIGO.Debugger.Navigate
import Language.LIGO.Debugger.Snapshots

contractsDir :: FilePath
contractsDir = "test" </> "contracts"

hasLigoExtension :: FilePath -> Bool
hasLigoExtension file =
  takeExtension file `elem`
    [ ".mligo"
    , ".jsligo"
    ]

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
rmode'tb = RMode (build . TB)

instance {-# OVERLAPPABLE #-} Buildable a => Buildable (TestBuildable a) where
  build = build . unTB

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

(@?==)
  :: (MonadIO m, MonadReader r m, Eq a, Buildable (TestBuildable a), HasCallStack)
  => Lens' r a -> a -> m ()
len @?== expected = do
  actual <- view len
  actual @?= expected
infixl 0 @?==

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
  => SourceLocation -> FrozenPredicate (DebuggerState (InterpretSnapshot u)) m
compareWithCurLocation oldSrcLoc = FrozenPredicate $
  getExecutedPosition >>= maybe (pure False) (pure . (/= oldSrcLoc))

goesAfter
  :: (MonadState (DebuggerState is) m, NavigableSnapshot is)
  => SrcLoc -> FrozenPredicate (DebuggerState is) m
goesAfter loc = FrozenPredicate $ fromMaybe False <$>
  ((\(SourceLocation _ startPos _) -> startPos >= loc) <<$>> getExecutedPosition)

goesBefore
  :: (MonadState (DebuggerState is) m, NavigableSnapshot is)
  => SrcLoc -> FrozenPredicate (DebuggerState is) m
goesBefore loc = FrozenPredicate $ fromMaybe False <$>
  ((\(SourceLocation _ _ endPos) -> endPos <= loc) <<$>> getExecutedPosition)

goesBetween
  :: (MonadState (DebuggerState is) m, NavigableSnapshot is)
  => SrcLoc -> SrcLoc -> FrozenPredicate (DebuggerState is) m
goesBetween left right = goesAfter left && goesBefore right

isAtLine
  :: (MonadState (DebuggerState is) m, NavigableSnapshot is)
  => Word -> FrozenPredicate (DebuggerState is) m
isAtLine line =
  goesBetween
    (SrcLoc line 0)
    (SrcLoc (line + 1) 0)

goToNextBreakpoint :: (HistoryReplay (InterpretSnapshot u) m) => m ()
goToNextBreakpoint = do
  oldSrcLocMb <- frozen getExecutedPosition
  void $ case oldSrcLocMb of
    Just oldSrcLoc -> moveTill Forward (isAtBreakpoint && compareWithCurLocation oldSrcLoc)
    Nothing -> continueUntilBreakpoint NextBreak

goToPreviousBreakpoint :: (HistoryReplay (InterpretSnapshot u) m) => m ()
goToPreviousBreakpoint = do
  oldSrcLocMb <- frozen getExecutedPosition
  void $ case oldSrcLocMb of
    Just oldSrcLoc -> moveTill Backward (isAtBreakpoint && compareWithCurLocation oldSrcLoc)
    Nothing -> reverseContinue NextBreak

data ContractRunData =
  forall param st.
  ( T.IsoValue param, T.IsoValue st
  , SingI (T.ToT param), SingI (T.ToT st)
  )
  => ContractRunData
  { crdProgram :: FilePath
  , crdEntrypoint :: Maybe String
  , crdParam :: param
  , crdStorage :: st
  }

-- | Doesn't log anything.
dummyLoggingFunction :: (Monad m) => String -> m ()
dummyLoggingFunction = const $ pure ()

mkSnapshotsForImpl
  :: HasCallStack
  => (String -> IO ()) -> ContractRunData -> IO (Set SourceLocation, InterpretHistory (InterpretSnapshot 'Unique))
mkSnapshotsForImpl logger (ContractRunData file mEntrypoint (param :: param) (st :: st)) = do
  let entrypoint = mEntrypoint ?: "main"
  ligoMapper <- compileLigoContractDebug entrypoint file
  (exprLocs, T.SomeContract (contract@T.Contract{} :: T.Contract cp' st'), allFiles, lambdaLocs) <-
    case readLigoMapper ligoMapper typesReplaceRules instrReplaceRules of
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

  his <-
    collectInterpretSnapshots
      file
      (fromString entrypoint)
      contract
      T.unsafeEpcCallRoot
      (T.toVal param)
      (T.toVal st)
      dummyContractEnv
      parsedContracts
      logger
      lambdaLocs

  return (allLocs, his)

-- | Make snapshots history for simple contract.
mkSnapshotsFor
  :: HasCallStack
  => ContractRunData -> IO (Set SourceLocation, InterpretHistory (InterpretSnapshot 'Unique))
mkSnapshotsFor = mkSnapshotsForImpl dummyLoggingFunction

-- | Same as @mkSnapshotsFor@ but prints
-- snapshots collection logs into the console.
{-# WARNING mkSnapshotsForLogging "'mkSnapshotsForLogging' remains in code" #-}
mkSnapshotsForLogging
  :: HasCallStack
  => ContractRunData -> IO (Set SourceLocation, InterpretHistory (InterpretSnapshot 'Unique))
mkSnapshotsForLogging = mkSnapshotsForImpl putStrLn

withSnapshots
  :: (Monad m)
  => (Set SourceLocation, InterpretHistory (InterpretSnapshot u))
  -> HistoryReplayM (InterpretSnapshot u) m a
  -> m a
withSnapshots (allLocs, his) action =
  evalWriterT $ evalStateT action (initDebuggerState his allLocs)

testWithSnapshotsImpl
  :: (String -> IO ())
  -> ContractRunData
  -> HistoryReplayM (InterpretSnapshot 'Unique) IO ()
  -> Assertion
testWithSnapshotsImpl logger runData action = do
  locsAndHis <- mkSnapshotsForImpl logger runData
  withSnapshots locsAndHis action

testWithSnapshots
  :: ContractRunData
  -> HistoryReplayM (InterpretSnapshot 'Unique) IO ()
  -> Assertion
testWithSnapshots = testWithSnapshotsImpl dummyLoggingFunction

{-# WARNING testWithSnapshotsLogging "'testWithSnapshotsLogging' remains in code" #-}
testWithSnapshotsLogging
  :: ContractRunData
  -> HistoryReplayM (InterpretSnapshot 'Unique) IO ()
  -> Assertion
testWithSnapshotsLogging = testWithSnapshotsImpl putStrLn

checkSnapshot
  :: (MonadState (DebuggerState (InterpretSnapshot 'Unique)) m, MonadIO m)
  => (InterpretSnapshot 'Concise -> Assertion)
  -> m ()
checkSnapshot check = frozen curSnapshot >>= liftIO . check . stripSuffixHashFromSnapshots

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

mkTypeExpression :: LigoTypeContent -> LigoTypeExpression
mkTypeExpression content = LigoTypeExpression
  { _lteTypeContent = content
  , _lteLocation = LRVirtual "dummy"
  , _lteSugar = Nothing
  , _lteTypeMeta = Nothing
  , _lteOrigVar = Nothing
  }

mkConstantType :: Text -> [LigoTypeExpression] -> LigoTypeExpression
mkConstantType typeName parameters = mkTypeExpression $ LTCConstant $
  LigoTypeConstant
    { _ltcParameters = parameters
    , _ltcLanguage = "Michelson"
    , _ltcInjection = singleton typeName
    }

mkArrowType :: LigoTypeExpression -> LigoTypeExpression -> LigoTypeExpression
mkArrowType domain codomain = mkTypeExpression $ LTCArrow $
  LigoTypeArrow
    { _ltaType2 = codomain
    , _ltaType1 = domain
    }

(~>) :: LigoTypeExpression -> LigoTypeExpression -> LigoTypeExpression
(~>) = mkArrowType

infixr 2 ~>

mkTypeTable :: [(Text, LigoTypeExpression)] -> LigoTypeTable
mkTypeTable keyValues = map (second mkTableField) keyValues
  & HM.fromList
  & flip LigoTypeTable Null
  where
    mkTableField :: LigoTypeExpression -> LigoTableField
    mkTableField expr = LigoTableField
      { _ltfDeclPos = 0
      , _lrfMichelsonAnnotation = Null
      , _ltfAssociatedType = expr
      }

mkRecordType :: [(Text, LigoTypeExpression)] -> LigoTypeExpression
mkRecordType keyValues = mkTypeTable keyValues
  & LTCRecord
  & mkTypeExpression

mkSumType :: [(Text, LigoTypeExpression)] -> LigoTypeExpression
mkSumType keyValues = mkTypeTable keyValues
  & LTCSum
  & mkTypeExpression

mkSimpleConstantType :: Text -> LigoTypeExpression
mkSimpleConstantType typ = mkConstantType typ []

mkPairType :: LigoTypeExpression -> LigoTypeExpression -> LigoTypeExpression
mkPairType fstElem sndElem = mkRecordType
  [ ("0", fstElem)
  , ("1", sndElem)
  ]

mkOptionType :: LigoTypeExpression -> LigoTypeExpression
mkOptionType typ = mkSumType
  [ ("Some", typ)
  , ("None", unitType')
  ]

intType' :: LigoTypeExpression
intType' = mkSimpleConstantType "Int"

unitType' :: LigoTypeExpression
unitType' = mkSimpleConstantType "Unit"

intType :: LigoType
intType = LigoTypeResolved intType'

unitType :: LigoType
unitType = LigoTypeResolved unitType'
