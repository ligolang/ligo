{-# LANGUAGE UndecidableInstances #-}

module Test.Util
  ( -- * Shared helpers
    (</>)
  , contractsDir
  , hasLigoExtension

    -- * Test utilities
  , ShowThroughBuild (..)
  , TestBuildable (..)
  , rmode'tb
  , (@?=)
  , (@@?=)
  , (@?)
  , (@@?)
  , HUnit.testCase
  , HUnit.testCaseSteps
  , HUnit.assertFailure
  , HUnit.assertBool
    -- * Common snippets
  , intType
    -- * Helpers for breakpoints
  , goToNextBreakpoint
  , goToPreviousBreakpoint
  ) where

import Fmt (Buildable (..), blockListF', pretty)
import Language.LIGO.Debugger.CLI.Types (LigoType (..), LigoTypeConstant (..))
import Language.LIGO.Debugger.Snapshots (InterpretSnapshot)
import Morley.Debugger.Core.Breakpoint
  (BreakpointSelector (NextBreak), continueUntilBreakpoint, reverseContinue)
import Morley.Debugger.Core.Navigate
  (DebuggerState, Direction (Backward, Forward), FrozenPredicate (FrozenPredicate),
  NavigableSnapshot (getExecutedPosition), SourceLocation, frozen, isAtBreakpoint, moveTill)
import System.FilePath (takeExtension, (</>))
import Test.Tasty.HUnit qualified as HUnit
import Text.Interpolation.Nyan
import Text.Interpolation.Nyan.Core (RMode (..))
import Text.Show qualified

contractsDir :: FilePath
contractsDir = "test" </> "contracts"

hasLigoExtension :: FilePath -> Bool
hasLigoExtension file =
  takeExtension file `elem`
    [ ".ligo"
    , ".pligo"
    , ".mligo"
    , ".religo"
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

intType :: LigoType
intType = LTConstant $
  LigoTypeConstant
    { ltcParameters = []
    , ltcInjection = "Int" :| []
    }

compareWithCurLocation
  :: (MonadState (DebuggerState InterpretSnapshot) m)
  => SourceLocation -> FrozenPredicate (DebuggerState InterpretSnapshot) m
compareWithCurLocation oldSrcLoc = FrozenPredicate $
  getExecutedPosition >>= maybe (pure False) (pure . (/= oldSrcLoc))

goToNextBreakpoint :: (MonadState (DebuggerState InterpretSnapshot) m) => m ()
goToNextBreakpoint = do
  oldSrcLocMb <- frozen getExecutedPosition
  void $ case oldSrcLocMb of
    Just oldSrcLoc -> moveTill Forward (isAtBreakpoint && compareWithCurLocation oldSrcLoc)
    Nothing -> continueUntilBreakpoint NextBreak

goToPreviousBreakpoint :: (MonadState (DebuggerState InterpretSnapshot) m) => m ()
goToPreviousBreakpoint = do
  oldSrcLocMb <- frozen getExecutedPosition
  void $ case oldSrcLocMb of
    Just oldSrcLoc -> moveTill Backward (isAtBreakpoint && compareWithCurLocation oldSrcLoc)
    Nothing -> reverseContinue NextBreak
