{-# LANGUAGE UndecidableInstances #-}

module Test.Util
  ( -- * Shared helpers
    (</>)
  , contractsDir
  , hasLigoExtension

    -- * Test utilities
  , ShowThroughBuild (..)
  , TestBuildable (..)
  , (@?=)
  , (@@?=)
  , (@?)
  , (@@?)
  , assertLeft
  , HUnit.testCase
  , HUnit.testCaseSteps
  , HUnit.assertFailure
  , HUnit.assertBool
    -- * Common snippets
  , intType
  ) where

import Fmt (Buildable (..), blockListF', pretty)
import Language.LIGO.Debugger.CLI.Types (LigoType (..), LigoTypeConstant (..))
import System.FilePath (takeExtension, (</>))
import Test.Tasty.HUnit qualified as HUnit
import Text.Interpolation.Nyan
import Text.Show qualified

contractsDir :: FilePath
contractsDir = "test" </> "contracts"

hasLigoExtension :: FilePath -> Bool
hasLigoExtension file =
  takeExtension file `elem`
    [ ".ligo"
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

instance {-# OVERLAPPABLE #-} Buildable a => Buildable (TestBuildable a) where
  build = build . unTB

instance Buildable (TestBuildable a) => Buildable (TestBuildable [a]) where
  build (TB l) = pretty $ blockListF' "-" (build . TB) l

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
  | otherwise = liftIO $ HUnit.assertFailure [int||Unexpected value: #{TB a}|]
infix 1 @?

-- | Similar to '@?' but checks monadic value.
(@@?)
  :: (Buildable (TestBuildable a), MonadIO m, HasCallStack)
  => m a -> (a -> Bool) -> m ()
(@@?) am p = am >>= \a -> a @? p
infix 1 @@?

assertLeft
  :: (Buildable (TestBuildable e), MonadIO m)
  => Either e a -> m ()
assertLeft = \case
  Left e -> liftIO $ HUnit.assertFailure (pretty $ TB e)
  Right _ -> pass

intType :: LigoType
intType = LTConstant $
  LigoTypeConstant
    { ltcParameters = []
    , ltcInjection = "int"
    }
