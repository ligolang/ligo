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
  , assertLeft
  , HUnit.testCase
  , HUnit.testCaseSteps
  , HUnit.assertFailure
  , HUnit.assertBool
  ) where

import Fmt (Buildable (..), blockListF', pretty)
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

assertLeft
  :: (Buildable (TestBuildable e), MonadIO m)
  => Either e a -> m ()
assertLeft = \case
  Left e -> liftIO $ HUnit.assertFailure (pretty $ TB e)
  Right _ -> pass
