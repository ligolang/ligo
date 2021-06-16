module Test.Common.FixedExpectations
  ( Expectation
  , HasCallStack
  , expectationFailure
  , shouldBe
  , shouldContain
  , shouldMatchList
  ) where

import Control.Exception.Safe (catch, impureThrow)
import Test.Hspec.Expectations (Expectation, HasCallStack)
import Test.Hspec.Expectations qualified as H
  (expectationFailure, shouldBe, shouldContain, shouldMatchList)
import Test.HUnit.Lang qualified as H (HUnitFailure (..), formatFailureReason)
import Test.Tasty.HUnit (HUnitFailure (..))

expectationFailure :: HasCallStack => String -> Expectation
expectationFailure s = tastify (H.expectationFailure s)

infix 1 `shouldBe`, `shouldContain`, `shouldMatchList`

shouldBe :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation
shouldBe a b = tastify (H.shouldBe a b)

shouldContain :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> Expectation
shouldContain a b = tastify (H.shouldContain a b)

shouldMatchList :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> Expectation
shouldMatchList a b = tastify (H.shouldMatchList a b)

toTastyFailure :: H.HUnitFailure -> HUnitFailure
toTastyFailure (H.HUnitFailure loc s)
  = HUnitFailure loc (H.formatFailureReason s)

tastify :: Expectation -> Expectation
tastify action = action `catch` (impureThrow . toTastyFailure)
