module Test.Common.FixedExpectations
  ( Expectation
  , HasCallStack
  , expectationFailure
  , H.anyException
  , shouldBe
  , shouldContain
  , shouldMatchList
  , shouldSatisfy
  , shouldThrow
  ) where

import Test.HUnit.Lang qualified as H (HUnitFailure (..), formatFailureReason)
import Test.Hspec.Expectations (Expectation)
import Test.Hspec.Expectations qualified as H
  (Selector, anyException, expectationFailure, shouldBe, shouldContain, shouldMatchList,
  shouldSatisfy, shouldThrow)
import Test.Tasty.HUnit (HUnitFailure (..))
import UnliftIO.Exception as UnliftIO (catch, impureThrow)

expectationFailure :: HasCallStack => String -> Expectation
expectationFailure s = tastify (H.expectationFailure s)

infix 1 `shouldBe`, `shouldContain`, `shouldMatchList`, `shouldSatisfy`

shouldBe :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation
shouldBe a b = tastify (H.shouldBe a b)

shouldContain :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> Expectation
shouldContain a b = tastify (H.shouldContain a b)

shouldMatchList :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> Expectation
shouldMatchList a b = tastify (H.shouldMatchList a b)

shouldSatisfy :: (HasCallStack, Show a) => a -> (a -> Bool) -> Expectation
shouldSatisfy a f = tastify (H.shouldSatisfy a f)

shouldThrow :: (HasCallStack, Exception e) => IO a -> H.Selector e -> Expectation
shouldThrow a s = tastify (H.shouldThrow a s)

toTastyFailure :: H.HUnitFailure -> HUnitFailure
toTastyFailure (H.HUnitFailure loc s)
  = HUnitFailure loc (H.formatFailureReason s)

tastify :: Expectation -> Expectation
tastify action = action `UnliftIO.catch` (impureThrow . toTastyFailure)
