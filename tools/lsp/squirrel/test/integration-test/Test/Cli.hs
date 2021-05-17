{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cli
  ( test_ligo_159
  ) where

import Control.Exception.Safe (try)
import System.FilePath ((</>))

import Cli
import ParseTree (Source (..))

import Test.FixedExpectations (HasCallStack, expectationFailure, shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

checkFile :: HasCallStack => Maybe LigoBinaryCallError -> FilePath -> TestTree
checkFile expectedError path = testCase path $
  try @_ @LigoBinaryCallError (getLigoDefinitions $ Path path) >>= \case
    Left err -> case expectedError of
      Nothing -> pure ()
      Just expected -> expected `shouldBe` err
    Right _ -> expectationFailure "Expected contract to fail, but it has succeeded."

test_ligo_159 :: TestTree
test_ligo_159 = testGroup "Contracts should throw errors" $
  checkFile Nothing <$> files
  where
    files :: [FilePath]
    files = (\f -> "test" </> "contracts" </> "json-bugs" </> f) <$>
      [ "LIGO-159_1.mligo"
      , "LIGO-159_2.mligo"
      , "LIGO-159_3.mligo"
      ]
