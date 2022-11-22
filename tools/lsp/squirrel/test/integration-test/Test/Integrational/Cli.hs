module Test.Integrational.Cli
  ( test_ligo_159
  ) where

import System.FilePath (takeDirectory, (</>))
import UnliftIO.Exception (tryJust)

import Cli
import Log (runNoLoggingT)
import ParseTree (pathToSrc)

import Test.Common.FixedExpectations (expectationFailure)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

filterException :: SomeException -> Maybe SomeLigoException
filterException e = asum
  [ SomeLigoException <$> fromException @LigoDecodedExpectedClientFailureException e
  , SomeLigoException <$> fromException @LigoErrorNodeParseErrorException          e
  , SomeLigoException <$> fromException @LigoUnexpectedCrashException              e
  ]

checkFile :: HasCallStack => FilePath -> TestTree
checkFile path = testCase path do
  src <- runNoLoggingT $ pathToSrc path
  let temp = TempSettings (takeDirectory path) $ GenerateDir ".temp"
  tryJust filterException (runNoLoggingT $ getLigoDefinitions temp src) >>= \case
    Left  _ -> pass
    Right _ -> expectationFailure "Expected contract to fail, but it has succeeded."

test_ligo_159 :: TestTree
test_ligo_159 = testGroup "Contracts should throw errors" $ checkFile <$> files
  where
    files :: [FilePath]
    files = (\f -> "test" </> "contracts" </> "json-bugs" </> f) <$>
      [ -- TODO: "LIGO-159_2.mligo"
        "LIGO-159_3.mligo"
      ]
