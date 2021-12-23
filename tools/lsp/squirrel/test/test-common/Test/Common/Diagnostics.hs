module Test.Common.Diagnostics
  ( inputFile
  , parseDiagnosticsDriver
  ) where

import Data.Text (Text)
import Parser
import Progress (noProgress)
import Range
import System.FilePath ((</>))

import AST.Parser (collectAllErrors, parseContractsWithDependencies, parsePreprocessed)
import AST.Scope (addScopes, lookupContract)
import AST.Skeleton (Error (..))

import qualified Test.Common.Capabilities.Util as Util (contractsDir)
import Test.Common.FixedExpectations (HasCallStack, expectationFailure, shouldMatchList)
import Test.Common.Util (ScopeTester, withoutLogger)
import Test.Tasty.HUnit (Assertion)

inputDir :: FilePath
inputDir = Util.contractsDir </> "diagnostic"

inputFile :: FilePath
inputFile = inputDir </> "a.mligo"

simplifyError :: Msg -> (Range, Text)
simplifyError (range, Error t _) = (range, t)

-- Try to parse a file, and check that the proper error messages are generated
parseDiagnosticsDriver
  :: forall impl
   . (HasCallStack, ScopeTester impl)
  => [(Range, Text)]
  -> Assertion
parseDiagnosticsDriver expectedMsgs = withoutLogger \runLogger -> do
  parsedContracts <- runLogger $ parseContractsWithDependencies parsePreprocessed noProgress inputDir
  contractGraph <- runLogger $ addScopes @impl noProgress parsedContracts
  let mContract = lookupContract inputFile contractGraph
  case mContract of
    Nothing -> expectationFailure ("Couldn't find " <> inputFile)
    Just contract ->
      let msgs = collectAllErrors contract
       in expectedMsgs `shouldMatchList` fmap simplifyError msgs
