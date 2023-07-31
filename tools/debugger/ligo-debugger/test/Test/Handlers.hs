-- | General tests on handlers.
module Test.Handlers
  ( test_exhaustiveness
  ) where

import Protocol.DAP.Serve.IO (StopAdapter (..))
import Protocol.DAP.TestUtils (ExhaustivenessError (..), areHandlersExhaustive)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

import Language.LIGO.Debugger.Handlers.Impl (LIGO, ligoHandlers)
import Morley.Debugger.DAP.Handlers (supportedCapabilities)

test_exhaustiveness :: TestTree
test_exhaustiveness =
  testCase "Handlers cover all the requests" $
    areHandlersExhaustive
      (supportedCapabilities @LIGO)
      (ligoHandlers (StopAdapter pass))
      -- TODO [#1984] should return Right ()
      @?= Left (ExhaustivenessError $ "attach" :| ["pause", "source"])
