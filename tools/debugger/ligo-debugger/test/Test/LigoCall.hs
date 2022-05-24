module Test.LigoCall
  ( module Test.LigoCall
  ) where

import Test.Tasty (TestTree, testGroup)

import Language.LIGO.Debugger.CLI.Call
import Language.LIGO.Debugger.CLI.Types
import Test.Util

test_Call :: TestTree
test_Call = testGroup "Getting debug info"
  [ testCase "simple-ops.mligo contract" do
      let file = inContractsDir "simple-ops.mligo"
      let (a, b) <-> (c, d) = LigoRange file (LigoPosition a b) (LigoPosition c d)
      res <- compileLigoContractDebug file
      take 15 (toList $ lmLocations res) @?= mconcat
        [ replicate 7 LigoEmptyLocationInfo

        , [ LigoMereEnvInfo [LigoHiddenStackEntry]
          , LigoEmptyLocationInfo
          ]

        , [ LigoMereEnvInfo [LigoStackEntryNoVar (LigoTypeRef 0)] ]
        , replicate 3 LigoEmptyLocationInfo
        , [ LigoMereLocInfo ((2, 11) <-> (2, 17)) ]
        , [ LigoMereEnvInfo [LigoStackEntryVar "s2" (LigoTypeRef 0)] ]

        ]

  ]
