module Test.LigoCall
  ( module Test.LigoCall
  ) where

import Fmt (Buildable, pretty)
import System.FilePath ((</>))
import Test.HUnit ((@?=))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Text.Show qualified

import Language.LIGO.Debugger.CLI.Call
import Language.LIGO.Debugger.CLI.Types

newtype ShowThroughBuild a = STB
  { unSTB :: a
  } deriving newtype (Eq, Ord)

instance {-# OVERLAPPABLE #-} Buildable a => Show (ShowThroughBuild a) where
  show = pretty . unSTB


test_Call :: TestTree
test_Call = testGroup "Getting debug info"
  [ testCase "noop.ligo contract" do
      let file = ".." </> "src-mapper" </> "noop.mligo"
      let (a, b) <-> (c, d) = LigoRange file (LigoPosition a b) (LigoPosition c d)
      res <- compileLigoContractDebug file
      STB (take 15 (toList $ lmLocations res)) @?= STB (mconcat
        [ replicate 7 LigoEmptyLocationInfo

        , [ LigoMereEnvInfo [LigoHiddenStackEntry]
          , LigoEmptyLocationInfo
          ]

        , [ LigoMereEnvInfo [LigoStackEntryNoVar (LigoTypeRef 0)] ]
        , replicate 3 LigoEmptyLocationInfo
        , [ LigoMereLocInfo ((2, 11) <-> (2, 17)) ]
        , [ LigoMereEnvInfo [LigoStackEntryVar "s2" (LigoTypeRef 0)] ]

        ])

  ]
