module Test.LigoCall
  ( module Test.LigoCall
  ) where

import Prelude hiding (try)

import Data.Text qualified as T
import Fmt (pretty)
import Test.Tasty (TestTree, testGroup)
import Test.Util
import UnliftIO.Exception (try)
import Unsafe qualified

import Morley.Michelson.Parser (MichelsonSource (MSName))
import Morley.Michelson.Text (mt)
import Morley.Michelson.Untyped qualified as U

import Language.LIGO.Debugger.CLI.Call
import Language.LIGO.Debugger.CLI.Types

test_Compilation :: TestTree
test_Compilation = testGroup "Getting debug info"
  [ testCase "simple-ops.mligo contract" do
      let file = contractsDir </> "simple-ops.mligo"
      let (a, b) <-> (c, d) = LigoRange file (LigoPosition a b) (LigoPosition c d)
      res <- compileLigoContractDebug "main" file
      take 15 (stripSuffixHashFromLigoIndexedInfo <$> toList (lmLocations res)) @?= mconcat
        [ replicate 7 LigoEmptyLocationInfo

        , [ LigoMereEnvInfo [LigoHiddenStackEntry]          ]

        , [ LigoMereLocInfo ((1, 10) <-> (1, 14)) ]

        , replicate 6 LigoEmptyLocationInfo
        ]

  ]

test_ExpressionCompilation :: TestTree
test_ExpressionCompilation = testGroup "Compiling expression"
  let evalExprOverContract1 =
        compileLigoExpression (MSName "test") (contractsDir </> "complex-storage.mligo")
  in
  [ testCase "Evaluating pure values" do
      res <- evalExprOverContract1 "(5n, \"abc\")"
      res @?= U.ValuePair (U.ValueInt 5) (U.ValueString [mt|abc|])

  , testCase "Relying on constants defined in the contract" do
      res <- evalExprOverContract1 "defEmptyStorage"
      res @?= U.ValuePair (U.ValuePair (U.ValueInt 0) (U.ValueInt 0)) (U.ValueString [mt|!|])

  , testCase "Relying on functions defined in the contract" do
      res <- try @_ @LigoCallException $ evalExprOverContract1 "defStorage \"a\""
      res @? isRight

  , testCase "Non-existing variable" do
      res <- try @_ @LigoCallException $ evalExprOverContract1 "absentStorage"
      res @? isLeft

  ]

test_EntrypointsCollection :: TestTree
test_EntrypointsCollection = testGroup "Getting entrypoints"
  [ testCase "Two entrypoints" do
      let file = contractsDir </> "two-entrypoints.mligo"

      EntrypointsList res <- getAvailableEntrypoints file
      res @~=? ["main1", "main2"]

  , testCase "Zero entrypoints" do
      let file = contractsDir </> "no-entrypoint.mligo"

      EntrypointsList res <- getAvailableEntrypoints file
      res @?= []
  ]

test_Versions :: TestTree
test_Versions = testGroup "Ligo version management"
  [ testGroup "Our versions base sanity"
      [ testCase "Recommended version is treated as supported" $
          isSupportedVersion recommendedVersion @?= VersionSupported

      , testCase "Minimal supported version is actually treated as supported" $
          isSupportedVersion minimalSupportedVersion @?= VersionSupported
      ]
  ]

-- | Corner cases that once broke in LIGO and we have to extra check them.
test_Regressions :: TestTree
test_Regressions = testGroup "Regressions"
  [ -- Getting entrypoints when a contract imports another contract of
    -- a different dialect

    -- TODO: enable this test
    testCase "ligolang#1461" $ when False do

      let file = contractsDir </> "module_contracts" </> "imported.mligo"

      EntrypointsList _res <- getAvailableEntrypoints file
      pass
  ]

test_ANSISequencesReplace :: TestTree
test_ANSISequencesReplace = testGroup "ANSI replacements"
  [ testCase "Check replacement" do
      Left (exc :: LigoCallException) <- try (compileLigoContractDebug "main" (contractsDir </> "malformed.mligo"))

      let actual = pretty exc
            & T.splitOn "-->"
            & flip (Unsafe.!!) 1
            & T.splitOn "<--"
            & Unsafe.head

      "Hello" @?= actual
  ]
