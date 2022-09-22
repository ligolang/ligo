module Test.LigoCall
  ( module Test.LigoCall
  ) where

import Prelude hiding (try)

import Test.Tasty (TestTree, testGroup)
import Test.Util
import UnliftIO.Exception (try)

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
      take 15 (toList $ lmLocations res) @?= mconcat
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
      res @?= (U.ValuePair (U.ValueInt 5) (U.ValueString [mt|abc|]))

  , testCase "Relying on constants defined in the contract" do
      res <- evalExprOverContract1 "defEmptyStorage"
      res @?= (U.ValuePair (U.ValuePair (U.ValueInt 0) (U.ValueInt 0)) (U.ValueString [mt|!|]))

  , testCase "Relying on functions defined in the contract" do
      res <- try @_ @LigoException $ evalExprOverContract1 "defStorage \"a\""
      res @? isRight

  , testCase "Non-existing variable" do
      res <- try @_ @LigoException $ evalExprOverContract1 "absentStorage"
      res @? isLeft

  ]

test_EntrypointsCollection :: TestTree
test_EntrypointsCollection = testGroup "Getting entrypoints"
  [ testCase "Two entrypoints" do
      let file = contractsDir </> "two-entrypoints.mligo"

      EntrypointsList res <- getAvailableEntrypoints file
      res @~=? ["main1", "main2"]

  , testCase "Zero entrypoints" do
      let file = contractsDir </> "module_contracts" </> "imported.mligo"

      EntrypointsList res <- getAvailableEntrypoints file
      res @?= []
  ]
