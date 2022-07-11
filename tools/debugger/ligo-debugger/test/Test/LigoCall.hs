module Test.LigoCall
  ( module Test.LigoCall
  ) where

import Test.Tasty (TestTree, testGroup)

import Language.LIGO.Debugger.CLI.Call
import Language.LIGO.Debugger.CLI.Types
import Morley.Michelson.Parser (MichelsonSource (MSName))
import Morley.Michelson.Text (mt)
import Morley.Michelson.Untyped qualified as U
import Test.Util

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
      res @?= Right (U.ValuePair (U.ValueInt 5) (U.ValueString [mt|abc|]))

  , testCase "Relying on constants defined in the contract" do
      res <- evalExprOverContract1 "defEmptyStorage"
      res @?= Right (U.ValuePair (U.ValuePair (U.ValueInt 0) (U.ValueInt 0)) (U.ValueString [mt|!|]))

  , testCase "Relying on functions defined in the contract" do
      res <- evalExprOverContract1 "defStorage \"a\""
      res @? isRight

  , testCase "Non-existing variable" do
      res <- evalExprOverContract1 "absentStorage"
      res @? isLeft

  ]
