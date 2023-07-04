module Test.LigoCall
  ( module Test.LigoCall
  ) where

import Prelude hiding (try)

import Control.Exception (throw)
import Data.HashMap.Strict qualified as HM
import Data.Map qualified as M
import Data.Text qualified as T
import Fmt (pretty)
import Test.Tasty (TestTree, testGroup)
import Test.Util
import Test.Util.Options (minor)
import UnliftIO.Exception (try)
import Unsafe qualified

import Morley.Michelson.Parser (MichelsonSource (MSName))
import Morley.Michelson.Text (MText, mt)
import Morley.Michelson.Typed qualified as T
import Morley.Michelson.Untyped qualified as U
import Morley.Tezos.Core
  (ChainId (UnsafeChainId), parseChainId, timestampFromSeconds, timestampQuote)

import Language.LIGO.Debugger.CLI
import Language.LIGO.Range

test_Compilation :: TestTree
test_Compilation = testGroup "Getting debug info"
  [ testCase "simple-ops.mligo contract" do
      let file = contractsDir </> "simple-ops.mligo"
      let (a, b) <-> (c, d) = Range (LigoPosition a b) (LigoPosition c d) file
      res <- compileLigoContractDebug "main" file

      let mainType = LigoTypeResolved $
            mkPairType
              unitType'
              intType'
            ~>
            mkPairType
              (mkConstantType "List" [mkSimpleConstantType "Operation"])
              intType'

      take 15 (makeConciseLigoIndexedInfo (lmTypes res) <$> toList (lmLocations res)) @?= mconcat
        [ replicate 7 LigoEmptyLocationInfo

        , [ LigoMereEnvInfo [LigoHiddenStackEntry] ]

        , [ LigoMereLocInfo ((1, 1) <-> (4, 30)) mainType ]
        , [ LigoMereLocInfo ((1, 1) <-> (4, 30)) mainType ]

        , replicate 5 LigoEmptyLocationInfo
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

  , testGroup "Expressions starting from `-`"
    -- At the moment of writing, `ligo` does not accept negative numbers easily
    -- See https://gitlab.com/ligolang/ligo/-/issues/1495
    [ testCase "Negative numbers" do
        res <- evalExprOverContract1 "-3"
        res @?= U.ValueInt (-3)

    , testCase "Simple expression" do
        res <- evalExprOverContract1 "-3 + 1"
        res @?= U.ValueInt (-2)

    ]
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

    minor $ testCase "ligolang#1461" do

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

test_Decompile_values :: TestTree
test_Decompile_values = testGroup "Decompilation of LIGO values"
  [ testCase "Decompile primitives" do
      let intValue = T.SomeValue $ T.toVal @Integer 42
      let boolValue = T.SomeValue $ T.toVal False
      let stringValue = T.SomeValue $ T.toVal [mt|ligo string|]

      let boolType = LigoTypeResolved $ mkSumType LTree
            [ ("True", unitType')
            , ("False", unitType')
            ]
      let stringType = LigoTypeResolved $ mkSimpleConstantType "String"

      let typesAndValues =
            [ (intType, intValue)
            , (boolType, boolValue)
            , (stringType, stringValue)
            ]

      decompiled <- decompileValues typesAndValues
      LVCt <$> [LCInt "42", LCBool False, LCString "ligo string"] @?= decompiled

  , testCase "Decompile constructor" do
      let someInt = T.SomeValue $ T.toVal $ Just (42 :: Integer)
      let optType = LigoTypeResolved $ mkOptionType intType'

      decompiled <- decompileValues [(optType, someInt)]
      [LVConstructor ("Some", LVCt $ LCInt "42")] @?= decompiled

  , testGroup "Decompile structures"
      [ testCase "Decompile record" do
          let recordVal = T.SomeValue $ T.toVal ((42 :: Integer, [mt|str|]), True)
          let recordType = LigoTypeResolved $ mkRecordType LTree
                [ ("a", intType')
                , ("b", mkSimpleConstantType "String")
                , ("c", mkSumType LTree [("True", unitType'), ("False", unitType')])
                ]

          decompiled <- decompileValues [(recordType, recordVal)]

          let expected = LVRecord $ HM.fromList
                [ ("a", LVCt $ LCInt "42")
                , ("b", LVCt $ LCString "str")
                , ("c", LVCt $ LCBool True)
                ]

          [expected] @?= decompiled

      , testCase "Decompile record with layout comb" do
          let recordVal = T.SomeValue $ T.toVal (42 :: Integer, ([mt|str|], True))
          let recordType = LigoTypeResolved $ mkRecordType LComb
                [ ("a", intType')
                , ("b", mkSimpleConstantType "String")
                , ("c", mkSumType LTree [("True", unitType'), ("False", unitType')])
                ]

          decompiled <- decompileValues [(recordType, recordVal)]

          let expected = LVRecord $ HM.fromList
                [ ("a", LVCt $ LCInt "42")
                , ("b", LVCt $ LCString "str")
                , ("c", LVCt $ LCBool True)
                ]

          [expected] @?= decompiled

      , testCase "Decompile list" do
          let intList = T.SomeValue $ T.toVal [1 :: Integer, 2, 3]
          let intListType = LigoTypeResolved $ mkConstantType "List" [intType']

          decompiled <- decompileValues [(intListType, intList)]
          [LVList (LVCt <$> [LCInt "1", LCInt "2", LCInt "3"])] @?= decompiled

      , testCase "Decompile map" do
          let intStringMap = T.SomeValue $ T.toVal $ M.fromList [(1 :: Integer, [mt|one|]), (2, [mt|two|])]
          let intStringMapType = LigoTypeResolved $ mkConstantType "Map" [intType', mkSimpleConstantType "String"]

          decompiled <- decompileValues [(intStringMapType, intStringMap)]

          let expected = LVMap $ bimap LVCt LVCt <$>
                [ (LCInt "1", LCString "one")
                , (LCInt "2", LCString "two")
                ]

          [expected] @?= decompiled
      ]

  , testGroup "Sum types"
      [ testCase "Decompile sum with default layout" do
          let sumVal = T.SomeValue $ T.toVal (Left @_ @() $ Right @Integer [mt|str|])
          let sumType = LigoTypeResolved $ mkSumType LTree
                [ ("A", intType')
                , ("B", mkSimpleConstantType "String")
                , ("C", unitType')
                ]

          decompiled <- decompileValues [(sumType, sumVal)]

          [LVConstructor ("B", LVCt $ LCString "str")] @?= decompiled

      , testCase "Decompile sum with layout comb" do
          let sumVal = T.SomeValue $ T.toVal (Right @Integer $ Left @_ @() [mt|str|])
          let sumType = LigoTypeResolved $ mkSumType LComb
                [ ("A", intType')
                , ("B", mkSimpleConstantType "String")
                , ("C", unitType')
                ]

          decompiled <- decompileValues [(sumType, sumVal)]

          [LVConstructor ("B", LVCt $ LCString "str")] @?= decompiled
      ]

  , testGroup "Timestamps"
      [ testCase "Timestamp from RFC3339" do
          let timeVal = T.SomeValue $ T.toVal [timestampQuote|1970-01-01T00:01:40Z|]
          let timeType = LigoTypeResolved $ mkSimpleConstantType "Timestamp"

          decompiled <- decompileValues [(timeType, timeVal)]

          [LVCt $ LCTimestamp "100"] @?= decompiled

      , testCase "Timestamp from seconds" do
          let timeVal = T.SomeValue $ T.toVal $ timestampFromSeconds 100
          let timeType = LigoTypeResolved $ mkSimpleConstantType "Timestamp"

          decompiled <- decompileValues [(timeType, timeVal)]

          [LVCt $ LCTimestamp "100"] @?= decompiled
      ]

  , testGroup "Chain id"
      [ testCase "Chain id from string" do
          let chainIdVal = T.SomeValue $ T.toVal $ either throw id $ parseChainId "NetXH12Aer3be93"
          let chainIdType = LigoTypeResolved $ mkSimpleConstantType "Chain_id"

          decompiled <- decompileValues [(chainIdType, chainIdVal)]

          [LVCt $ LCChainId "\0\0\0\0"] @?= decompiled

      , testCase "Chain id from bytes" do
          let chainIdVal = T.SomeValue $ T.toVal $ UnsafeChainId "\0\0\0\0"
          let chainIdType = LigoTypeResolved $ mkSimpleConstantType "Chain_id"

          decompiled <- decompileValues [(chainIdType, chainIdVal)]

          [LVCt $ LCChainId "\0\0\0\0"] @?= decompiled
      ]

  , testCase "Complex value" do
      let complexVal = T.SomeValue
            $ T.toVal
            $ Left @_ @()
                ( (UnsafeChainId "\0\0\0\0", [mt|large|])
                , Right @Integer (Left @_ @MText [timestampQuote|1970-01-01T00:01:40Z|])
                )

      let complexType = LigoTypeResolved $ mkSumType LTree
            [ ( "Go"
              , mkRecordType LTree
                  [ ("ch", mkSimpleConstantType "Chain_id")
                  , ( "state"
                    , mkSumType LComb
                        [ ("B", intType')
                        , ("A", mkSimpleConstantType "Timestamp")
                        , ("C", mkSimpleConstantType "String")
                        ]
                    )
                  , ("s", mkSimpleConstantType "String")
                  ]
              )

            , ("Nop", unitType')
            ]

      decompiled <- decompileValues [(complexType, complexVal)]

      let expected = LVConstructor
            ( "Go"
            , LVRecord $ HM.fromList
                [ ("ch", LVCt $ LCChainId "\0\0\0\0")
                , ("state", LVConstructor ("A", LVCt $ LCTimestamp "100"))
                , ("s", LVCt $ LCString "large")
                ]
            )

      [expected] @?= decompiled
  ]
  where
    decompileValues :: [(LigoType, T.SomeValue)] -> IO [LigoValue]
    decompileValues typesAndValues = do
      decompiledValues <- decompileLigoValues typesAndValues
      pure $ catMaybes decompiledValues
