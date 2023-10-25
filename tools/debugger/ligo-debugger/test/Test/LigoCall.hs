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
import Test.Util.Options (minor, reinsuring)
import Text.Interpolation.Nyan hiding (rmode')
import UnliftIO.Exception (try)
import Unsafe qualified

import Morley.Debugger.DAP.Types (MichelsonJson (..))
import Morley.Michelson.Parser (MichelsonSource (MSName))
import Morley.Michelson.Text (MText, mt)
import Morley.Michelson.Typed qualified as T
import Morley.Michelson.Untyped qualified as U
import Morley.Tezos.Address (Constrained (Constrained), ImplicitAddress, ta, unImplicitAddress)
import Morley.Tezos.Core
  (ChainId (UnsafeChainId), parseChainId, timestampFromSeconds, timestampQuote, tz)

import Language.LIGO.AST hiding ((<.>))
import Language.LIGO.Debugger.CLI
import Language.LIGO.Debugger.Handlers.Types
import Language.LIGO.Extension
import Language.LIGO.Range

test_Compilation :: TestTree
test_Compilation = testGroup "Getting debug info"
  [ testCase "simple-ops.mligo contract" do
      let file = contractsDir </> "simple-ops.mligo"
      let (a, b) <-> (c, d) = Range (LigoPosition a b) (LigoPosition c d) file
      res <- compileLigoContractDebug "main" file

      let returnType = mkPairType
            (mkConstantType "List" [mkSimpleConstantType "Operation"])
            intType'

      let uncurriedMainType = LigoTypeResolved $
            mkPairType
              (mkSumType (LLField "Main") [("Main", unitType')])
              intType'
            ~> returnType

      let curriedMainType = LigoTypeResolved $
            unitType' ~> intType' ~> returnType

      take 15 (makeConciseLigoIndexedInfo (lmTypes res) <$> toList (lmLocations res)) @?= mconcat
        [ replicate 7 LigoEmptyLocationInfo

        , [ LigoMereEnvInfo [LigoHiddenStackEntry] ]

        , [ LigoMereLocInfo ((2, 1) <-> (5, 30)) uncurriedMainType ]
        , [ LigoMereLocInfo ((2, 1) <-> (5, 30)) curriedMainType ]

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
      res @?= U.ValuePair (U.ValueInt 0) (U.ValuePair (U.ValueInt 0) (U.ValueString [mt|!|]))

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

test_ModuleNamesCollection :: TestTree
test_ModuleNamesCollection = testGroup "Getting module names"
  [ testCase "Two module names" do
      let file = contractsDir </> "two-module-names.mligo"

      ModuleNamesList res <- getAvailableModules file
      res @~=? ["Main1.main1", "Main1.$main", "Main2.main2", "Main2.$main"]

  , testCase "Zero module names" do
      let file = contractsDir </> "no-modules.mligo"

      ModuleNamesList res <- getAvailableModules file
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

      ModuleNamesList _res <- getAvailableModules file
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
      [ testCase "Decompile record" $ legacyMode do
          let recordVal = T.SomeValue $ T.toVal ((42 :: Integer, [mt|str|]), True)

          let recordLayout = LLInner
                [ LLInner
                    [ LLField "a"
                    , LLField "b"
                    ]
                , LLField "c"
                ]

          let recordType = LigoTypeResolved $ mkRecordType recordLayout
                [ ("a", intType')
                , ("b", mkSimpleConstantType "String")
                , ("c", boolType')
                ]

          decompiled <- decompileValues [(recordType, recordVal)]

          let expected = LVRecord $ HM.fromList
                [ (LLabel "a", LVCt $ LCInt "42")
                , (LLabel "b", LVCt $ LCString "str")
                , (LLabel "c", LVCt $ LCBool True)
                ]

          [expected] @?= decompiled

      , testCase "Decompile record with layout comb" do
          let recordVal = T.SomeValue $ T.toVal (42 :: Integer, ([mt|str|], True))
          let recordType = LigoTypeResolved $ mkRecordType (combLayout ["a", "b", "c"])
                [ ("a", intType')
                , ("b", mkSimpleConstantType "String")
                , ("c", boolType')
                ]

          decompiled <- decompileValues [(recordType, recordVal)]

          let expected = LVRecord $ HM.fromList
                [ (LLabel "a", LVCt $ LCInt "42")
                , (LLabel "b", LVCt $ LCString "str")
                , (LLabel "c", LVCt $ LCBool True)
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
      [ testCase "Decompile sum with default layout" $ legacyMode do
          let sumVal = T.SomeValue $ T.toVal (Left @_ @() $ Right @Integer [mt|str|])

          let sumLayout = LLInner
                [ LLInner
                    [ LLField "A"
                    , LLField "B"
                    ]
                , LLField "C"
                ]

          let sumType = LigoTypeResolved $ mkSumType sumLayout
                [ ("A", intType')
                , ("B", mkSimpleConstantType "String")
                , ("C", unitType')
                ]

          decompiled <- decompileValues [(sumType, sumVal)]

          [LVConstructor ("B", LVCt $ LCString "str")] @?= decompiled

      , testCase "Decompile sum with layout comb" do
          let sumVal = T.SomeValue $ T.toVal (Right @Integer $ Left @_ @() [mt|str|])
          let sumType = LigoTypeResolved $ mkSumType (combLayout ["A", "B", "C"])
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

          [LVCt $ LCChainId "NetXH12Aer3be93"] @?= decompiled

      , testCase "Chain id from bytes" do
          let chainIdVal = T.SomeValue $ T.toVal $ UnsafeChainId "\0\0\0\0"
          let chainIdType = LigoTypeResolved $ mkSimpleConstantType "Chain_id"

          decompiled <- decompileValues [(chainIdType, chainIdVal)]

          [LVCt $ LCChainId "NetXH12Aer3be93"] @?= decompiled
      ]

  , testCase "Complex value" $ legacyMode do
      let complexVal = T.SomeValue
            $ T.toVal
            $ Left @_ @()
                ( (UnsafeChainId "\0\0\0\0", [mt|large|])
                , Right @Integer (Left @_ @MText [timestampQuote|1970-01-01T00:01:40Z|])
                )

      let innerRecordLayout = LLInner
            [ LLInner
                [ LLField "ch"
                , LLField "s"
                ]
            , LLField "state"
            ]

      let complexType = LigoTypeResolved $ mkSumType (twoElemTreeLayout "Go" "Nop")
            [ ( "Go"
              , mkRecordType innerRecordLayout
                  [ ("ch", mkSimpleConstantType "Chain_id")
                  , ( "state"
                    , mkSumType (combLayout ["B", "A", "C"])
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
                [ (LLabel "ch", LVCt $ LCChainId "NetXH12Aer3be93")
                , (LLabel "state", LVConstructor ("A", LVCt $ LCTimestamp "100"))
                , (LLabel "s", LVCt $ LCString "large")
                ]
            )

      [expected] @?= decompiled
  ]
  where
    decompileValues :: [(LigoType, T.SomeValue)] -> IO [LigoValue]
    decompileValues typesAndValues = do
      decompiledValues <- decompileLigoValues typesAndValues
      pure $ catMaybes decompiledValues

test_config_resolution :: TestTree
test_config_resolution = testGroup "LIGO config resolution"
  let configsPath = "test" </> "configs" in

  let checkResolutionWithExt
        :: FilePath
        -> FilePath
        -> LigoLaunchRequest
        -> IO (Either LigoResolveConfigException ())
      checkResolutionWithExt configName ext expected = try do
        let fullPath = configsPath </> configName <.> ext
        resolveConfig fullPath @@?= expected
  in
  let checkResolution configName expected =
        [ checkResolutionWithExt configName ext expected
        | ext <- supportedExtensions
        ] & nonEmpty
          & fromMaybe (error "Expected list to be non-empty")
          & foldl1 \ma mb -> ((,) <$> ma <*> mb) >>= \case
              (Left exc, Left{}) -> pure (Left exc)
              (Right{}, Right{}) -> pure pass
              _ -> assertFailure "Expected result for all extensions to be the same"
  in
  [ testCase "Full config" do
      let expectedConfiguration = LigoLaunchRequest
            { noDebug = Nothing
            , logDir = Just "tmp/contract.log"
            , program = Just "main.mligo"
            , moduleName = Just "default"
            , storage = Just [int||"some_storage"|]
            , entrypoint = Just "main"
            , parameter = Just [int||"some_param"|]
            , contractEnv = Just LigoContractEnv
                { now = Just $ MichelsonJson [timestampQuote|2020-01-01T00:00:00Z|]
                , balance = Just $ MichelsonJson [tz|1|]
                , amount = Just $ MichelsonJson [tz|2|]
                , self = Just [ta|KT1XQcegsEtio9oGbLUHA8SKX4iZ2rpEXY9b|]
                , source = Just $ Constrained [ta|tz1hTK4RYECTKcjp2dddQuRGUX5Lhse3kPNY|]
                , sender = Just $ Constrained [ta|tz1hTK4RYECTKcjp2dddQuRGUX5Lhse3kPNY|]
                , chainId = Just $ MichelsonJson $ unsafe $ parseChainId "NetXH12Aer3be93"
                , level = Just $ MichelsonJson 10000
                , votingPowers = Just $ mkVotingPowers
                        [ ([ta|tz1aZcxeRT4DDZZkYcU3vuBaaBRtnxyTmQRr|], 40)
                        , ([ta|tz1hTK4RYECTKcjp2dddQuRGUX5Lhse3kPNY|], 60)
                        ]
                }
            }

      res <- checkResolution "full_config" expectedConfiguration
      res @? isRight

  , testCase "Omit some fields in the config" do
      let expectedConfiguration = LigoLaunchRequest
            { noDebug = Nothing
            , logDir = Just "tmp/contract.log"
            , program = Nothing
            , moduleName = Just "default"
            , storage = Nothing
            , entrypoint = Nothing
            , parameter = Just [int||"some_param"|]
            , contractEnv = Just LigoContractEnv
                { now = Just $ MichelsonJson [timestampQuote|2020-01-01T00:00:00Z|]
                , balance = Just $ MichelsonJson [tz|1|]
                , amount = Just $ MichelsonJson [tz|2|]
                , self = Just [ta|KT1XQcegsEtio9oGbLUHA8SKX4iZ2rpEXY9b|]
                , source = Just $ Constrained [ta|tz1hTK4RYECTKcjp2dddQuRGUX5Lhse3kPNY|]
                , sender = Nothing
                , chainId = Nothing
                , level = Nothing
                , votingPowers = Nothing
                }
            }

      res <- checkResolution "omitted_fields" expectedConfiguration
      res @? isRight

  , testCase "Config with computations" do
      let expectedConfiguration = LigoLaunchRequest
            { noDebug = Nothing
            , logDir = Nothing
            , program = Nothing
            , moduleName = Nothing
            , storage = Just [int||Unit|]
            , entrypoint = Just [int||main|]
            , parameter = Just [int||(Pair 200 "just a regular string")|]
            , contractEnv = Just LigoContractEnv
                { now = Nothing
                , balance = Nothing
                , amount = Just $ MichelsonJson [tz|3|]
                , self = Nothing
                , source = Nothing
                , sender = Nothing
                , chainId = Nothing
                , level = Nothing
                , votingPowers = Nothing
                }
            }

      res <- checkResolution "config_with_computations" expectedConfiguration
      res @? isRight

  , testCase "Malformed config" do
      -- We don't care about this value since the
      -- config in this test is malformed.
      let expectedConfiguration = LigoLaunchRequest
            { noDebug = Nothing
            , logDir = Nothing
            , program = Nothing
            , entrypoint = Nothing
            , storage = Nothing
            , moduleName = Nothing
            , parameter = Nothing
            , contractEnv = Nothing
            }

      res <- checkResolution "malformed_config" expectedConfiguration
      res @? isLeft
  ]
  where
    mkVotingPowers :: [(ImplicitAddress, Natural)] -> VotingPowersConfig
    mkVotingPowers = SimpleVotingPowers
      . SimpleVotingPowersInfo
      . M.fromList
      . map (bimap (MichelsonJson . unImplicitAddress) MichelsonJson)

test_Dumped_cst_parse :: TestTree
test_Dumped_cst_parse = reinsuring $ testCase "Dumped cst parse" do
  contracts <- scanContracts (const True) compilerContractsDir

  errors <- newIORef []

  forM_ contracts \contract -> do
    try @_ @SomeException (decodeCST (contract :| [])) >>= \case
      Left (fromException -> Just (err :: LigoDecodeException)) -> do
        modifyIORef errors ((contract, err) :)
      _ -> pass

  readIORef errors >>= \case
    [] -> pass
    errs -> do
      let errMsg = errs
            <&> do \(file, err) -> [int||Error occurred in file #{file}: #{err}|]
            & unlines

      assertFailure (toString errMsg)
