-- | Reading debug info from ligo debug output.
module Test.DebugInfo
  ( test_SourceMapper
  , test_Errors
  , test_Function_call_locations
  ) where

import Control.Lens (_Empty, hasn't)
import Data.Default (def)
import Data.Set qualified as Set
import Fmt (Buildable (..), pretty)
import Test.HUnit (Assertion)
import Test.Tasty (TestTree, testGroup)
import Test.Util
import Text.Interpolation.Nyan hiding (rmode')

import Morley.Debugger.Core (SourceLocation, SourceLocation' (..), SrcLoc (..))
import Morley.Michelson.Parser.Types (MichelsonSource (MSFile))
import Morley.Michelson.Typed qualified as T
import Morley.Michelson.Typed.Util (dsGoToValues)
import Morley.Util.PeanoNatural (toPeanoNatural')

import Language.LIGO.AST (Info, LIGO)
import Language.LIGO.Debugger.CLI.Call
import Language.LIGO.Debugger.CLI.Types
import Language.LIGO.Debugger.Common
import Language.LIGO.Debugger.Handlers.Helpers
import Language.LIGO.Debugger.Michelson
import Language.LIGO.Range

data SomeInstr = forall i o. SomeInstr (T.Instr i o)

instance Eq SomeInstr where
  SomeInstr i1 == SomeInstr i2 = T.instrToOps i1 == T.instrToOps i2
deriving stock instance Show SomeInstr

instance Buildable SomeInstr where
  build (SomeInstr i) = build i

-- | Get instructions with associated 'InstrNo' metas.
collectCodeMetas :: HashMap FilePath (LIGO Info) -> T.Instr i o -> [(EmbeddedLigoMeta, SomeInstr)]
collectCodeMetas parsedContracts = T.dfsFoldInstr def { dsGoToValues = True } \case
  T.ConcreteMeta meta i ->
    case liiLocation meta of
      Just loc
        | shouldIgnoreMeta loc i parsedContracts -> mempty
      _ -> one (meta, SomeInstr i)
  _ -> mempty

collectContractMetas :: HashMap FilePath (LIGO Info) -> T.Contract cp st -> [(EmbeddedLigoMeta, SomeInstr)]
collectContractMetas parsedContracts = collectCodeMetas parsedContracts . T.unContractCode . T.cCode

-- | Run the given contract + entrypoint, build code with embedded ligo metadata.
buildSourceMapper
  :: FilePath
  -> ModuleName
  -> IO LigoMapperResult
buildSourceMapper file entrypoint = do
  ligoMapper <- compileLigoContractDebug entrypoint file
  case readLigoMapper ligoMapper of
    Right v -> pure v
    Left err -> assertFailure $ pretty err

(?-) :: a -> b -> (a, b)
(?-) = (,)
infixr 0 ?-

-- | LIGO uses them to insert metadata between actual instructions
dummyInstr :: T.Instr a a
dummyInstr = T.Nested T.Nop

test_SourceMapper :: TestTree
test_SourceMapper = testGroup "Reading source mapper"
  [ testCase "simple-ops.mligo contract" do
      let file = contractsDir </> "simple-ops.mligo"

      LigoMapperResult exprLocs (T.SomeContract contract) allFiles _ _ ligoTypesVec _
        <- buildSourceMapper file ""

      parsedContracts <- parseContracts allFiles

      let nonEmptyMetasAndInstrs =
            map (first $ makeConciseLigoIndexedInfo ligoTypesVec) $
            filter (hasn't (_1 . _Empty)) $
            collectContractMetas parsedContracts contract

      let unitIntTuple' = mkPairType
            (mkSumType (LLField "Main") [("Main", unitType')])
            intType'

      let unitIntTuple = LigoTypeResolved unitIntTuple'

      let operationList' = mkConstantType "List" [mkSimpleConstantType "operation"]
      let operationList = LigoTypeResolved operationList'

      let resultType' = mkPairType operationList' intType'
      let resultType = LigoTypeResolved resultType'

      let curriedMainType = LigoTypeResolved $ unitType' ~> intType' ~> resultType'
      let uncurriedMainType = LigoTypeResolved $ unitIntTuple' ~> resultType'

      let fileName = Just file

      nonEmptyMetasAndInstrs
        @?=
        [ LigoMereEnvInfo
            [LigoHiddenStackEntry]
            ?- SomeInstr dummyInstr

        , LigoMereEnvInfo
            [LigoStackEntryVar "s" intType fileName]
            ?- SomeInstr dummyInstr

        , LigoMereLocInfo
            (Range (LigoPosition 3 12) (LigoPosition 3 18) file)
            intType
            ?- SomeInstr (T.ADD @'T.TInt @'T.TInt)

        , LigoMereLocInfo
            (Range (LigoPosition 3 12) (LigoPosition 3 18) file)
            intType
            ?- SomeInstr
                (    T.Nested
                $    T.Nested (T.PUSH @'T.TInt (T.VInt 42))
                T.:# T.Nested (T.SWAP)
                T.:# T.ADD @'T.TInt @'T.TInt
                )

        , LigoMereEnvInfo
            [LigoStackEntryVar "s2" intType fileName]
            ?- SomeInstr dummyInstr

        , LigoMereLocInfo
            (Range (LigoPosition 4 12) (LigoPosition 4 19) file)
            intType
            ?- SomeInstr (T.MUL @'T.TInt @'T.TInt)

        , LigoMereLocInfo
            (Range (LigoPosition 4 12) (LigoPosition 4 19) file)
            intType
            ?- SomeInstr
                (    T.Nested
                $    T.Nested (T.DUPN @_ @_ @_ @'T.TInt $ toPeanoNatural' @2)
                T.:# T.Nested (T.DUPN @_ @_ @_ @'T.TInt $ toPeanoNatural' @3)
                T.:# T.MUL @'T.TInt @'T.TInt
                )

        , LigoMereLocInfo
            (Range (LigoPosition 4 12) (LigoPosition 4 23) file)
            intType
            ?- SomeInstr (T.MUL @'T.TInt @'T.TInt)

        , LigoMereLocInfo
            (Range (LigoPosition 4 12) (LigoPosition 4 23) file)
            intType
            ?- SomeInstr
                (    T.Nested
                $    T.Nested (T.PUSH @'T.TInt (T.VInt 2))
                T.:# T.Nested
                       (    T.Nested (T.DUPN @_ @_ @_ @'T.TInt $ toPeanoNatural' @2)
                       T.:# T.Nested (T.DUPN @_ @_ @_ @'T.TInt $ toPeanoNatural' @3)
                       T.:# T.MUL @'T.TInt @'T.TInt
                       )
                T.:# T.MUL
                T.:# T.DROP
                )

        , LigoMereEnvInfo
            [LigoStackEntryVar "s2" intType fileName]
            ?- SomeInstr dummyInstr

        , LigoMereLocInfo
            (Range (LigoPosition 5 4) (LigoPosition 5 25) file)
            operationList
            ?- SomeInstr (T.NIL @'T.TOperation)

        , LigoMereLocInfo
            (Range (LigoPosition 5 4) (LigoPosition 5 25) file)
            operationList
            ?- SomeInstr (T.Nested $ T.NIL @'T.TOperation)

        , LigoMereLocInfo
            (Range (LigoPosition 5 4) (LigoPosition 5 29) file)
            resultType
            ?- SomeInstr
                (    T.Nested
                $    T.Nested T.Nop
                T.:# T.Nested (T.NIL @'T.TOperation)
                T.:# T.PAIR
                )

        , LigoMereEnvInfo
            [ LigoStackEntryVar "main" curriedMainType fileName
            , LigoHiddenStackEntry
            ]
            ?- SomeInstr dummyInstr

        , LigoMereEnvInfo
            [ LigoStackEntryNoVar unitIntTuple
            , LigoStackEntryVar "main" curriedMainType fileName
            ]
            ?- SomeInstr dummyInstr

        , LigoMereEnvInfo
            [ LigoStackEntryNoVar unitIntTuple
            , LigoStackEntryVar "main" curriedMainType fileName
            ]
            ?- SomeInstr dummyInstr

        , LigoMereEnvInfo
            [ LigoStackEntryNoVar unitType
            , LigoStackEntryNoVar intType
            , LigoStackEntryVar "main" curriedMainType fileName
            ]
            ?- SomeInstr dummyInstr

        , LigoMereEnvInfo
            [ LigoStackEntryNoVar unitType
            , LigoStackEntryNoVar intType
            , LigoStackEntryVar "main" curriedMainType fileName
            ]
            ?- SomeInstr dummyInstr

        , LigoMereEnvInfo
            [ LigoStackEntryNoVar intType
            , LigoStackEntryNoVar unitType
            , LigoStackEntryVar "main" curriedMainType fileName
            ]
            ?- SomeInstr dummyInstr

        , LigoMereEnvInfo
            [ LigoStackEntryVar "p" unitType Nothing
            , LigoStackEntryNoVar intType
            , LigoStackEntryVar "main" curriedMainType fileName
            ]
            ?- SomeInstr dummyInstr

        , LigoMereEnvInfo
            [ LigoStackEntryVar "s" intType Nothing
            , LigoStackEntryVar "p" unitType Nothing
            , LigoStackEntryVar "main" curriedMainType fileName
            ]
            ?- SomeInstr dummyInstr

        , LigoMereEnvInfo
            [ LigoStackEntryNoVar unitType
            , LigoStackEntryVar "s" intType Nothing
            , LigoStackEntryVar "main" curriedMainType fileName
            ]
            ?- SomeInstr dummyInstr

        , LigoMereEnvInfo
            [ LigoStackEntryNoVar unitType
            , LigoStackEntryVar "s" intType Nothing
            , LigoStackEntryVar "main" curriedMainType fileName
            ]
            ?- SomeInstr dummyInstr

        , LigoMereEnvInfo
            [ LigoStackEntryNoVar unitIntTuple
            , LigoStackEntryVar "s" intType Nothing
            , LigoStackEntryVar "main" curriedMainType fileName
            ]
            ?- SomeInstr dummyInstr

        , LigoMereEnvInfo
            [ LigoStackEntryVar "<module main>" uncurriedMainType Nothing
            , LigoHiddenStackEntry
            ]
            ?- SomeInstr dummyInstr

        ]

      ((_slStart &&& _slEnd) <$> toList (getInterestingSourceLocations parsedContracts exprLocs))
        @?=
        -- Note: the order of entries below is not the interpretation order
        -- because we extracted these pairs from Set with its lexicographical order
        [ ( SrcLoc 2 11
          , SrcLoc 2 17
          )
        , ( SrcLoc 3 11
          , SrcLoc 3 18
          )
        , ( SrcLoc 3 11
          , SrcLoc 3 22
          )
        , ( SrcLoc 4 3
          , SrcLoc 4 24
          )
        , ( SrcLoc 4 3
          , SrcLoc 4 28
          )
        ]

  , testCase "metas are not shifted in `if` blocks" do
      let file = contractsDir </> "if.mligo"
      LigoMapperResult _ (T.SomeContract contract) allFiles _ _ _ _
        <- buildSourceMapper file ""

      parsedContracts <- parseContracts allFiles

      forM_ @_ @_ @() (collectContractMetas parsedContracts contract)
        \(LigoIndexedInfo{..}, SomeInstr instr) -> case instr of
          T.MUL -> liiLocation @?= Just
            do Range (LigoPosition 3 27) (LigoPosition 3 32) file
          T.CAR -> liiLocation @?= Just
            do Range (LigoPosition 3 38) (LigoPosition 3 43) file
          _ -> pass

  ]


test_Errors :: TestTree
test_Errors = testGroup "Errors"
  [ testCase "duplicated ticket error is recognized" do
      let file = contractsDir </> "dupped-ticket.mligo"
      ligoMapper <- compileLigoContractDebug "" file
      case readLigoMapper ligoMapper of
        Left (PreprocessError UnsupportedTicketDup) -> pass
        _ -> assertFailure [int||Expected "UnsupportedTicketDup" error.|]
  ]

test_Function_call_locations :: TestTree
test_Function_call_locations = testGroup "Function call locations"
  let
    makeSourceLocation :: FilePath -> (Word, Word) -> (Word, Word) -> SourceLocation
    makeSourceLocation filepath (startL, startCol) (endLine, endCol) =
      SourceLocation
        (MSFile filepath)
        (SrcLoc (startL - 1) startCol)
        (SrcLoc (endLine - 1) endCol)

    checkLocations :: FilePath -> [((Word, Word), (Word, Word))] -> Assertion
    checkLocations contractName expectedLocs = do
      let file = contractsDir </> contractName
      LigoMapperResult
        { lmrExpressionLocation = Set.map (rangeToSourceLocation . eslRange) -> locs
        } <- buildSourceMapper file ""

      forM_ (uncurry (makeSourceLocation file) <$> expectedLocs) \loc -> do
        if Set.member loc locs
        then pass
        else assertFailure [int||Expected #{loc} to be a part of #{toList locs}|]
  in
  [ testCase "Locations for built-ins" do
      let expectedLocs =
            [ ((3, 10), (3, 16)) -- "is_nat" location
            , ((4, 11), (4, 17)) -- "assert" location
            , ((10, 12), (10, 21)) -- "List.fold" location
            ]

      checkLocations "builtins-locations.mligo" expectedLocs

  , testCase "Locations for user-defined functions" do
      let expectedLocs =
            [ ((3, 32), (3, 35)) -- "add" location
            , ((8, 12), (8, 16)) -- "add5" location
            ]

      checkLocations "apply.mligo" expectedLocs
  ]
