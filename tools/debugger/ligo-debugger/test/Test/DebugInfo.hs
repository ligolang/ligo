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

import Language.LIGO.AST (LIGO)
import Language.LIGO.Debugger.CLI.Call
import Language.LIGO.Debugger.CLI.Types
import Language.LIGO.Debugger.Common
import Language.LIGO.Debugger.Handlers.Helpers
import Language.LIGO.Debugger.Michelson
import Language.LIGO.Parser (ParsedInfo)
import Language.LIGO.Range

data SomeInstr = forall i o. SomeInstr (T.Instr i o)

instance Eq SomeInstr where
  SomeInstr i1 == SomeInstr i2 = T.instrToOps i1 == T.instrToOps i2
deriving stock instance Show SomeInstr

instance Buildable SomeInstr where
  build (SomeInstr i) = build i

-- | Get instructions with associated 'InstrNo' metas.
collectCodeMetas :: HashMap FilePath (LIGO ParsedInfo) -> T.Instr i o -> [(EmbeddedLigoMeta, SomeInstr)]
collectCodeMetas parsedContracts = T.dfsFoldInstr def { dsGoToValues = True } \case
  T.ConcreteMeta meta i ->
    case liiLocation meta of
      Just loc
        | shouldIgnoreMeta loc i parsedContracts -> mempty
      _ -> one (meta, SomeInstr i)
  _ -> mempty

collectContractMetas :: HashMap FilePath (LIGO ParsedInfo) -> T.Contract cp st -> [(EmbeddedLigoMeta, SomeInstr)]
collectContractMetas parsedContracts = collectCodeMetas parsedContracts . T.unContractCode . T.cCode

-- | Run the given contract + entrypoint, build code with embedded ligo metadata.
buildSourceMapper
  :: FilePath
  -> String
  -> IO (Set ExpressionSourceLocation, T.SomeContract, [FilePath], HashSet Range, LigoType, LigoTypesVec)
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

      (exprLocs, T.SomeContract contract, allFiles, _, _, ligoTypesVec) <- buildSourceMapper file "main"

      parsedContracts <- parseContracts allFiles

      let nonEmptyMetasAndInstrs =
            map (first $ makeConciseLigoIndexedInfo ligoTypesVec) $
            filter (hasn't (_1 . _Empty)) $
            collectContractMetas parsedContracts contract

      let unitIntTuple' = mkPairType
            (mkSimpleConstantType "Unit")
            (mkSimpleConstantType "Int")

      let unitIntTuple = LigoTypeResolved unitIntTuple'

      let operationList' = mkConstantType "List" [mkSimpleConstantType "operation"]
      let operationList = LigoTypeResolved operationList'

      let resultType' = mkPairType operationList' intType'
      let resultType = LigoTypeResolved resultType'

      let mainType = LigoTypeResolved $ unitIntTuple' ~> resultType'

      nonEmptyMetasAndInstrs
        @?=
        [ LigoMereEnvInfo
            [LigoHiddenStackEntry]
            ?- SomeInstr dummyInstr

        , LigoMereEnvInfo
            [LigoStackEntryNoVar unitIntTuple]
            ?- SomeInstr dummyInstr

        , LigoMereEnvInfo
            [LigoStackEntryNoVar unitIntTuple]
            ?- SomeInstr dummyInstr

        , LigoMereEnvInfo
            [LigoStackEntryNoVar intType]
            ?- SomeInstr dummyInstr

        , LigoMereEnvInfo
            [LigoStackEntryNoVar intType]
            ?- SomeInstr dummyInstr

        , LigoMereEnvInfo
            [LigoStackEntryNoVar intType]
            ?- SomeInstr dummyInstr

        , LigoMereEnvInfo
            [LigoStackEntryNoVar intType]
            ?- SomeInstr dummyInstr

        , LigoMereEnvInfo
            [LigoStackEntryVar "s" intType]
            ?- SomeInstr dummyInstr

        , LigoMereLocInfo
            (Range (LigoPosition 2 12) (LigoPosition 2 18) file)
            intType
            ?- SomeInstr (T.ADD @'T.TInt @'T.TInt)

        , LigoMereLocInfo
            (Range (LigoPosition 2 12) (LigoPosition 2 18) file)
            intType
            ?- SomeInstr
                (    T.Nested
                $    T.Nested (T.PUSH @'T.TInt (T.VInt 42))
                T.:# T.Nested (T.SWAP)
                T.:# T.ADD @'T.TInt @'T.TInt
                )

        , LigoMereEnvInfo
            [LigoStackEntryVar "s2" intType]
            ?- SomeInstr dummyInstr

        , LigoMereLocInfo
            (Range (LigoPosition 3 12) (LigoPosition 3 19) file)
            intType
            ?- SomeInstr (T.MUL @'T.TInt @'T.TInt)

        , LigoMereLocInfo
            (Range (LigoPosition 3 12) (LigoPosition 3 19) file)
            intType
            ?- SomeInstr
                (    T.Nested
                $    T.Nested (T.DUPN @_ @_ @_ @'T.TInt $ toPeanoNatural' @2)
                T.:# T.Nested (T.DUPN @_ @_ @_ @'T.TInt $ toPeanoNatural' @3)
                T.:# T.MUL @'T.TInt @'T.TInt
                )

        , LigoMereLocInfo
            (Range (LigoPosition 3 12) (LigoPosition 3 23) file)
            intType
            ?- SomeInstr (T.MUL @'T.TInt @'T.TInt)

        , LigoMereLocInfo
            (Range (LigoPosition 3 12) (LigoPosition 3 23) file)
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
            [LigoStackEntryVar "s2" intType]
            ?- SomeInstr dummyInstr

        , LigoMereLocInfo
            (Range (LigoPosition 4 4) (LigoPosition 4 25) file)
            operationList
            ?- SomeInstr (T.NIL @'T.TOperation)

        , LigoMereLocInfo
            (Range (LigoPosition 4 4) (LigoPosition 4 25) file)
            operationList
            ?- SomeInstr (T.Nested $ T.NIL @'T.TOperation)

        , LigoMereLocInfo
            (Range (LigoPosition 4 4) (LigoPosition 4 29) file)
            resultType
            ?- SomeInstr
                (    T.Nested
                $    T.Nested T.Nop
                T.:# T.Nested (T.NIL @'T.TOperation)
                T.:# T.PAIR
                )

        , LigoMereEnvInfo
            [ LigoStackEntryVar "main" mainType
            , LigoHiddenStackEntry
            ]
            ?- SomeInstr dummyInstr

        ]

      ((_slStart &&& _slEnd) <$> toList (getInterestingSourceLocations parsedContracts exprLocs))
        @?=
        -- Note: the order of entries below is not the interpretation order
        -- because we extracted these pairs from Set with its lexicographical order
        [ ( SrcLoc 1 11
          , SrcLoc 1 17
          )
        , ( SrcLoc 2 11
          , SrcLoc 2 18
          )
        , ( SrcLoc 2 11
          , SrcLoc 2 22
          )
        , ( SrcLoc 3 3
          , SrcLoc 3 24
          )
        , ( SrcLoc 3 3
          , SrcLoc 3 28
          )
        ]

  , testCase "metas are not shifted in `if` blocks" do
      let file = contractsDir </> "if.mligo"
      (_, T.SomeContract contract, allFiles, _, _, _) <- buildSourceMapper file "main"

      parsedContracts <- parseContracts allFiles

      forM_ @_ @_ @() (collectContractMetas parsedContracts contract)
        \(LigoIndexedInfo{..}, SomeInstr instr) -> case instr of
          T.MUL -> liiLocation @?= Just
            do Range (LigoPosition 2 27) (LigoPosition 2 32) file
          T.CAR -> liiLocation @?= Just
            do Range (LigoPosition 2 38) (LigoPosition 2 43) file
          _ -> pass

  ]


test_Errors :: TestTree
test_Errors = testGroup "Errors"
  [ testCase "duplicated ticket error is recognized" do
      let file = contractsDir </> "dupped-ticket.mligo"
      ligoMapper <- compileLigoContractDebug "main" file
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
      (Set.map (rangeToSourceLocation . eslRange) -> locs, _, _, _, _, _) <- buildSourceMapper file "main"

      forM_ (uncurry (makeSourceLocation file) <$> expectedLocs) \loc -> do
        if Set.member loc locs
        then pass
        else assertFailure [int||Expected #{loc} to be a part of #{toList locs}|]
  in
  [ testCase "Locations for built-ins" do
      let expectedLocs =
            [ ((2, 10), (2, 16)) -- "is_nat" location
            , ((3, 11), (3, 17)) -- "assert" location
            , ((9, 12), (9, 21)) -- "List.fold" location
            ]

      checkLocations "builtins-locations.mligo" expectedLocs

  , testCase "Locations for user-defined functions" do
      let expectedLocs =
            [ ((3, 32), (3, 35)) -- "add" location
            , ((7, 12), (7, 16)) -- "add5" location
            ]

      checkLocations "apply.mligo" expectedLocs
  ]
