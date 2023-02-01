-- | Reading debug info from ligo debug output.
module Test.DebugInfo
  ( module Test.DebugInfo
  ) where

import AST (LIGO)
import Control.Lens (_Empty, hasn't)
import Data.Default (def)
import Data.Set qualified as Set
import Fmt (Buildable (..), pretty)
import Parser (Info)
import Test.HUnit (Assertion)
import Test.Tasty (TestTree, testGroup)
import Test.Util
import Text.Interpolation.Nyan

import Morley.Debugger.Core (SourceLocation (..))
import Morley.Michelson.ErrorPos (Pos (..), SrcPos (..))
import Morley.Michelson.Parser.Types (MichelsonSource (MSFile))
import Morley.Michelson.Typed qualified as T
import Morley.Michelson.Typed.Util (dsGoToValues)

import Language.LIGO.Debugger.CLI.Call
import Language.LIGO.Debugger.CLI.Types
import Language.LIGO.Debugger.Common
import Language.LIGO.Debugger.Handlers.Helpers
import Language.LIGO.Debugger.Michelson

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

-- | Run the given contract + entrypoint, build code with embedded ligo metadata,
buildSourceMapper
  :: FilePath
  -> String
  -> IO (Set ExpressionSourceLocation, T.SomeContract, [FilePath])
buildSourceMapper file entrypoint = do
  ligoMapper <- compileLigoContractDebug entrypoint file
  case readLigoMapper ligoMapper typesReplaceRules instrReplaceRules of
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

      (exprLocs, T.SomeContract contract, allFiles) <- buildSourceMapper file "main"

      parsedContracts <- parseContracts allFiles

      let nonEmptyMetasAndInstrs =
            map (first stripSuffixHashFromLigoIndexedInfo) $
            filter (hasn't (_1 . _Empty)) $
            collectContractMetas parsedContracts contract

      let unitIntTuple = LigoTypeResolved
            ( mkPairType
                (mkSimpleConstantType "Unit")
                (mkSimpleConstantType "Int")
            )

      let mainType = LigoTypeResolved
            ( mkPairType
                (mkSimpleConstantType "Unit")
                (mkSimpleConstantType "Int")
              `mkArrowType`
              mkPairType
                (mkConstantType "List" [mkSimpleConstantType "operation"])
                (mkSimpleConstantType "Int")
            )

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
            [LigoStackEntryVar "s" intType]
            ?- SomeInstr dummyInstr

        , LigoMereLocInfo
            (LigoRange file (LigoPosition 2 11) (LigoPosition 2 17))
            ?- SomeInstr (T.ADD @'T.TInt @'T.TInt)

        , LigoMereEnvInfo
            [LigoStackEntryVar "s2" intType]
            ?- SomeInstr dummyInstr

        , LigoMereLocInfo
            (LigoRange file (LigoPosition 3 11) (LigoPosition 3 18))
            ?- SomeInstr (T.MUL @'T.TInt @'T.TInt)

        , LigoMereLocInfo
            (LigoRange file (LigoPosition 3 11) (LigoPosition 3 22))
            ?- SomeInstr (T.MUL @'T.TInt @'T.TInt)

        , LigoMereEnvInfo
            [LigoStackEntryVar "s2" intType]
            ?- SomeInstr dummyInstr

        , LigoMereLocInfo
            (LigoRange file (LigoPosition 4 3) (LigoPosition 4 24))
            ?- SomeInstr (T.NIL @'T.TOperation)

        , LigoMereLocInfo
            (LigoRange file (LigoPosition 4 3) (LigoPosition 4 28))
            ?- SomeInstr T.PAIR

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
        [ ( SrcPos (Pos 1) (Pos 11)
          , SrcPos (Pos 1) (Pos 17)
          )
        , ( SrcPos (Pos 2) (Pos 11)
          , SrcPos (Pos 2) (Pos 18)
          )
        , ( SrcPos (Pos 2) (Pos 11)
          , SrcPos (Pos 2) (Pos 22)
          )
        , ( SrcPos (Pos 3) (Pos 3)
          , SrcPos (Pos 3) (Pos 24)
          )
        , ( SrcPos (Pos 3) (Pos 3)
          , SrcPos (Pos 3) (Pos 28)
          )
        ]

  , testCase "metas are not shifted in `if` blocks" do
      let file = contractsDir </> "if.mligo"
      (_, T.SomeContract contract, allFiles) <- buildSourceMapper file "main"

      parsedContracts <- parseContracts allFiles

      forM_ @_ @_ @() (collectContractMetas parsedContracts contract)
        \(LigoIndexedInfo{..}, SomeInstr instr) -> case instr of
          T.MUL -> liiLocation @?= Just
            do LigoRange file (LigoPosition 2 26) (LigoPosition 2 31)
          T.CAR -> liiLocation @?= Just
            do LigoRange file (LigoPosition 2 37) (LigoPosition 2 42)
          _ -> pass

  ]


test_Errors :: TestTree
test_Errors = testGroup "Errors"
  [ testCase "duplicated ticket error is recognized" do
      let file = contractsDir </> "dupped-ticket.mligo"
      ligoMapper <- compileLigoContractDebug "main" file
      case readLigoMapper ligoMapper typesReplaceRules instrReplaceRules of
        Left (PreprocessError UnsupportedTicketDup) -> pass
        _ -> assertFailure [int||Expected "UnsupportedTicketDup" error.|]
  ]

test_Function_call_locations :: TestTree
test_Function_call_locations = testGroup "Function call locations"
  let
    makeSourceLocation :: FilePath -> (Word, Word) -> (Word, Word) -> SourceLocation
    makeSourceLocation filepath (startLine, startCol) (endLine, endCol) =
      SourceLocation
        (MSFile filepath)
        (SrcPos (Pos $ startLine - 1) (Pos startCol))
        (SrcPos (Pos $ endLine - 1) (Pos endCol))

    checkLocations :: FilePath -> [((Word, Word), (Word, Word))] -> Assertion
    checkLocations contractName expectedLocs = do
      let file = contractsDir </> contractName
      (Set.map (ligoRangeToSourceLocation . eslLigoRange) -> locs, _, _) <- buildSourceMapper file "main"

      forM_ (uncurry (makeSourceLocation file) <$> expectedLocs) \loc -> do
        if Set.member loc locs
        then pass
        else assertFailure [int||Expected #{loc} to be a part of #{toList locs}|]
  in
  [ testCase "Locations for built-ins" do
      let expectedLocs =
            [ ((2, 12), (2, 18)) -- "is_nat" location
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
