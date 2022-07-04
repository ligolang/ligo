-- | Reading debug info from ligo debug output.
module Test.DebugInfo
  ( module Test.DebugInfo
  ) where

import Control.Lens (_Empty, hasn't)
import Data.Default (def)
import Data.Typeable (cast)
import Fmt (Buildable (..), pretty)
import Morley.Michelson.Typed qualified as T
import Test.Tasty (TestTree, testGroup)

import Morley.Debugger.Core (SourceLocation (..))
import Morley.Michelson.ErrorPos (Pos (..), SrcPos (..))

import Language.LIGO.Debugger.CLI.Call
import Language.LIGO.Debugger.CLI.Types
import Language.LIGO.Debugger.Michelson
import Language.LIGO.Debugger.Snapshots
import Test.Util

data SomeInstr = forall i o. SomeInstr (T.Instr i o)

instance Eq SomeInstr where
  SomeInstr i1 == SomeInstr i2 = T.instrToOps i1 == T.instrToOps i2
deriving stock instance Show SomeInstr

instance Buildable SomeInstr where
  build (SomeInstr i) = build i

-- | Get instructions with associated 'InstrNo' metas.
collectMetas :: T.Instr i o -> [(EmbeddedLigoMeta, SomeInstr)]
collectMetas = T.dfsFoldInstr def \case
  T.Meta (T.SomeMeta (cast -> Just (meta :: EmbeddedLigoMeta))) i ->
    one (meta, SomeInstr i)
  _ -> mempty

(?-) :: a -> b -> (a, b)
(?-) = (,)
infixr 0 ?-

-- | LIGO uses them to insert metadata between actual instructions
dummyInstr :: T.Instr a a
dummyInstr = T.Nested T.Nop

-- | Eventually this should be removed, it exists as a temporary
-- measure to make tests pass
unknownSrcPos :: SrcPos
unknownSrcPos = SrcPos (Pos 0) (Pos 0)

test_SourceMapper :: TestTree
test_SourceMapper = testGroup "Reading source mapper"
  [ testCase "simple-ops.mligo contract" do
      let file = contractsDir </> "simple-ops.mligo"
      ligoMapper <- compileLigoContractDebug "main" file
      (allLocs, T.SomeContract contract) <-
        case readLigoMapper ligoMapper of
          Right v -> pure v
          Left err -> assertFailure $ pretty err

      let metasAndInstrs =
            filter (hasn't (_1 . _Empty)) $
            toList $ collectMetas (T.cCode contract)
      metasAndInstrs
        @?=
        [ LigoMereEnvInfo
            [LigoHiddenStackEntry]
            ?- SomeInstr dummyInstr

        , LigoMereEnvInfo
            [LigoStackEntryNoVar intType]
            ?- SomeInstr dummyInstr

        , LigoMereLocInfo
            (LigoRange file (LigoPosition 2 15) (LigoPosition 2 17))
            ?- SomeInstr (T.PUSH $ T.VInt 42)

        , LigoMereLocInfo
            (LigoRange file (LigoPosition 2 11) (LigoPosition 2 17))
            ?- SomeInstr (T.ADD @'T.TInt @'T.TInt)

        , LigoMereEnvInfo
            [LigoStackEntryVar "s2" intType]
            ?- SomeInstr dummyInstr

        , LigoMereLocInfo
            (LigoRange file (LigoPosition 3 21) (LigoPosition 3 22))
            ?- SomeInstr (T.PUSH $ T.VInt 2)

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

        ]

      (_slSrcPos <$> toList allLocs)
        @?=
        -- Note: some ranges have the same start position and different
        -- end positions - since we account for only the former, they
        -- get de-duplicated.
        [ unknownSrcPos
        , SrcPos (Pos 1) (Pos 11)
        , SrcPos (Pos 1) (Pos 15)
        , SrcPos (Pos 2) (Pos 11)
        , SrcPos (Pos 2) (Pos 21)
        , SrcPos (Pos 3) (Pos 3)
        ]
  ]
