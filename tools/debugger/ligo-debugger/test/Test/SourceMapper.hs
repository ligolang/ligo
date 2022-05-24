-- | Reading source mapper from ligo debug output.
module Test.SourceMapper
  ( module Test.SourceMapper
  ) where

import Data.Default (def)
import Data.IntMap qualified as IntMap
import Data.Typeable (cast)
import Fmt (Buildable (..), pretty)
import Morley.Michelson.Typed qualified as T
import Test.Tasty (TestTree, testGroup)

import Morley.Debugger.Core (InstrNo (..), SourceLocation (..), SourceMapper (..))
import Morley.Michelson.ErrorPos (Pos (..), SrcPos (..))

import Language.LIGO.Debugger.CLI.Call
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
collectInstrNo :: T.Instr i o -> IntMap SomeInstr
collectInstrNo = T.dfsFoldInstr def \case
  T.Meta (T.SomeMeta (cast -> Just ((InstrNo instrNo, _) :: EmbeddedLigoMeta))) i ->
    one (instrNo, SomeInstr i)
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
      let file = inContractsDir "simple-ops.mligo"
      ligoMapper <- compileLigoContractDebug file
      (SourceMapper srcMapper, T.SomeContract contract) <-
        case readLigoMapper ligoMapper of
          Right v -> pure v
          Left err -> assertFailure $ pretty err
      id
        ( IntMap.toList $ collectInstrNo (T.cCode contract)
        , IntMap.toList srcMapper
            & map (second _slSrcPos)
        )
        @?=
        ( [ 0  ?- SomeInstr $ dummyInstr
          , 2  ?- SomeInstr $ dummyInstr
          , 4  ?- SomeInstr $ T.ADD @'T.TInt @'T.TInt
          , 5  ?- SomeInstr $ dummyInstr
          , 11 ?- SomeInstr $ T.MUL @'T.TInt @'T.TInt
          , 12 ?- SomeInstr $ T.MUL @'T.TInt @'T.TInt
          , 14 ?- SomeInstr $ dummyInstr
          , 15 ?- SomeInstr $ T.NIL @'T.TOperation
          , 16 ?- SomeInstr $ T.PAIR @_ @_ @'T.TOperation @'T.TInt
          ]

        ,
          [ 0  ?- unknownSrcPos
          , 2  ?- unknownSrcPos
          , 4  ?- SrcPos (Pos 1) (Pos 11)
          , 5  ?- unknownSrcPos
          , 11 ?- SrcPos (Pos 2) (Pos 11)
          , 12 ?- SrcPos (Pos 2) (Pos 11)  -- range end changed, but start - not
          , 14 ?- unknownSrcPos
          , 15 ?- SrcPos (Pos 3) (Pos 3)
          , 16 ?- SrcPos (Pos 3) (Pos 3)
          ]
        )
  ]
