module Language.LIGO.Debugger.Michelson
  ( dummyMapper
  , ligoMapper
  , fromIndexedExpression
  ) where

import Data.Char (isAsciiUpper, isDigit)
import Data.Coerce (coerce)
import Data.Default (def)
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import qualified Data.Text as Text
import Morley.Debugger.Core.Navigate (SourceLocation (..), SourceMapper (..), SourceType (..))
import Morley.Debugger.Core.Snapshots (InstrNo (..), annotateInstrWith)
import Morley.Micheline.Class (FromExpressionError, fromExpression)
import Morley.Micheline.Expression (Expression (..), MichelinePrimAp (..), MichelinePrimitive (..),
                                    michelsonPrimitive)
import Morley.Michelson.ErrorPos (srcPos)
import Morley.Michelson.TypeCheck (TCError, mapSomeContract, typeCheckContract, typeCheckingWith)
import Morley.Michelson.Typed (SomeContract (..))

dummyMapper :: SourceMapper
dummyMapper = SourceMapper
  { _smLocs = IntMap.fromDistinctAscList
    [ (3, mkLoc 1 11)
    , (5, mkLoc 2  3)
    --, (7, mkLoc 2  3)
    , (8, mkLoc 2  2)
    ]
    --[ (1, mkLoc 0 13)  -- CDR
    --, (3, mkLoc 1 15)  -- PUSH int 42
    --, (5, mkLoc 1 11)  -- ADD
    --, (7, mkLoc 2  4)  -- NIL operation
    --, (8, mkLoc 2  2)  -- PAIR
    --]
  }
  where
    mkLoc :: Word -> Word -> SourceLocation
    mkLoc = SourceLocation (SourcePath "/home/heitor/Serokell/ligo/tools/debugger/src-mapper/noop.mligo") ... srcPos

data LigoSourceMapper = LigoSourceMapper
  { _lsmMichelson    :: Expression
  , _lsmSourceMapper :: ()  -- TODO
  }

ligoMapper :: LigoSourceMapper -> Either DecodeError SourceMapper
ligoMapper LigoSourceMapper {..} = do
  _contr <- fromIndexedExpression _lsmMichelson
  pure SourceMapper
    { _smLocs = mempty -- TODO
    }

-- | Extracts all instruction numbers from the given expression.
extractInstructionsIndexes :: Expression -> [InstrNo]
-- We drop the head since we are not interested in the initial seq.
extractInstructionsIndexes = drop 1 . flip evalState 0 . go
  where
    go :: Expression -> State Int [InstrNo]
    go = \case
      ExpressionInt _ -> skip
      ExpressionString _ -> skip
      ExpressionBytes _ -> skip
      ExpressionSeq exprs -> addFold exprs
      ExpressionPrim MichelinePrimAp {mpaPrim, mpaArgs}
        | Set.member (coerce mpaPrim) primInstrs -> addFold mpaArgs
        | otherwise -> modify (+ 1) *> (fold <$> traverse go mpaArgs)

    skip :: State Int [InstrNo]
    skip = mempty <$ modify (+ 1)

    addFold :: [Expression] -> State Int [InstrNo]
    addFold exprs = do
      index <- get
      put $ index + 1
      (InstrNo index :) . fold <$> traverse go exprs

    prims, primInstrs :: Set Text
    primInstrs = Set.filter (Text.all (\c -> isAsciiUpper c || isDigit c || c == '_')) prims
    prims = Set.fromList $ toList michelsonPrimitive

data DecodeError
  = FromExpressionFailed FromExpressionError
  | TypeCheckFailed TCError

fromExpressionToTyped
  :: Expression
  -> Either DecodeError SomeContract
fromExpressionToTyped expr = do
  uContract <- first FromExpressionFailed $ fromExpression expr
  first TypeCheckFailed $ typeCheckingWith def $ typeCheckContract uContract

fromIndexedExpression
  :: Expression
  -> Either DecodeError SomeContract
fromIndexedExpression expr =
  mapSomeContract (annotateInstrWith $ extractInstructionsIndexes expr)
    <$> fromExpressionToTyped expr
