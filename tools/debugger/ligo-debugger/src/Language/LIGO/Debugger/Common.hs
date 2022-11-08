-- | Common stuff for debugger.
module Language.LIGO.Debugger.Common
  ( EmbeddedLigoMeta
  , ligoPositionToSrcPos
  , ligoRangeToSourceLocation
  , spineAtPoint
  , getStatementLocs
  , isLigoStdLib
  , errorValueType
  , createErrorValue
  , ReplacementException(..)
  , replacementErrorValueToException
  , refineStack
  , ligoRangeToRange
  , rangeToLigoRange
  , shouldIgnoreMeta
  ) where

import Unsafe qualified

import AST (Expr, LIGO)
import AST qualified
import Data.HashMap.Strict qualified as HM
import Data.Set qualified as Set
import Data.Vinyl (Rec (RNil, (:&)))
import Duplo (layer, leq, spineTo)
import Fmt (Buildable (..), pretty)
import Parser (Info)
import Product (Contains)
import Range (Range (..), getRange)
import Text.Interpolation.Nyan

import Morley.Debugger.Core.Navigate (SourceLocation (..))
import Morley.Debugger.Core.Snapshots (SourceType (..))
import Morley.Michelson.ErrorPos (Pos (..), SrcPos (..))
import Morley.Michelson.Interpret (StkEl (seValue))
import Morley.Michelson.Parser (utypeQ)
import Morley.Michelson.Text (MText)
import Morley.Michelson.Typed
  (EpAddress (..), Instr (LAMBDA, PUSH), SomeConstrainedValue (SomeValue), SomeValue, Value,
  Value' (..), withValueTypeSanity)
import Morley.Michelson.Untyped qualified as U
import Morley.Tezos.Address (Address, mformatAddress, ta)

import Language.LIGO.Debugger.CLI.Types
import Language.LIGO.Debugger.Error

-- | Type of meta that we embed in Michelson contract to later use it
-- in debugging.
type EmbeddedLigoMeta = LigoIndexedInfo 'Unique

ligoPositionToSrcPos :: HasCallStack => LigoPosition -> SrcPos
ligoPositionToSrcPos (LigoPosition l c) =
  SrcPos
    (Pos $ Unsafe.fromIntegral (toInteger l - 1))
    (Pos $ Unsafe.fromIntegral c)

ligoRangeToSourceLocation :: HasCallStack => LigoRange -> SourceLocation
ligoRangeToSourceLocation LigoRange{..} =
  SourceLocation (SourcePath lrFile) (ligoPositionToSrcPos lrStart) (ligoPositionToSrcPos lrEnd)

-- | Returns all nodes which cover given range
-- ordered from the most local to the least local.
spineAtPoint
  :: Contains Range xs
  => Range -> LIGO xs -> [LIGO xs]
spineAtPoint pos = spineTo (\i -> pos `leq` getRange i)

getStatementLocs :: HasCallStack => Set SourceLocation -> HashMap FilePath (LIGO Info) -> Set SourceLocation
getStatementLocs locs parsedContracts =
  ranges
    <&> getStatementRanges
    & concat
    <&> rangeToSourceLocation
    & Set.fromList
  where
    sourceLocationToRange :: SourceLocation -> Range
    sourceLocationToRange (SourceLocation (SourcePath file) startPos endPos) = Range
      { _rStart = posToTuple startPos
      , _rFinish = posToTuple endPos
      , _rFile = file
      }
      where
        posToTuple :: Integral i => SrcPos -> (i, i, i)
        posToTuple (SrcPos (Pos l) (Pos c)) =
          (Unsafe.fromIntegral (l + 1), Unsafe.fromIntegral (c + 1), 0)

    sourceLocationToRange loc = error [int||Got source location with Lorentz source #{loc}|]

    rangeToSourceLocation :: Range -> SourceLocation
    rangeToSourceLocation Range{..} =
      SourceLocation
        (SourcePath _rFile)
        (tupleToPos _rStart)
        (tupleToPos _rFinish)
      where
        tupleToPos :: Integral i => (i, i, i) -> SrcPos
        tupleToPos (l, c, _) = SrcPos
          (Pos $ Unsafe.fromIntegral $ l - 1)
          (Pos $ Unsafe.fromIntegral $ c - 1)

    ranges = toList locs
      <&> sourceLocationToRange
      & filter (not . isLigoStdLib . _rFile)

    getStatementRanges :: Range -> [Range]
    getStatementRanges range@Range{..} = filterStatements $ spineAtPoint range parsedLigo
      where
        parsedLigo =
          fromMaybe
            (error [int||Can't find parsed ligo with filename #{_rFile}|])
            (parsedContracts HM.!? _rFile)

        -- TODO: refactor it somehow to avoid copypaste.
        --
        -- We can use here @Unsafe.head@ because this filtering won't
        -- accept empty list.
        filterStatements :: [LIGO Info] -> [Range]
        filterStatements = fmap (getRange . Unsafe.head) . filter worthPicking . tails
          where
            worthPicking = \case
              (layer -> Just AST.Assign{}) : _ -> True
              (layer -> Just AST.BConst{}) : _ -> True
              (layer -> Just AST.BVar{}) : _ -> True
              (layer -> Just AST.Apply{}) : (layer @Expr -> Just ctor) : _ -> isApplyExpr ctor
              _ -> False

            -- We treat @Apply@ as an expression if only and only
            -- it contains in @Let@ or @Seq@ nodes.
            isApplyExpr = \case
              AST.Let{} -> True
              AST.Seq{} -> True
              _ -> False

-- | LIGO debug output, when optimizations are disabled, may mention locations
-- referring to LIGO's standard library that defines bindings to every single
-- Michelson instruction.
-- LIGO teams says that we should just ignore such locations.
isLigoStdLib :: FilePath -> Bool
isLigoStdLib path =
  path == ""

errorValueType :: U.Ty
errorValueType = [utypeQ|pair address string|]

createErrorValue :: MText -> U.Value
createErrorValue errMsg =
  U.ValuePair (U.ValueString addrText) (U.ValueString errMsg)
  where
    addrText = mformatAddress errorAddress

errorAddress :: Address
errorAddress = [ta|tz1fakefakefakefakefakefakefakcphLA5|]

-- | Something was found to be wrong after replacing Michelson code
-- in 'preprocessContract'.
newtype ReplacementException = ReplacementException MText
  deriving newtype (Show, Buildable)

instance Exception ReplacementException where
  displayException = pretty

instance DebuggerException ReplacementException where
  type ExceptionTag ReplacementException = "Replacement"
  debuggerExceptionType _ = MidLigoLayerException

-- | We're replacing some @Michelson@ instructions.
-- Sometimes we perform unsafe unwrapping in the replaced code.
-- In failure case we're doing something like @{ PUSH errValue; FAILWITH }@
-- where @errValue@ has @address@ type. The entrypoint in this address
-- contains the error message.
replacementErrorValueToException :: Value t -> Maybe ReplacementException
replacementErrorValueToException = \case
  (VPair (VAddress (EpAddress addr _), VString errMsg)) | addr == errorAddress -> do
    pure $ ReplacementException errMsg
  _ -> Nothing

stkElValue :: StkEl v -> SomeValue
stkElValue stkEl = let v = seValue stkEl in withValueTypeSanity v (SomeValue v)

-- | Leave only information that matters in LIGO.
refineStack :: Rec StkEl st -> [SomeValue]
refineStack =
  -- Note: it is important for this function to be lazy if we don't
  -- want to have full copy of stack skeleton (which is sequence of `:&`)
  -- in each snapshot, that would take O(snapshots num * avg stack size) memory.
  --
  -- And 'Rec' is strict datatype, so using functions like 'rmap' would not
  -- fit our purpose.
  \case
    RNil -> []
    stkEl :& st -> stkElValue stkEl : refineStack st

ligoRangeToRange :: LigoRange -> Range
ligoRangeToRange LigoRange{..} = Range
  { _rStart = toPosition lrStart
  , _rFinish = toPosition lrEnd
  , _rFile = lrFile
  }
  where
    toPosition LigoPosition{..} = (Unsafe.fromIntegral lpLine, Unsafe.fromIntegral $ lpCol + 1, 0)

rangeToLigoRange :: Range -> LigoRange
rangeToLigoRange Range{..} = LigoRange
  { lrStart = toLigoPosition _rStart
  , lrEnd = toLigoPosition _rFinish
  , lrFile = _rFile
  }
  where
    toLigoPosition (line, col, _) = LigoPosition (Unsafe.fromIntegral line) (Unsafe.fromIntegral $ col - 1)

-- | Sometimes we want to ignore metas for some instructions.
shouldIgnoreMeta :: Instr i o -> Bool
shouldIgnoreMeta = \case
  -- We're ignoring @LAMBDA@ instruction here in order
  -- not to stop on function assignment.
  LAMBDA{} -> True

  -- @PUSH@es have location metas that point to constants.
  -- E.g. @PUSH int 42@ may have a location of @42@.
  --
  -- So, stopping at them and showing an evaluation
  -- seems useless. I see that @42@ evaluates to @42@
  -- without any debug info.
  PUSH{} -> True
  _ -> False
