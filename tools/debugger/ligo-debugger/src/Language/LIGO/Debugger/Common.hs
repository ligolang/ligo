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
  , internalStackFrameName
  , embedFunctionNames
  , embedFunctionNameIntoLambda
  , getLambdaMeta
  , refineStack
  , ligoRangeToRange
  , rangeToLigoRange
  ) where

import Unsafe qualified

import AST (Expr, LIGO)
import AST qualified
import Data.Default (def)
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty (cons)
import Data.Set qualified as Set
import Data.Vinyl (Rec (RNil, (:&)))
import Duplo (layer, leq, spineTo)
import Fmt (Buildable (..), pretty)
import Parser (Info)
import Product (Contains)
import Range (Range (..), getRange, point)
import Text.Interpolation.Nyan

import Morley.Debugger.Core.Navigate (SourceLocation (..))
import Morley.Debugger.Core.Snapshots (SourceType (..))
import Morley.Michelson.ErrorPos (Pos (..), SrcPos (..))
import Morley.Michelson.Interpret (StkEl (StkEl, seValue))
import Morley.Michelson.Parser (utypeQ)
import Morley.Michelson.Text (MText)
import Morley.Michelson.Typed
  (EpAddress (..), Instr, RemFail, SomeConstrainedValue (SomeValue), SomeValue, T (TLambda), Value,
  Value' (..), rfAnyInstr, rfMapAnyInstr, withValueTypeSanity)
import Morley.Michelson.Typed qualified as T
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
  SourceLocation (SourcePath lrFile) (ligoPositionToSrcPos lrStart)

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
    sourceLocationToRange (SourceLocation (SourcePath file) srcPos) =
      pointWithoutFile
        { _rFile = file
        }
      where
        SrcPos (Pos l) (Pos c) = srcPos
        pointWithoutFile = point (Unsafe.fromIntegral $ l + 1) (Unsafe.fromIntegral $ c + 1)
    sourceLocationToRange loc = error [int||Got source location with Lorentz source #{loc}|]

    rangeToSourceLocation :: Range -> SourceLocation
    rangeToSourceLocation Range{..} =
      SourceLocation
        (SourcePath _rFile) $
        SrcPos
          (Pos $ Unsafe.fromIntegral $ l - 1)
          (Pos $ Unsafe.fromIntegral $ c - 1)
      where
        (l, c, _) = _rStart

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

embedFunctionNameIntoLambda
  :: Maybe (LigoVariable 'Unique)
  -> Value t
  -> Value t
embedFunctionNameIntoLambda mVar (VLam rf) = VLam $ embedIntoRemFail rf
  where
    embedIntoRemFail :: RemFail Instr i o -> RemFail Instr i o
    embedIntoRemFail = rfMapAnyInstr \case
      T.ConcreteMeta (lambdaMeta :: LambdaMeta) instr ->
        let
          LigoVariable topLambdaName :| others = lambdaMeta ^. lmVariablesL

          updatedMeta =
            lambdaMeta
              & lmVariablesL %~
                  if | topLambdaName `compareUniqueNames` Name internalStackFrameName -> const (lambdaVar :| others)
                      | lambdaName `compareUniqueNames` topLambdaName -> id
                      | otherwise -> cons lambdaVar

        in T.Meta (T.SomeMeta updatedMeta) instr

      instr -> T.Meta (T.SomeMeta $ LambdaMeta (lambdaVar :| [])) instr

    lambdaVar@(LigoVariable lambdaName) = fromMaybe (LigoVariable (Name internalStackFrameName)) mVar
embedFunctionNameIntoLambda _ val = val

tryToEmbedEnvIntoLambda :: (LigoStackEntry 'Unique, StkEl t) -> StkEl t
tryToEmbedEnvIntoLambda (LigoStackEntry LigoExposedStackEntry{..}, stkEl@(StkEl val)) =
  case leseType of
    LTArrow{} -> StkEl $ embedFunctionNameIntoLambda leseDeclaration val
    _ -> stkEl
tryToEmbedEnvIntoLambda (_, stkEl) = stkEl

embedFunctionNames :: Rec StkEl t -> LigoStack 'Unique -> Rec StkEl t
embedFunctionNames (x :& xs) (y : ys) = tryToEmbedEnvIntoLambda (y, x) :& embedFunctionNames xs ys
embedFunctionNames stack [] = stack
embedFunctionNames RNil _ = RNil

getLambdaMeta :: Value ('TLambda i o) -> LambdaMeta
getLambdaMeta (VLam (rfAnyInstr -> instr)) =
  case instr of
    T.ConcreteMeta meta _ -> meta
    _ -> def

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
