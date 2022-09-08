module Language.LIGO.Debugger.Michelson
  ( DecodeError (..)
  , EmbedError
  , readLigoMapper
  ) where

import Unsafe qualified

import Control.Lens (each)
import Control.Lens.Prism (_Just)
import Control.Monad.Except (Except, runExcept, throwError)
import Data.Char (isAsciiUpper, isDigit)
import Data.Coerce (coerce)
import Data.DList qualified as DL
import Data.Data (cast)
import Data.Default (def)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Vector qualified as V
import Fmt (Buildable (..), Builder, genericF)
import Morley.Debugger.Core.Common (debuggerTcOptions)
import Morley.Debugger.Core.Navigate (SourceLocation (..))
import Morley.Micheline.Class (FromExpressionError, fromExpression)
import Morley.Micheline.Expression
  (Exp (..), Expression, MichelinePrimAp (..), MichelinePrimitive (..), michelsonPrimitive)
import Morley.Michelson.TypeCheck (TCError, typeCheckContract, typeCheckingWith)
import Morley.Michelson.Typed
  (Contract' (..), ContractCode' (ContractCode, unContractCode), CtorEffectsApp (..),
  DfsSettings (..), Instr (..), SomeContract (..), SomeMeta (SomeMeta), dfsFoldInstr,
  dfsTraverseInstr, isMichelsonInstr)
import Text.Interpolation.Nyan

import Language.LIGO.Debugger.CLI.Types
import Language.LIGO.Debugger.Common

-- | When it comes to information attached to entries in Michelson code,
-- so-called table encoding stands for representing that info in a list
-- in the order of DFS traversal over Micheline tree.
--
-- This type stands for index in such list, i.e. it is number of the
-- Micheline node that we will visit if we go with DFS.
newtype TableEncodingIdx = TableEncodingIdx { unTableEncodingIdx :: Int }
  deriving stock (Show, Eq, Ord)
  deriving newtype (Buildable)

-- | Enumerates all the Micheline nodes starting from 0 and
-- returns only those indices that correspond to actual instructions.
extractInstructionsIndexes :: Expression -> [TableEncodingIdx]
extractInstructionsIndexes =
  -- We drop the head since we are not interested in the initial seq.
  -- Why dropping the second element - is yet a mistery
  drop 2 . evaluatingState 0 . go
  where
    go :: Expression -> State Int [TableEncodingIdx]
    go = \case
      ExpInt _ _ -> skip
      ExpString _ _ -> skip
      ExpBytes _ _ -> skip
      ExpSeq _ exprs -> addFold exprs
      ExpPrim _ MichelinePrimAp {mpaPrim, mpaArgs}
        | Set.member (coerce mpaPrim) primInstrs -> addFold mpaArgs
        | otherwise -> modify (+ 1) *> (fold <$> traverse go mpaArgs)

    skip :: State Int [TableEncodingIdx]
    skip = mempty <$ modify (+ 1)

    addFold :: [Expression] -> State Int [TableEncodingIdx]
    addFold exprs = do
      index <- get
      put $ index + 1
      (TableEncodingIdx index :) . fold <$> traverse go exprs

    prims, primInstrs :: Set Text
    primInstrs = Set.filter (Text.all (\c -> isAsciiUpper c || isDigit c || c == '_')) prims
    prims = Set.fromList $ toList michelsonPrimitive

data DecodeError
  = FromExpressionFailed FromExpressionError
  | TypeCheckFailed TCError
  | InsufficientMeta TableEncodingIdx
  | MetaEmbeddingError EmbedError
  deriving stock (Eq, Generic)

instance Buildable DecodeError where
  build = genericF

fromExpressionToTyped
  :: Expression
  -> Either DecodeError SomeContract
fromExpressionToTyped expr = do
  uContract <- first FromExpressionFailed $ fromExpression expr
  first TypeCheckFailed $ typeCheckingWith debuggerTcOptions $ typeCheckContract uContract

-- Using proper content in this type is too inconvenient at the moment
data EmbedError
  = RemainingExtraEntries Word
  | InsufficientEntries Builder
  deriving stock (Show, Eq)

instance Buildable EmbedError where
  build = \case
    RemainingExtraEntries num ->
      [int||Too many debug entries left: #s{num}|]
    InsufficientEntries msg -> build msg

-- | Embed data into typed instructions visiting them in DFS order.
embedInInstr
  :: forall meta inp out.
     (Show meta, Typeable meta, NFData meta)
  => [meta]
  -> Instr inp out
  -> Either EmbedError (Instr inp out)
embedInInstr metaTape instr = do
  (resInstr, tapeRest) <- runExcept $ usingStateT metaTape $
    dfsTraverseInstr def{ dsGoToValues = True, dsCtorEffectsApp = recursionImpl } instr
  unless (null tapeRest) $
    Left $ RemainingExtraEntries (Unsafe.fromIntegral @Int @Word $ length tapeRest)
  return resInstr
  where
    isActualInstr = \case
      Seq{} -> False
      i -> isMichelsonInstr i

    -- Sometimes we want to ignore embeding meta for some instructions.
    shouldIgnoreMeta :: Instr i o -> Bool
    shouldIgnoreMeta = \case
      -- We're ignoring @LAMBDA@ instruction here in order
      -- not to stop on function assignment.
      LAMBDA{} -> True
      _ -> False

    recursionImpl :: CtorEffectsApp $ StateT [meta] $ Except EmbedError
    recursionImpl = CtorEffectsApp "embed" $ \oldInstr mkNewInstr ->
      if not $ isActualInstr oldInstr
      then mkNewInstr
      else get >>= \case
        [] -> throwError . InsufficientEntries $
          [int||Insufficient number of entries, broke at #{oldInstr}|]
        (meta : rest) -> do
          -- We have to skip several metas due to difference between typed
          -- representation and Micheline.
          -- In Micheline every Seq is a separate node that has a corresponding
          -- meta, and in our typed representation we tend to avoid 'Nested'
          -- wrapper where Michelson's @{ }@ are mandatory.
          -- I.e. @IF ADD (SWAP # SUB)@ in typed representation corresponds to
          -- @Prim "IF" [ [Prim "ADD"], [Prim "SWAP", Prim "SUB"] ]@ in Micheline
          -- and we have to account for these inner @[]@ manually.
          let metasToDrop = michelsonInstrInnerBranches oldInstr
          put $ drop (Unsafe.fromIntegral @Word @Int metasToDrop) rest

          if shouldIgnoreMeta oldInstr
          then mkNewInstr
          else Meta (SomeMeta meta) <$> mkNewInstr

-- TODO: extract this to Morley
-- | For Michelson instructions this returns how many sub-instructions this
-- instruction directly contains. For non-Michelson instructions this returns 1.
michelsonInstrInnerBranches :: Instr i o -> Word
michelsonInstrInnerBranches = \case
  IF{} -> 2
  IF_NONE{} -> 2
  IF_LEFT{} -> 2
  IF_CONS{} -> 2

  LOOP{} -> 1
  LOOP_LEFT{} -> 1

  MAP{} -> 1
  ITER{} -> 1

  DIP{} -> 1
  DIPN{} -> 1

  LAMBDA{} -> 1

  _ -> 0

-- | Read LIGO's debug output and produce
--
-- 1. All locations that may be worth attention. This is to be used
--    in switching breakpoints.
-- 2. A contract with inserted @Meta (SomeMeta (info :: 'EmbeddedLigoMeta'))@
--    wrappers that carry the debug info.
-- 3. All contract filepaths that would be used in debugging session.
readLigoMapper
  :: LigoMapper
  -> Either DecodeError (Set SourceLocation, SomeContract, [FilePath])
readLigoMapper ligoMapper = do
  let indexes :: [TableEncodingIdx] =
        extractInstructionsIndexes (lmMichelsonCode ligoMapper)
  metaPerInstr :: [LigoIndexedInfo] <-
    forM indexes \i ->
      maybe (Left $ InsufficientMeta i) pure $
        lmLocations ligoMapper V.!? unTableEncodingIdx i

  SomeContract contract <- fromExpressionToTyped (lmMichelsonCode ligoMapper)
  extendedContract@(SomeContract extContract) <- first MetaEmbeddingError $
    (\code -> SomeContract contract{ cCode = ContractCode code }) <$>
      embedInInstr @EmbeddedLigoMeta
        metaPerInstr
        (unContractCode $ cCode contract)

  let allFiles = metaPerInstr ^.. each . liiLocationL . _Just . lrFileL
        -- We want to remove duplicates
        & unstableNub
        & filter (/= "")

  let exprLocs =
        -- We expect a lot of duplicates, stripping them via putting to Set
        Set.fromList $
        foldMap mentionedSourceLocs $ getSourceLocations (unContractCode $ cCode extContract)

  -- The LIGO's debug info may be really large, so we better force
  -- the evaluation for all the info that will be stored for the entire
  -- debug session, and let GC wipe out everything intermediate.
  return $! force (exprLocs, extendedContract, allFiles)

  where
    mentionedSourceLocs :: LigoIndexedInfo -> [SourceLocation]
    mentionedSourceLocs LigoIndexedInfo{..} =
      maybeToList $ ligoRangeToSourceLocation <$> liiLocation

    getSourceLocations :: Instr i o -> [EmbeddedLigoMeta]
    getSourceLocations = DL.toList . dfsFoldInstr def { dsGoToValues = True } \case
      Meta (SomeMeta (cast -> Just (meta :: EmbeddedLigoMeta))) _ -> DL.singleton meta
      _ -> mempty
