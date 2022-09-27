module Language.LIGO.Debugger.Michelson
  ( DecodeError (..)
  , EmbedError
  , readLigoMapper
  ) where

import Unsafe qualified

import Control.Lens (at, each)
import Control.Lens.Prism (_Just)
import Control.Monad.Except (Except, liftEither, runExcept, throwError)
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
import Morley.Michelson.Untyped qualified as U
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
  | PreprocessError PreprocessError
  deriving stock (Eq, Generic)

instance Buildable DecodeError where
  build = \case
    TypeCheckFailed err ->
      [int||
        Something went wrong: LIGO executable produced \
        badly typed Michelson contract. Please contact us.
        #{err}
      |]
    decodeError -> genericF decodeError

fromExpressionToTyped
  :: Expression
  -> Either DecodeError SomeContract
fromExpressionToTyped expr = do
  uContract <- first FromExpressionFailed $ fromExpression expr
  processedUContract <- first PreprocessError $ preprocessContract uContract
  first TypeCheckFailed $ typeCheckingWith debuggerTcOptions $ typeCheckContract processedUContract

newtype PreprocessError
  = EntrypointTypeNotFound U.EpName
  deriving stock (Eq, Generic)

instance Buildable PreprocessError where
  build = \case
    EntrypointTypeNotFound epName -> [int||Type for entrypoint #{epName} not found|]

type PreprocessMonad = ReaderT (Map U.EpName U.Ty) (Except PreprocessError)

-- | Since optimization disabling feature in @ligo@ can produce
-- badly typed Michelson code, we should fix it somehow. At this
-- moment we're doing the next things:
-- 1. Replacing @SELF %ep@ instr inside lambda with @{ SELF_ADDRESS; CONTRACT %ep ty }@
-- 2. Replacing all @BIG_MAP@ occurrences with @MAP@.
--    We're doing this by replacing all big-map's @Ty@ to map ones
--    and replacing @EMPTY_BIG_MAP@ instr to @EMPTY_MAP@.
preprocessContract :: U.Contract -> Either PreprocessError U.Contract
preprocessContract con@U.Contract{..} =
  let
    ctx = U.mkEntrypointsMap contractParameter
    mappedOpsInMonad = mapM (go False) contractCode
    mappedOpsE = runExcept $ runReaderT mappedOpsInMonad ctx
  in (\ops -> con { U.contractCode = ops }) <$> mappedOpsE
  where
    go :: Bool -> U.ExpandedOp -> PreprocessMonad U.ExpandedOp
    go insideLambda = \case
      prim@(U.PrimEx instr) -> case instr of
        U.PUSH varAnn ty value -> do
          processedValue <- preprocessValue value
          pure $ U.PrimEx $ U.PUSH varAnn (preprocessTy ty) processedValue
        U.NONE typeAnn varAnn ty -> pure $ U.PrimEx $ U.NONE typeAnn varAnn (preprocessTy ty)
        U.IF_NONE ops1 ops2 -> do
          mappedOps1 <- mapM (go insideLambda) ops1
          mappedOps2 <- mapM (go insideLambda) ops2
          pure $ U.PrimEx $ U.IF_NONE mappedOps1 mappedOps2
        U.LEFT typeAnn varAnn fstFieldAnn sndFieldAnn ty ->
          pure $ U.PrimEx $ U.LEFT typeAnn varAnn fstFieldAnn sndFieldAnn (preprocessTy ty)
        U.RIGHT typeAnn varAnn fstFieldAnn sndFieldAnn ty ->
          pure $ U.PrimEx $ U.RIGHT typeAnn varAnn fstFieldAnn sndFieldAnn (preprocessTy ty)
        U.IF_LEFT ops1 ops2 -> do
          mappedOps1 <- mapM (go insideLambda) ops1
          mappedOps2 <- mapM (go insideLambda) ops2
          pure $ U.PrimEx $ U.IF_LEFT mappedOps1 mappedOps2
        U.NIL typeAnn varAnn ty -> pure $ U.PrimEx $ U.NIL typeAnn varAnn (preprocessTy ty)
        U.IF_CONS ops1 ops2 -> do
          mappedOps1 <- mapM (go insideLambda) ops1
          mappedOps2 <- mapM (go insideLambda) ops2
          pure $ U.PrimEx $ U.IF_CONS mappedOps1 mappedOps2
        U.EMPTY_SET typeAnn varAnn ty -> pure $ U.PrimEx $ U.EMPTY_SET typeAnn varAnn (preprocessTy ty)
        U.EMPTY_MAP typeAnn varAnn tyKeys tyValues ->
          pure $ U.PrimEx $ U.EMPTY_MAP typeAnn varAnn (preprocessTy tyKeys) (preprocessTy tyValues)
        U.EMPTY_BIG_MAP typeAnn varAnn tyKeys tyValues ->
          pure $ U.PrimEx $ U.EMPTY_MAP typeAnn varAnn (preprocessTy tyKeys) (preprocessTy tyValues)
        U.MAP varAnn ops -> do
          processedOps <- mapM (go insideLambda) ops
          pure $ U.PrimEx $ U.MAP varAnn processedOps
        U.ITER ops -> do
          processedOps <- mapM (go insideLambda) ops
          pure $ U.PrimEx $ U.ITER processedOps
        U.IF ops1 ops2 -> do
          mappedOps1 <- mapM (go insideLambda) ops1
          mappedOps2 <- mapM (go insideLambda) ops2
          pure $ U.PrimEx $ U.IF mappedOps1 mappedOps2
        U.LOOP ops -> do
          processedOps <- mapM (go insideLambda) ops
          pure $ U.PrimEx $ U.LOOP processedOps
        U.LOOP_LEFT ops -> do
          processedOps <- mapM (go insideLambda) ops
          pure $ U.PrimEx $ U.LOOP_LEFT processedOps
        U.LAMBDA varAnn tyIn tyOut ops -> do
          processedOps <- mapM (go True) ops
          pure $ U.PrimEx $ U.LAMBDA varAnn (preprocessTy tyIn) (preprocessTy tyOut) processedOps
        U.DIP ops -> do
          processedOps <- mapM (go insideLambda) ops
          pure $ U.PrimEx $ U.DIP processedOps
        U.DIPN n ops -> do
          processedOps <- mapM (go insideLambda) ops
          pure $ U.PrimEx $ U.DIPN n processedOps
        U.CAST varAnn ty -> pure $ U.PrimEx $ U.CAST varAnn (preprocessTy ty)
        U.UNPACK typeAnn varAnn ty -> pure $ U.PrimEx $ U.UNPACK typeAnn varAnn (preprocessTy ty)
        U.VIEW varAnn viewName ty -> pure $ U.PrimEx $ U.VIEW varAnn viewName (preprocessTy ty)
        U.SELF varAnn fieldAnn
          | insideLambda -> do
              let epName = U.epNameFromSelfAnn fieldAnn
              ty <-
                if U.isDefEpName epName
                then do
                  let (U.ParameterType ty' _) = contractParameter
                  pure ty'
                else do
                  tyMb <- view $ at epName
                  maybe
                    do throwError $ EntrypointTypeNotFound epName
                    pure
                    tyMb
              pure $
                U.SeqEx $ U.PrimEx <$>
                  [ U.SELF_ADDRESS varAnn
                  , U.CONTRACT varAnn fieldAnn (preprocessTy ty)
                  , U.IF_NONE
                      do
                        U.PrimEx <$>
                          [ U.UNIT def def
                          , U.FAILWITH
                          ]
                      do []
                  ]
          | otherwise -> pure $ U.PrimEx $ U.SELF varAnn fieldAnn
        U.CONTRACT varAnn fieldAnn ty -> pure $ U.PrimEx $ U.CONTRACT varAnn fieldAnn (preprocessTy ty)
        U.CREATE_CONTRACT varAnn1 varAnn2 contract -> do
          processedContract <- liftEither $ preprocessContract contract
          pure $ U.PrimEx $ U.CREATE_CONTRACT varAnn1 varAnn2 processedContract
        _ -> pure prim
      U.SeqEx expandedOps -> U.SeqEx <$> mapM (go insideLambda) expandedOps
      U.WithSrcEx errorSrcPos expanedOp -> do
        processedExpandedOp <- go insideLambda expanedOp
        pure $ U.WithSrcEx errorSrcPos processedExpandedOp
      where
        preprocessTy :: U.Ty -> U.Ty
        preprocessTy oldTy@(U.Ty typ ann) = case typ of
          U.TOption ty -> U.Ty (U.TOption $ preprocessTy ty) ann
          U.TList ty -> U.Ty (U.TList $ preprocessTy ty) ann
          U.TSet ty -> U.Ty (U.TSet $ preprocessTy ty) ann
          U.TContract ty -> U.Ty (U.TContract $ preprocessTy ty) ann
          U.TTicket ty -> U.Ty (U.TTicket $ preprocessTy ty) ann
          U.TPair fstFieldAnn sndFieldAnn fstVarAnn sndVarAnn tyLeft tyRight ->
            U.Ty (U.TPair fstFieldAnn sndFieldAnn fstVarAnn sndVarAnn (preprocessTy tyLeft) (preprocessTy tyRight)) ann
          U.TOr fstFieldAnn sndFieldAnn tyLeft tyRight ->
            U.Ty (U.TOr fstFieldAnn sndFieldAnn (preprocessTy tyLeft) (preprocessTy tyRight)) ann
          U.TLambda tyIn tyOut ->
            U.Ty (U.TLambda (preprocessTy tyIn) (preprocessTy tyOut)) ann
          U.TMap tyKey tyValue ->
            U.Ty (U.TMap (preprocessTy tyKey) (preprocessTy tyValue)) ann
          U.TBigMap tyKey tyValue ->
            U.Ty (U.TMap (preprocessTy tyKey) (preprocessTy tyValue)) ann
          _ -> oldTy

        preprocessValue :: U.Value -> PreprocessMonad U.Value
        preprocessValue = \case
          U.ValuePair valLeft valRight ->
            U.ValuePair <$> preprocessValue valLeft <*> preprocessValue valRight
          U.ValueLeft val -> U.ValueLeft <$> preprocessValue val
          U.ValueRight val -> U.ValueRight <$> preprocessValue val
          U.ValueSome val -> U.ValueSome <$> preprocessValue val
          U.ValueSeq vals -> U.ValueSeq <$> mapM preprocessValue vals
          U.ValueMap elts ->
            let mapElt (U.Elt val1 val2) = U.Elt <$> preprocessValue val1 <*> preprocessValue val2
            in U.ValueMap <$> mapM mapElt elts
          U.ValueLambda ops -> U.ValueLambda <$> mapM (go insideLambda) ops
          value -> pure value

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
        & filter (not . isLigoStdLib)

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
