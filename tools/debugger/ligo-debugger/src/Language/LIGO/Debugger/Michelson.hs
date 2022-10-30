module Language.LIGO.Debugger.Michelson
  ( DecodeError (..)
  , MichelsonDecodeException (..)
  , EmbedError (..)
  , PreprocessError (..)
  , typesReplaceRules
  , instrReplaceRules
  , readLigoMapper
  ) where

import Unsafe qualified

import Control.Lens (at, cons, each, (%=), (.=))
import Control.Lens.Prism (_Just)
import Control.Monad.Except (Except, liftEither, runExcept, throwError)
import Data.Char (isAsciiUpper, isDigit)
import Data.Coerce (coerce)
import Data.DList qualified as DL
import Data.Data (cast)
import Data.Default (Default, def)
import Data.Map qualified as M
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Vector qualified as V
import Fmt (Buildable (..), Builder, pretty)
import Generics.SYB (everywhere, everywhereM, mkM, mkT)
import Text.Interpolation.Nyan
import Text.Show qualified
import Util (everywhereM')

import Morley.Debugger.Core.Common (debuggerTcOptions)
import Morley.Debugger.Core.Navigate (SourceLocation (..))
import Morley.Micheline.Class (FromExpressionError, fromExpression)
import Morley.Micheline.Expression
  (Exp (..), Expression, MichelinePrimAp (..), MichelinePrimitive (..), michelsonPrimitive)
import Morley.Michelson.Text (mt)
import Morley.Michelson.TypeCheck
  (TCError (..), TCTypeError (..), typeCheckContract, typeCheckingWith)
import Morley.Michelson.Typed
  (BadTypeForScope (BtHasTicket), Contract' (..), ContractCode' (ContractCode, unContractCode),
  CtorEffectsApp (..), DfsSettings (..), Instr (..), SomeContract (..), SomeMeta (SomeMeta),
  dfsFoldInstr, dfsTraverseInstr, isMichelsonInstr)
import Morley.Michelson.Untyped qualified as U
import Morley.Util.Lens (makeLensesWith, postfixLFields)

import Language.LIGO.Debugger.CLI.Types
import Language.LIGO.Debugger.Common
import Language.LIGO.Debugger.Error

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

-- | In this state we store two lists with metas.
-- When we're processing one meta then we're taking it
-- from the first list and prepening it to the second one.
-- We're processing them in such manner because sometimes
-- we want to insert empty metas for replaced instr.
data PreprocessState meta = PreprocessState
  { psMetasIn :: [meta]
    -- ^ Metas to process.
  , psMetasOut :: [meta]
    -- ^ Processed metas (in reversed order).
  }

makeLensesWith postfixLFields ''PreprocessState

data DecodeError
  = FromExpressionFailed FromExpressionError
  | TypeCheckFailed TCError
  | InsufficientMeta TableEncodingIdx
  | MetaEmbeddingError EmbedError
  | PreprocessError PreprocessError
  deriving stock (Eq, Generic)

instance Buildable DecodeError where
  build = \case
    FromExpressionFailed err ->
      [int||Failed to parse Micheline expression: #{err}|]
    TypeCheckFailed err ->
      [int||Failed to typecheck the Michelson contract: #{err}|]
    InsufficientMeta idx ->
      [int||Not enough metadata, missing at index #{idx}|]
    MetaEmbeddingError err ->
      pretty err
    PreprocessError err ->
      pretty err

newtype MichelsonDecodeException = MichelsonDecodeException DecodeError
  deriving stock (Eq, Generic)
  deriving newtype Buildable

instance Show MichelsonDecodeException where
  show = pretty

instance Exception MichelsonDecodeException

instance DebuggerException MichelsonDecodeException where
  type ExceptionTag MichelsonDecodeException = "MichelsonDecode"
  debuggerExceptionType (MichelsonDecodeException err) = case err of
    FromExpressionFailed{} -> MidLigoLayerException
    TypeCheckFailed{} -> MidLigoLayerException
    InsufficientMeta{} -> MidLigoLayerException
    MetaEmbeddingError{} -> MidLigoLayerException
    PreprocessError err' -> case err' of
      EntrypointTypeNotFound{} -> UserException
      UnsupportedTicketDup -> UserException

wrapTypeCheckFailed :: TCError -> DecodeError
wrapTypeCheckFailed = \case
  TCFailedOnInstr _ _ _ _ (Just (UnsupportedTypeForScope _ BtHasTicket)) ->
    PreprocessError UnsupportedTicketDup
  other -> TypeCheckFailed other

fromExpressionToTyped
  :: (Default meta)
  => Expression
  -> [meta]
  -> (U.T -> U.T)
  -> (U.ExpandedInstr -> PreprocessMonad meta U.ExpandedOp)
  -> Either DecodeError (SomeContract, [meta])
fromExpressionToTyped expr metas typeRules instrRules = do
  uContract <- first FromExpressionFailed $ fromExpression expr
  (processedUContract, newMetas, oldMetas) <- first PreprocessError $ preprocessContract uContract metas typeRules instrRules
  contract <- first wrapTypeCheckFailed $ typeCheckingWith debuggerTcOptions $ typeCheckContract processedUContract
  pure (contract, reverse newMetas <> oldMetas)

data PreprocessError
  = EntrypointTypeNotFound U.EpName
  | UnsupportedTicketDup
  deriving stock (Eq, Generic)

instance Buildable PreprocessError where
  build = \case
    EntrypointTypeNotFound epName ->
      [int||
        SELF instruction have the entrypoint #{epName} \
        that the contract's parameter doesn't declare.
      |]
    UnsupportedTicketDup ->
      [int||
        Typechecking failed due to `DUP` being used on a non-dupable value
        (e.g. ticket). This case may not work in the debugger even if the
        contract actually compiles.
      |]

type PreprocessMonad meta =
  ReaderT (Map U.EpName U.Ty) $
  ExceptT PreprocessError $
  State (PreprocessState meta)

metasToProcess :: U.ExpandedInstr -> Int
metasToProcess = \case
  U.IF{} -> 3
  U.IF_NONE{} -> 3
  U.IF_LEFT{} -> 3
  U.IF_CONS{} -> 3

  U.LOOP{} -> 2
  U.LOOP_LEFT{} -> 2

  U.MAP{} -> 2
  U.ITER{} -> 2

  U.DIP{} -> 2
  U.DIPN{} -> 2

  U.LAMBDA{} -> 2

  _ -> 1

-- | This function may generate a wrong number of empty metas for
-- complex replacable instructions (e.g. @LAMBDA@, @IF@ or @LOOP@).
-- For all other instructions this function works correctly.
generateEmptyMetas :: forall meta. (Default meta) => U.ExpandedInstr -> U.ExpandedOp -> PreprocessMonad meta ()
generateEmptyMetas replacableInstr op = do
  generateEmptyMetasImpl op
  psMetasOutL %= drop (metasToProcess replacableInstr)
  where
    generateEmptyMetasImpl :: U.ExpandedOp -> PreprocessMonad meta ()
    generateEmptyMetasImpl = \case
      U.PrimEx instr -> do
        replicateM_ (metasToProcess instr) insertEmptyMeta
        traverseOps instr
      U.SeqEx ops -> insertEmptyMeta >> mapM_ generateEmptyMetasImpl ops
      U.WithSrcEx _ op' -> generateEmptyMetasImpl op'
      where
        traverseOps :: U.ExpandedInstr -> PreprocessMonad meta ()
        traverseOps =
          void . everywhereM (mkM \op' -> generateEmptyMetasImpl op' >> pure op')

    insertEmptyMeta :: PreprocessMonad meta ()
    insertEmptyMeta = do
      psMetasOutL %= cons def

instrReplaceRules :: (Default meta) => U.ExpandedInstr -> PreprocessMonad meta U.ExpandedOp
instrReplaceRules = \case
  U.EMPTY_BIG_MAP typeAnn varAnn tyKey tyValue ->
    pure $ U.PrimEx $ U.EMPTY_MAP typeAnn varAnn tyKey tyValue
  selfInstr@(U.SELF varAnn fieldAnn) -> do
    let epName = U.epNameFromSelfAnn fieldAnn
    ty <- do
      tyMb <- view $ at epName
      maybe
        do throwError $ EntrypointTypeNotFound epName
        pure
        tyMb

    let errorValue =
          createErrorValue [mt|Cannot find self contract in the contract's environment|]

    let replacement = U.SeqEx $ U.PrimEx <$>
          [ U.SELF_ADDRESS varAnn
          , U.CONTRACT varAnn fieldAnn ty
          , U.IF_NONE
              do
                U.PrimEx <$>
                  [ U.PUSH def errorValueType errorValue
                  , U.FAILWITH
                  ]
              do []
          ]

    generateEmptyMetas selfInstr replacement
    pure replacement
  instr -> pure $ U.PrimEx instr

typesReplaceRules :: U.T -> U.T
typesReplaceRules = \case
  U.TBigMap tyKey tyValue -> U.TMap tyKey tyValue
  other -> other

-- | Since optimization disabling feature in @ligo@ can produce
-- badly typed Michelson code, we should fix it somehow. At this
-- moment we're doing the next things:
-- 1. Replacing @SELF %ep@ instr inside lambda with @{ SELF_ADDRESS; CONTRACT %ep ty }@
-- 2. Replacing all @BIG_MAP@ occurrences with @MAP@.
--    We're doing this by replacing all big-map's @Ty@ to map ones
--    and replacing @EMPTY_BIG_MAP@ instr to @EMPTY_MAP@.
--
-- Returns processed contract, processed metas (in reversed order) and not processed metas.
preprocessContract
  :: forall meta
   . (Default meta)
  => U.Contract
  -> [meta]
  -> (U.T -> U.T)
  -> (U.ExpandedInstr -> PreprocessMonad meta U.ExpandedOp)
  -> Either PreprocessError (U.Contract, [meta], [meta])
preprocessContract con@U.Contract{..} metas typesRules instrRules =
  let
    (U.ParameterType rootType _) = contractParameter
    ctx = U.mkEntrypointsMap contractParameter <> M.fromList [(U.DefEpName, rootType)]
    mappedOpsInMonad = mapM go contractCode
    (mappedOpsE, PreprocessState{..}) = runState (runExceptT $ runReaderT mappedOpsInMonad ctx) (PreprocessState metas [])
  in (\ops -> (con { U.contractCode = ops }, psMetasOut, psMetasIn)) <$> mappedOpsE
  where
    processOneMeta :: PreprocessMonad meta ()
    processOneMeta = do
      metasIn <- use psMetasInL
      case metasIn of
        [] -> pass
        (meta : xs) -> do
          psMetasOutL %= cons meta
          psMetasInL .= xs

    processMetas :: U.ExpandedInstr -> PreprocessMonad meta ()
    processMetas instr = replicateM_ (metasToProcess instr) processOneMeta

    go :: U.ExpandedOp -> PreprocessMonad meta U.ExpandedOp
    go = fmap (everywhere $ mkT typesRules) . everywhereM' (mkM preprocessExpandedOps)
      where
        preprocessExpandedOps :: U.ExpandedOp -> PreprocessMonad meta U.ExpandedOp
        preprocessExpandedOps = \case
          U.PrimEx instr -> do
            processMetas instr
            case instr of
              U.CREATE_CONTRACT varAnn1 varAnn2 contract -> do
                oldMetas <- use psMetasInL
                (processedContract, newMetas, unusedMetas) <- liftEither $ preprocessContract contract oldMetas typesRules instrRules
                psMetasOutL %= mappend newMetas
                psMetasInL .= unusedMetas
                pure $ U.PrimEx $ U.CREATE_CONTRACT varAnn1 varAnn2 processedContract
              _ -> instrRules instr
          seqEx@U.SeqEx{} -> do
            processOneMeta
            pure seqEx
          other -> pure other

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
  :: LigoMapper 'Unique
  -> (U.T -> U.T)
  -> (forall meta. (Default meta) => U.ExpandedInstr -> PreprocessMonad meta U.ExpandedOp)
  -> Either DecodeError (Set SourceLocation, SomeContract, [FilePath])
readLigoMapper ligoMapper typeRules instrRules = do
  let indexes :: [TableEncodingIdx] =
        extractInstructionsIndexes (lmMichelsonCode ligoMapper)
  metaPerInstr :: [LigoIndexedInfo 'Unique] <-
    forM indexes \i ->
      maybe (Left $ InsufficientMeta i) pure $
        lmLocations ligoMapper V.!? unTableEncodingIdx i

  (SomeContract contract, newMetas) <- fromExpressionToTyped (lmMichelsonCode ligoMapper) metaPerInstr typeRules instrRules
  extendedContract@(SomeContract extContract) <- first MetaEmbeddingError $
    (\code -> SomeContract contract{ cCode = ContractCode code }) <$>
      embedInInstr @EmbeddedLigoMeta
        newMetas
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
    mentionedSourceLocs :: LigoIndexedInfo 'Unique -> [SourceLocation]
    mentionedSourceLocs LigoIndexedInfo{..} =
      maybeToList $ ligoRangeToSourceLocation <$> liiLocation

    getSourceLocations :: Instr i o -> [EmbeddedLigoMeta]
    getSourceLocations = DL.toList . dfsFoldInstr def { dsGoToValues = True } \case
      Meta (SomeMeta (cast -> Just (meta :: EmbeddedLigoMeta))) _ -> DL.singleton meta
      _ -> mempty
