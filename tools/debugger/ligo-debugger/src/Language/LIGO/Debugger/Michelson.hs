{-# LANGUAGE DeriveDataTypeable, UndecidableInstances #-}

module Language.LIGO.Debugger.Michelson
  ( DecodeError (..)
  , MichelsonDecodeException (..)
  , PreprocessError (..)
  , readLigoMapper
  ) where

import Control.Lens (at, devoid, forOf, unsafePartsOf)
import Control.Lens.Extras (template)
import Control.Lens.Prism (_Just)
import Control.Monad.Except (Except, liftEither, runExcept, throwError)
import Data.DList qualified as DL
import Data.Data (Data)
import Data.Default (Default, def)
import Data.HashSet qualified as HashSet
import Data.Set qualified as Set
import Fmt.Buildable (Buildable, build, indentF, pretty, unlinesF, (+|), (|+))
import Fmt.Utils (Doc)
import Text.Interpolation.Nyan hiding (rmode')
import Text.Show qualified
import Util (rmode')

import Morley.Debugger.Core.Common (debuggerTcOptions)
import Morley.Micheline.Class (FromExp, FromExpError (FromExpError), fromExp)
import Morley.Micheline.Expression (Exp (..), Expression, MichelinePrimAp (..), annotToText)
import Morley.Micheline.Expression.WithMeta (ExpressionWithMeta, WithMeta, expAllExtraL, expMetaL)
import Morley.Michelson.Printer (RenderDoc, isRenderable, renderDoc)
import Morley.Michelson.Printer.Util (renderOpsList)
import Morley.Michelson.Text (MText, mt)
import Morley.Michelson.TypeCheck
  (TcError' (..), TcTypeError (UnsupportedTypeForScope), typeCheckingWith)
import Morley.Michelson.TypeCheck qualified as Tc
import Morley.Michelson.TypeCheck.Helpers qualified as Tc
import Morley.Michelson.TypeCheck.Instr qualified as Tc
import Morley.Michelson.Typed
  (BadTypeForScope (BtHasTicket), Contract' (..), ContractCode' (unContractCode), DfsSettings (..),
  HandleImplicitDefaultEp (WithImplicitDefaultEp), Instr (..), SomeContract (..), dfsFoldInstr,
  pattern (:#), pattern ConcreteMeta)
import Morley.Michelson.Typed qualified as T
import Morley.Michelson.Untyped qualified as U
import Morley.Tezos.Address (mformatAddress)
import Morley.Tezos.Crypto (encodeBase58Check)

import Language.LIGO.Debugger.CLI
import Language.LIGO.Debugger.Common
import Language.LIGO.Debugger.Error
import Language.LIGO.Range

-- | Untyped Michelson instruction with meta embedded.
data OpWithMeta meta
  = MPrimEx (InstrWithMeta meta)
  | MSeqEx [OpWithMeta meta]
  | MMetaEx meta (OpWithMeta meta)
  deriving stock (Eq, Show, Data, Generic)
  deriving anyclass (NFData)

instance RenderDoc (OpWithMeta meta) where
  renderDoc pn (MMetaEx _ op) = renderDoc pn op
  renderDoc pn (MPrimEx i) = renderDoc pn i
  renderDoc _  (MSeqEx i) = renderOpsList False i
  isRenderable =
    \case MPrimEx i -> isRenderable i
          MMetaEx _ op -> isRenderable op
          _ -> True

instance (Buildable meta) => Buildable (OpWithMeta meta) where
  build (MMetaEx meta op) =  [int||<MMetaEx: #{meta} #{op}>|]
  build (MPrimEx expandedInstr) =  [int||<MPrimEx: #{expandedInstr}>|]
  build (MSeqEx expandedOps)    =  [int||<MSeqEx: #{expandedOps}>|]

type InstrWithMeta meta = U.InstrAbstract [] (OpWithMeta meta)
type ContractWithMeta meta = U.Contract' (OpWithMeta meta)

instance (meta ~ meta') => FromExp (WithMeta meta) (OpWithMeta meta') where
  fromExp e = MMetaEx (view expMetaL e) <$> case e of
    ExpSeq _ exps -> MSeqEx <$> traverse fromExp exps
    other -> MPrimEx <$> fromExp other

typeCheckOpWithMeta
  :: (Show meta, NFData meta, Data meta)
  => (meta -> Bool) -> Tc.TcInstrBase (OpWithMeta meta)
typeCheckOpWithMeta isRedundantMeta instr hst = case instr of
  MMetaEx m i ->
    typeCheckOpWithMeta isRedundantMeta i hst
      <&> Tc.mapSeq
            if isRedundantMeta m
            then id
            else Tc.mapSomeInstr $ T.Meta (T.SomeMeta m)
  MPrimEx i ->
    Tc.typeCheckInstr (typeCheckOpWithMeta isRedundantMeta) i hst
  MSeqEx is ->
    Tc.typeCheckImpl (typeCheckOpWithMeta isRedundantMeta) is hst <&> Tc.mapSeq (Tc.mapSomeInstr T.Nested)

instance Data meta => Tc.IsInstrOp (OpWithMeta meta) where
  liftInstr = MPrimEx
  pickErrorSrcPos _ = Nothing
  tryOpToVal = \case
    MMetaEx _ i -> Tc.tryOpToVal i
    MSeqEx is -> case nonEmpty is of
      Nothing -> pure $ U.ValueNil
      Just is' -> U.ValueSeq <$> traverse Tc.tryOpToVal is'
    MPrimEx _ -> mzero
  tryValToOp = \case
    U.ValueNil -> Just $ MSeqEx []
    U.ValueSeq xs -> MSeqEx . toList <$> traverse Tc.tryValToOp xs
    _ -> Nothing

newtype FromExpressionWithMetaError meta
  = FromExpressionWithMetaError (FromExpError (WithMeta meta))
  deriving newtype (Eq)

instance Buildable (FromExpressionWithMetaError EmbeddedLigoMeta) where
  build (FromExpressionWithMetaError (FromExpError expr err)) =
    unlinesF
      [ "Failed to convert expression:"
      , indentF 2 $ buildExpWithMeta expr
      , ""
      , "Error:"
      , indentF 2 $ build err
      ]
    where
      buildExpWithMeta :: Exp (WithMeta EmbeddedLigoMeta) -> Doc
      buildExpWithMeta = \case
        ExpInt _ i -> build i
        ExpString _ s -> build s
        ExpBytes _ b ->
          build $ encodeBase58Check b
        ExpSeq _ s -> "(" +| buildList' buildExpWithMeta s |+ ")"
        ExpPrim _ (MichelinePrimAp prim s annots) ->
          build prim <> " " |+ "(" +|
          buildList' buildExpWithMeta s +| ") " +|
          buildList' (build . annotToText) annots
        where
          buildList' buildElem = mconcat . intersperse ", " . map buildElem

newtype TcErrorWithMeta meta = TcErrorWithMeta (TcError' (OpWithMeta meta))
  deriving newtype (Eq, Buildable)

data DecodeError meta
  = FromExpressionFailed (FromExpressionWithMetaError meta)
  | MetaEmbeddingError MetaEmbeddingError
  | TypeCheckFailed (TcErrorWithMeta meta)
  | PreprocessError PreprocessError
  deriving stock (Eq, Generic)

instance (Buildable (FromExpressionWithMetaError meta), Buildable meta) => Buildable (DecodeError meta) where
  build = \case
    FromExpressionFailed err ->
      [int||Failed to parse Micheline expression: #{err}|]
    MetaEmbeddingError err ->
      pretty err
    TypeCheckFailed err ->
      [int||Failed to typecheck the Michelson contract: #{err}|]
    PreprocessError err ->
      pretty err

data MetaEmbeddingError
  = InsufficientMetas Int
  | ExtraMetas Int
  deriving stock (Eq, Generic)

instance Buildable MetaEmbeddingError where
  build = \case
    InsufficientMetas n ->  [int||Insufficient number of entries: #s{n}|]
    ExtraMetas n ->  [int||Too many debug entries left: #s{n}|]

-- | Embeds metas in DFS order. If the length of metas list is not equal to
-- the size of @Micheline@ tree, then the error would be thrown.
embedMetas :: forall meta. [meta] -> Expression -> Either MetaEmbeddingError (ExpressionWithMeta meta)
embedMetas metas expr = forOf (unsafePartsOf (expAllExtraL devoid)) expr (ensureLength metas)
  where
    ensureLength :: [meta] -> [()] -> Either MetaEmbeddingError [meta]
    ensureLength [] [] = pure []
    ensureLength (x : xs) (_ : ys) = (x :) <$> ensureLength xs ys
    ensureLength [] ys = throwError (InsufficientMetas $ length ys)
    ensureLength xs [] = throwError (ExtraMetas $ length xs)

expressionToUntypedContract :: forall meta. ExpressionWithMeta meta -> Either (DecodeError meta) (ContractWithMeta meta)
expressionToUntypedContract expWithMeta = first (FromExpressionFailed . FromExpressionWithMetaError) $
  fromExp @(WithMeta meta) @(ContractWithMeta _) expWithMeta

newtype MichelsonDecodeException = MichelsonDecodeException (DecodeError EmbeddedLigoMeta)
  deriving stock (Generic)
  deriving newtype Buildable

instance Show MichelsonDecodeException where
  show = pretty

instance Exception MichelsonDecodeException

instance DebuggerException MichelsonDecodeException where
  type ExceptionTag MichelsonDecodeException = "MichelsonDecode"
  debuggerExceptionType (MichelsonDecodeException err) = case err of
    FromExpressionFailed{} -> MidLigoLayerException
    MetaEmbeddingError{} -> MidLigoLayerException
    TypeCheckFailed{} -> MidLigoLayerException
    PreprocessError err' -> case err' of
      EntrypointTypeNotFound{} -> UserException
      UnsupportedTicketDup -> UserException
  shouldInterruptDebuggingSession = False

wrapTypeCheckFailed :: TcError' (OpWithMeta meta) -> DecodeError meta
wrapTypeCheckFailed = \case
  TcFailedOnInstr _ _ _ _ (Just (UnsupportedTypeForScope _ BtHasTicket)) ->
    PreprocessError UnsupportedTicketDup
  other -> TypeCheckFailed $ TcErrorWithMeta other

fromUntypedToTyped
  :: (Default meta, Data meta, Show meta, NFData meta)
  => ContractWithMeta meta
  -> (meta -> Bool)
  -> Either (DecodeError meta) SomeContract
fromUntypedToTyped uContract isRedundantMeta = do
  processedUContract <- first PreprocessError $ preprocessContract uContract
  first wrapTypeCheckFailed
    $ typeCheckingWith debuggerTcOptions
    $ Tc.typeCheckContract' (typeCheckOpWithMeta isRedundantMeta) processedUContract

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
  Except PreprocessError

createErrorValue :: MText -> U.Value' [] (OpWithMeta meta)
createErrorValue errMsg =
  U.ValuePair (U.ValueString addrText) (U.ValueString errMsg)
  where
    addrText = mformatAddress errorAddress

-- | Makes replacements in types.
-- At this moment we replace only big maps with maps.
replaceTy :: U.Ty -> U.Ty
replaceTy (U.Ty t ann) = U.Ty (replaceT t) ann
  where
    replaceT :: U.T -> U.T
    replaceT = \case
      U.TOption ty -> U.TOption (replaceTy ty)
      U.TList ty -> U.TList (replaceTy ty)
      U.TSet ty -> U.TSet (replaceTy ty)
      U.TContract ty -> U.TContract (replaceTy ty)
      U.TTicket ty -> U.TTicket (replaceTy ty)
      U.TPair fAnn1 fAnn2 varAnn1 varAnn2 ty ty' ->
        U.TPair fAnn1 fAnn2 varAnn1 varAnn2 (replaceTy ty) (replaceTy ty')
      U.TOr fAnn1 fAnn2 ty ty' -> U.TOr fAnn1 fAnn2 (replaceTy ty) (replaceTy ty')
      U.TLambda ty ty' -> U.TLambda (replaceTy ty) (replaceTy ty')
      U.TMap ty ty' -> U.TMap (replaceTy ty) (replaceTy ty')

      -- Actual replacements
      U.TBigMap ty ty' -> U.TMap (replaceTy ty) (replaceTy ty')
      other -> other

-- | Makes replacements in instructions.
-- At this moments we do the next replacements:
-- 1, @EMPTY_BIG_MAP@ -> @EMPTY_MAP@
-- 2. @SELF@ -> @{ SELF_ADDRESS; CONTRACT; IF_NONE { PUSH err; FAILWITH; } {}; }@
replaceOpWithMeta
  :: forall meta.
  (Default meta, Data meta)
  => OpWithMeta meta
  -> PreprocessMonad meta (OpWithMeta meta)
replaceOpWithMeta = \case
  MPrimEx instr -> replaceInstr instr
  MSeqEx ops -> MSeqEx <$> traverse replaceOpWithMeta ops
  MMetaEx meta op -> do
    mappedOp <- replaceOpWithMeta op
    pure $ MMetaEx meta mappedOp
  where
    replaceInstr :: InstrWithMeta meta -> PreprocessMonad meta (OpWithMeta meta)
    replaceInstr = \case
      U.EXT ext -> case ext of
        U.UTEST_ASSERT U.TestAssert{tassInstrs = ops, ..} -> do
          tassInstrs <- traverse replaceOpWithMeta ops
          returnInstr $ U.EXT $ U.UTEST_ASSERT U.TestAssert{..}
        other -> returnInstr (U.EXT other)
      U.PUSH varAnn ty val -> do
        replacedVal <- replaceValue val
        returnInstr $ U.PUSH varAnn (replaceTy ty) replacedVal
      U.NONE typeAnn varAnn ty -> returnInstr $ U.NONE typeAnn varAnn (replaceTy ty)
      U.IF_NONE ops ops' ->
        MPrimEx ... U.IF_NONE <$> traverse replaceOpWithMeta ops <*> traverse replaceOpWithMeta ops'
      U.LEFT typeAnn varAnn fAnn1 fAnn2 ty ->
        returnInstr $ U.LEFT typeAnn varAnn fAnn1 fAnn2 (replaceTy ty)
      U.RIGHT typeAnn varAnn fAnn1 fAnn2 ty ->
        returnInstr $ U.RIGHT typeAnn varAnn fAnn1 fAnn2 (replaceTy ty)
      U.IF_LEFT ops ops' ->
        MPrimEx ... U.IF_LEFT <$> traverse replaceOpWithMeta ops <*> traverse replaceOpWithMeta ops'
      U.NIL typeAnn varAnn ty -> returnInstr $ U.NIL typeAnn varAnn (replaceTy ty)
      U.IF_CONS ops ops' ->
        MPrimEx ... U.IF_CONS <$> traverse replaceOpWithMeta ops <*> traverse replaceOpWithMeta ops'
      U.EMPTY_SET typeAnn varAnn ty -> returnInstr $ U.EMPTY_SET typeAnn varAnn (replaceTy ty)
      U.EMPTY_MAP typeAnn varAnn tyKey tyValue ->
        returnInstr $ U.EMPTY_MAP typeAnn varAnn (replaceTy tyKey) (replaceTy tyValue)
      U.MAP varAnn ops -> MPrimEx . U.MAP varAnn <$> traverse replaceOpWithMeta ops
      U.ITER ops -> MPrimEx . U.ITER <$> traverse replaceOpWithMeta ops
      U.IF ops ops' ->
        MPrimEx ... U.IF <$> traverse replaceOpWithMeta ops <*> traverse replaceOpWithMeta ops'
      U.LOOP ops -> MPrimEx . U.LOOP <$> traverse replaceOpWithMeta ops
      U.LOOP_LEFT ops -> MPrimEx . U.LOOP_LEFT <$> traverse replaceOpWithMeta ops
      U.LAMBDA varAnn ty ty' ops ->
        MPrimEx . U.LAMBDA varAnn (replaceTy ty) (replaceTy ty') <$> traverse replaceOpWithMeta ops
      U.LAMBDA_REC varAnn ty ty' ops ->
        MPrimEx . U.LAMBDA_REC varAnn (replaceTy ty) (replaceTy ty') <$> traverse replaceOpWithMeta ops
      U.DIP ops -> MPrimEx . U.DIP <$> traverse replaceOpWithMeta ops
      U.DIPN n ops -> MPrimEx . U.DIPN n <$> traverse replaceOpWithMeta ops
      U.CAST varAnn ty -> returnInstr $ U.CAST varAnn (replaceTy ty)
      U.UNPACK typeAnn varAnn ty -> returnInstr $ U.UNPACK typeAnn varAnn (replaceTy ty)
      U.VIEW varAnn viewName ty -> returnInstr $ U.VIEW varAnn viewName (replaceTy ty)
      U.CONTRACT varAnn fAnn ty -> returnInstr $ U.CONTRACT varAnn fAnn (replaceTy ty)
      U.CREATE_CONTRACT varAnn1 varAnn2 contract -> do
        processedContract <- liftEither $ preprocessContract contract
        returnInstr $ U.CREATE_CONTRACT varAnn1 varAnn2 processedContract
      U.EMIT varAnn fAnn tyMb -> returnInstr $ U.EMIT varAnn fAnn (replaceTy <$> tyMb)

      -- Actual replacements
      U.EMPTY_BIG_MAP typeAnn varAnn tyKey tyValue ->
        returnInstr $ U.EMPTY_MAP typeAnn varAnn (replaceTy tyKey) (replaceTy tyValue)

      U.SELF varAnn rawLigoAnn -> do
        -- LIGO compiles `self` with default entrypoint to `SELF @default`,
        -- not just `SELF`, and Morley does not work with that well
        -- (`EpName ""` and `EpName "default"` are treated as different
        -- entrypoints)
        let michAnn = if rawLigoAnn == [U.annQ|default|] then U.noAnn else rawLigoAnn
        let epName = U.epNameFromSelfAnn michAnn
        ty <- replaceTy <$> do
          tyMb <- view $ at epName
          maybe
            do throwError $ EntrypointTypeNotFound epName
            pure
            tyMb

        let errorValue =
              createErrorValue [mt|Cannot find self contract in the contract's environment|]

        pure $
          MSeqEx $ MPrimEx <$>
            [ U.SELF_ADDRESS varAnn
            , U.CONTRACT varAnn michAnn ty
            , U.IF_NONE
                do
                  MPrimEx <$>
                    [ U.PUSH def errorValueType errorValue
                    , U.FAILWITH
                    ]
                do []
            ]

      other -> returnInstr other

    returnInstr :: InstrWithMeta meta -> PreprocessMonad meta (OpWithMeta meta)
    returnInstr = pure . MPrimEx

    replaceValue :: U.Value' [] (OpWithMeta meta) -> PreprocessMonad meta (U.Value' [] (OpWithMeta meta))
    replaceValue = \case
      U.ValuePair l r -> U.ValuePair <$> replaceValue l <*> replaceValue r
      U.ValueLeft l -> U.ValueLeft <$> replaceValue l
      U.ValueRight r -> U.ValueRight <$> replaceValue r
      U.ValueSome v -> U.ValueSome <$> replaceValue v
      U.ValueSeq lst -> U.ValueSeq <$> traverse replaceValue lst
      U.ValueMap lst ->
        let
          mapElt (U.Elt k v) = U.Elt <$> replaceValue k <*> replaceValue v
        in U.ValueMap <$> traverse mapElt lst
      U.ValueLambda lst -> U.ValueLambda <$> traverse replaceOpWithMeta lst
      U.ValueLamRec lst -> U.ValueLamRec <$> traverse replaceOpWithMeta lst
      other -> pure other

-- | Since optimization disabling feature in @ligo@ can produce
-- badly typed Michelson code, we should fix it somehow. At this
-- moment we're doing the next things:
-- 1. Replacing @SELF %ep@ instr inside lambda with @{ SELF_ADDRESS; CONTRACT %ep ty }@
-- 2. Replacing all @BIG_MAP@ occurrences with @MAP@.
--    We're doing this by replacing all big-map's @Ty@ to map ones
--    and replacing @EMPTY_BIG_MAP@ instr to @EMPTY_MAP@.
--
-- Returns a contract with replaced instructions and types.
preprocessContract
  :: forall meta
   . (Default meta, Data meta)
  => ContractWithMeta meta
  -> Either PreprocessError (ContractWithMeta meta)
preprocessContract con@U.Contract{..} =
  let
    ctx = U.mkEntrypointsMap WithImplicitDefaultEp contractParameter
    U.ParameterType t rootAnn = contractParameter
    mappedOpsInMonad = replaceOpWithMeta contractCode
    mappedOpsE = runExcept $ runReaderT mappedOpsInMonad ctx
  in mappedOpsE <&> \ops -> con
      { U.contractCode = ops
      , U.contractParameter = U.ParameterType (replaceTy t) rootAnn
      , U.contractStorage = replaceTy contractStorage
      }

-- | Read LIGO's debug output and produce
--
-- 1. All expression locations. We return __all__ expression locations
--    because we need them to extract all the statement ones.
-- 2. A contract with inserted @Meta (SomeMeta (info :: 'EmbeddedLigoMeta'))@
--    wrappers that carry the debug info.
-- 3. All contract filepaths that would be used in debugging session.
-- 4. All locations that are related to lambdas.
-- 5. LIGO type of entrypoint.
-- 6. Vector with LIGO types.
readLigoMapper
  :: LigoMapper 'Unique
  -> Either (DecodeError EmbeddedLigoMeta) (Set ExpressionSourceLocation, SomeContract, [FilePath], HashSet Range, LigoType, LigoTypesVec)
readLigoMapper ligoMapper = do
  extendedExpression <- first MetaEmbeddingError $
    embedMetas (lmLocations ligoMapper) (lmMichelsonCode ligoMapper)

  -- At this moment entrypoint type appears as
  -- the first element in the last environment meta.
  let entrypointType = ligoMapper
        & lmLocations
        & foldMap
            do \LigoIndexedInfo{..} -> case liiEnvironment of
                Just (LigoStackEntry LigoExposedStackEntry{..} : _) -> Last $ Just leseType
                _ -> Last Nothing
        & getLast
        & maybe (LigoType Nothing) (readLigoType $ lmTypes ligoMapper)

  uContract <-
    expressionToUntypedContract extendedExpression

  extendedContract@(SomeContract extContract) <-
    fromUntypedToTyped uContract isRedundantIndexedInfo

  let allFiles = uContract ^.. template @_ @EmbeddedLigoMeta . liiLocationL . _Just . rFile
        -- We want to remove duplicates
        & unstableNub
        & filter (not . isLigoStdLib)

  let (exprLocs, lambdaLocs) =
        -- We expect a lot of duplicates, stripping them via putting to Set
        bimap Set.fromList HashSet.fromList $
        getSourceLocations (unContractCode $ cCode extContract)

  -- The LIGO's debug info may be really large, so we better force
  -- the evaluation for all the info that will be stored for the entire
  -- debug session, and let GC wipe out everything intermediate.
  return $! force (exprLocs, extendedContract, allFiles, lambdaLocs, entrypointType, lmTypes ligoMapper)

  where
    getSourceLocations :: Instr i o -> ([ExpressionSourceLocation], [Range])
    getSourceLocations = bimap DL.toList DL.toList . dfsFoldInstr def { dsGoToValues = True } \case
      ConcreteMeta (liiLocation @'Unique -> Just loc) instr
        -> let lambdaRange =
                case instr of
                  Nested LAMBDA{} -> DL.singleton loc
                  Nested (LAMBDA{} :# _) -> DL.singleton loc
                  _ -> mempty
           in (DL.singleton (ExpressionSourceLocation loc $ not . shouldIgnoreMeta loc instr), lambdaRange)
      _ -> mempty
