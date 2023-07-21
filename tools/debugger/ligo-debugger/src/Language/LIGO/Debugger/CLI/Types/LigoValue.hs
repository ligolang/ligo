{-# LANGUAGE DeriveDataTypeable, NumDecimals, UndecidableInstances #-}

-- | Ligo Value representation from
-- https://ligolang.org/assets/files/values.schema-11c1c8c46fa17d8c9eae1cf28c49a307.json
-- json-schema.
module Language.LIGO.Debugger.CLI.Types.LigoValue
  ( module Language.LIGO.Debugger.CLI.Types.LigoValue
  ) where

import Debug qualified

import Control.Exception (throw)
import Data.Aeson
  (FromJSON (parseJSON), FromJSONKey, GFromJSON, Options (..), SumEncoding (TwoElemArray),
  Value (..), Zero, defaultOptions, genericParseJSON, withArray, withText)
import Data.Aeson.Types (Parser)
import Data.Char (toUpper)
import Data.Data (Data)
import Data.Fixed (Fixed (MkFixed))
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Text.Lazy.Builder (Builder)
import Data.Time (nominalDiffTimeToSeconds)
import Data.Time.Clock.System (SystemTime (MkSystemTime), systemToUTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Vector qualified as V
import Fmt.Buildable (Buildable, build, hexF, pretty)
import Fmt.Utils (Doc)
import GHC.Generics (Rep)
import GHC.TypeLits (Nat)
import Text.Interpolation.Nyan hiding (rmode')
import Util

import Morley.Debugger.Core (DebugPrint (DebugPrint), DebugPrintMode (..))
import Morley.Michelson.Typed (Constrained (SomeValue), SomeValue)
import Morley.Michelson.Typed qualified as T
import Morley.Michelson.Untyped qualified as U
import Morley.Tezos.Core qualified as T
import Morley.Tezos.Crypto.BLS12381 (toMichelsonBytes)

import Language.LIGO.AST.Skeleton (Lang (Caml, Js))
import Language.LIGO.Debugger.CLI.Exception
import Language.LIGO.Debugger.CLI.Helpers
import Language.LIGO.Debugger.CLI.Types

-------------
--- Types ---
-------------

data LigoOrMichValue
  = LigoValue LigoType LigoValue
  | MichValue LigoType SomeValue
  | ToBeComputed
    -- ^ Since we compute @LIGO@ values in async manner
    -- we can use this stub if value isn't available yet.
  deriving stock (Generic, Show, Eq)

data LigoValue
  = LVCt LigoConstant
  | LVList [LigoValue]
  | LVRecord (HashMap LigoLabel LigoValue)
  | LVConstructor (Text, LigoValue)
  | LVSet [LigoValue]
  | LVMap [(LigoValue, LigoValue)]
  | LVTypedAddress Text
  | LVMichelson LigoValueMichelson
  | LVMichelsonContract Value
  | LVGen Value
  | LVLocation Value
  | LVMutation Value
  | LVAstContract Value
  | LVFuncVal
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass (NFData)

data LigoLabel = LLabel Text
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass (NFData, Hashable, FromJSONKey)

data LigoMutFlag
  = LMFImmutable
  | LMFMutable
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass (NFData)

data LigoValueExpr = LigoValueExpr
  { lveAstType :: Value
  , lveEvalTerm :: LigoValue
  } deriving stock (Generic, Show, Eq, Data)
    deriving anyclass (NFData)
    deriving (FromJSON) via LigoValueJSON 3 LigoValueExpr

data LigoEnvItem = LigoEnvItem
  { leiItem :: LigoValueExpr
  , leiNoMutation :: Bool
  , leiInline :: Bool
  } deriving stock (Generic, Show, Eq, Data)
    deriving anyclass (NFData)
    deriving (FromJSON) via LigoValueJSON 3 LigoEnvItem

-- This type is unused now but may be
-- useful at some moment of time.
data LigoFuncVal = LigoFuncVal
  { lfvRecName :: Maybe LigoVar
  , lfvOrigLambda :: Value
  , lfvBody :: Value
  , lfvArgBinder :: LigoVar
  , lfvArgMutFlag :: LigoMutFlag
  , lfvEnv :: [(LigoVar, LigoEnvItem)]
  } deriving stock (Generic, Show, Eq, Data)
    deriving anyclass (NFData)
    deriving (FromJSON) via LigoValueJSON 3 LigoFuncVal

data LigoContract = LigoContract
  { lcAddress :: Text
  , lcEntrypoint :: Maybe Text
  } deriving stock (Generic, Show, Eq, Data)
    deriving anyclass (NFData)
    deriving (FromJSON) via LigoValueJSON 2 LigoContract

data LigoConstant
  = LCString Text
  | LCBytes Text
  | LCAddress Text
  | LCContract LigoContract
  | LCNat Text
  | LCTimestamp Text
  | LCKeyHash Text
  | LCKey Text
  | LCSignature Text
  | LCBls LigoBLS12381
  | LCChainId Text
  | LCInt Text
  | LCInt64 Integer
  | LCMutez Text
  | LCBool Bool
  | LCUnit
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass (NFData)

data LigoBLS12381
  = LBls12381G1 ByteString
  | LBls12381G2 ByteString
  | LBls12381Fr ByteString
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass (NFData)

data LigoValueTyCode = LigoValueTyCode
  { lvtcAstTy :: Value
  , lvtcMichelineRepr :: Value
  } deriving stock (Generic, Show, Eq, Data)
    deriving anyclass (NFData)
    deriving (FromJSON) via LigoValueJSON 4 LigoValueTyCode

data LigoValueMichelson
  = LVMUntypedCode Value
  | LVMTyCode LigoValueTyCode
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass (NFData)

---------------
--- Helpers ---
---------------

newtype LigoValueJSON (n :: Nat) a = LigoValueJSON a

instance (Generic a, GFromJSON Zero (Rep a), KnownNat n) => FromJSON (LigoValueJSON n a) where
  parseJSON = fmap LigoValueJSON . genericParseJSON defaultOptions
    { fieldLabelModifier = genericDrop (natVal (Proxy @n) + 1) . toSnakeCase
    }

toUpperCase :: String -> String
toUpperCase = \case
  x : xs -> toUpper x : xs
  [] -> []

surround :: (Buildable a) => Doc -> Doc -> Doc -> [a] -> Doc
surround left right sep lst = mconcat $ left : intersperse sep (build <$> lst) ++ [right]

toTupleMaybe :: LigoValue -> Maybe [LigoValue]
toTupleMaybe (LVRecord record) = forM [0..HM.size record - 1] \i -> HM.lookup (LLabel $ pretty i) record
toTupleMaybe _ = Nothing

getTypeByFieldName :: Text -> LigoType -> LigoType
getTypeByFieldName field = \case
  LigoTypeResolved LigoTypeExpression
    { _lteTypeContent = LTCRecord LigoTypeTable{..}
    } -> LigoType $ HM.lookup field _lttFields
  _ -> LigoTypeUnresolved

-- | Tries to decompile @Michelson@ primitive into LIGO value.
tryDecompilePrimitive :: SomeValue -> Maybe LigoValue
tryDecompilePrimitive (SomeValue val) = case val of
  T.VKey pk -> mkConstant (LCKey $ pretty pk)
  T.VUnit -> mkConstant LCUnit
  T.VSignature sig -> mkConstant (LCSignature $ pretty sig)
  T.VChainId chainId -> mkConstant (LCChainId $ decodeUtf8 $ T.unChainId chainId)
  T.VContract addr (T.SomeEpc T.EntrypointCall{..}) ->
    mkConstant
      (LCContract $ LigoContract (pretty addr)
      (guard (not $ U.isDefEpName epcName) >> pure (pretty epcName)))
  T.VInt n -> mkConstant (LCInt $ pretty n)
  T.VNat n -> mkConstant (LCNat $ pretty n)
  T.VString str -> mkConstant (LCString $ pretty str)
  T.VBytes bts -> mkConstant (LCBytes $ decodeUtf8 bts)
  T.VMutez mu -> mkConstant (LCMutez $ show $ T.unMutez mu)
  T.VBool bVal -> mkConstant (LCBool bVal)
  T.VKeyHash ha -> mkConstant (LCKeyHash $ pretty ha)
  T.VTimestamp (T.Timestamp (nominalDiffTimeToSeconds -> (MkFixed time))) ->
     mkConstant (LCTimestamp $ pretty (time `div` 1e12))
  T.VAddress T.EpAddress'{..} ->
    mkConstant
      (LCContract $ LigoContract (pretty eaAddress)
      (guard (not $ U.isDefEpName eaEntrypoint) >> pure (pretty eaEntrypoint)))
  T.VBls12381Fr bls -> mkConstant (LCBls $ LBls12381Fr $ toMichelsonBytes bls)
  T.VBls12381G1 bls -> mkConstant (LCBls $ LBls12381G1 $ toMichelsonBytes bls)
  T.VBls12381G2 bls -> mkConstant (LCBls $ LBls12381G2 $ toMichelsonBytes bls)
  _ -> Nothing
  where
    mkConstant :: LigoConstant -> Maybe LigoValue
    mkConstant = Just . LVCt

getLigoType :: LigoOrMichValue -> LigoType
getLigoType = \case
  LigoValue typ _ -> typ
  MichValue typ _ -> typ
  ToBeComputed -> LigoType Nothing

-----------------
--- Instances ---
-----------------

----------------
--- FromJSON ---
----------------

instance FromJSON LigoValue where
  parseJSON val = genericParseJSON defaultOptions
    { sumEncoding = TwoElemArray
    , constructorTagModifier = drop 1 . toSnakeCase . processCtor . drop 2
    } val <|> parseRecord val <|> parseFunction val
    where
      processCtor :: String -> String
      processCtor = \case
        "Ct" -> "Constant"
        "MichelsonContract" -> "ContractCode"
        "Gen" -> "Generator"
        other -> other

      parseRecord :: Value -> Parser LigoValue
      parseRecord = withArray "Record" \(V.toList -> arr) -> do
        LVRecord . HM.fromList <$> mapM parseElem arr
        where
          parseElem :: Value -> Parser (LigoLabel, LigoValue)
          parseElem = withArray "RecordElem" \arr -> do
            case V.toList arr of
              [labelValue, ligoValue] -> do
                label <- parseJSON labelValue
                value <- parseJSON ligoValue
                pure (label, value)
              _ -> fail "Expected 2 elements array"

      parseFunction :: Value -> Parser LigoValue
      parseFunction = withText "Function" \txt -> guard (txt == "<fun>") >> pure LVFuncVal

instance FromJSON LigoLabel where
  parseJSON = withArray "LigoLabel" \arr -> do
    case V.toList arr of
      [String ctor, label]
        | ctor == "Label" -> LLabel <$> parseJSON label
      _ -> fail "Expected 2 elements array"

instance FromJSON LigoMutFlag where
  parseJSON val = asum
    [ LMFImmutable <$ guardOneElemList "Immutable" val
    , LMFMutable <$ guardOneElemList "Mutable" val
    ]

-- An explanation about constants in @drop@s.
-- First @drop@ (the rightmosts) removes type abbreviation in constructor.
-- The second one @drop 1@ removes an underscore. This is because
-- @toSnakeCase@ function adds an underscore if string starts with a capital letter
-- (e.g. SomeCtor -> _some_ctor).

instance FromJSON LigoBLS12381 where
  parseJSON = const empty

instance FromJSON LigoConstant where
  parseJSON val = twoElemParser val <|> (LCUnit <$ guardOneElemList "unit" val)
    where
      twoElemParser = genericParseJSON defaultOptions
        { sumEncoding = TwoElemArray
        , constructorTagModifier = drop 1 . toSnakeCase . drop 2
        }

instance FromJSON LigoValueMichelson where
  parseJSON = genericParseJSON defaultOptions
    { sumEncoding = TwoElemArray
    , constructorTagModifier =  toUpperCase . drop 1 . toSnakeCase . drop 3
    }

-------------------------------------
--- Buildable and build functions ---
-------------------------------------

instance Buildable LigoOrMichValue where
  build = \case
    LigoValue ligoType ligoValue -> buildLigoValue ligoType ligoValue
    MichValue _ mich -> build mich
    ToBeComputed -> "computing..."

instance Buildable (DebugPrint (Lang, LigoOrMichValue)) where
  build (DebugPrint mode (lang, val)) =
    case val of
      MichValue _ (SomeValue michValue) -> build (DebugPrint mode michValue)
      LigoValue ligoType ligoValue -> buildLigoValue' lang mode ligoType ligoValue
      ToBeComputed -> build ToBeComputed

buildLigoValue' :: Lang -> DebugPrintMode -> LigoType -> LigoValue -> Doc
buildLigoValue' lang mode ligoType = \case
  LVCt constant -> buildConstant' lang mode constant
  LVList values ->
    let listWithSep sep = surround "[" "]" sep (buildLigoValue' lang mode innerTypeFromConstant <$> values) in
    case (mode, lang) of
      (DpmNormal, _) -> listWithSep "; "
      (DpmEvaluated, Caml) -> listWithSep "; "
      (DpmEvaluated, Js) ->  [int||list(#{listWithSep ", "})|]
  val@(LVRecord record) ->
    case toTupleMaybe val of
      Just tuple ->
        let
          innerTypes = map (\i -> getTypeByFieldName (pretty i) ligoType) [0..length tuple - 1]
          builtTuple lb rb = surround lb rb ", " (zipWith (buildLigoValue' lang mode) innerTypes tuple)
        in
          case (mode, lang) of
            (DpmNormal, _) -> builtTuple "(" ")"
            (DpmEvaluated, Caml) -> builtTuple "(" ")"
            (DpmEvaluated, Js) -> builtTuple "[" "]"
      Nothing ->
        let
          buildRecord :: [(Text, (LigoType, LigoValue))] -> Doc
          buildRecord keysAndValues =
            let
              recordWithSep sep =
                surround @Doc "{" "}" sep
                  (map (\(l, (t, v)) ->  [int||#{l} = #{buildLigoValue' lang mode t v}|]) keysAndValues)
            in
            case (mode, lang) of
              (DpmNormal, _) -> recordWithSep "; "
              (DpmEvaluated, Caml) -> recordWithSep "; "
              (DpmEvaluated, Js) -> recordWithSep ", "
        in
          buildRecord $ map (\(LLabel t, v) -> (t, (getTypeByFieldName t ligoType, v))) (HM.toList record)
  LVConstructor (ctor, value) ->
    let
      innerType = LigoType do
        LTCSum LigoTypeTable{..} <- _lteTypeContent <$> unLigoType ligoType
        HM.lookup ctor _lttFields
    in [int||#{ctor} (#{buildLigoValue' lang mode innerType value})|]
  LVSet values -> case mode of
    DpmNormal -> surround "{" "}" "; " (buildLigoValue' lang mode innerTypeFromConstant <$> values)
    DpmEvaluated ->  [int||Set.literal(#{buildLigoValue' lang mode ligoType $ LVList values})|]
  LVMap keyValues -> case mode of
    DpmNormal ->
      surround @Doc "[" "]" "; " $
        map do \(k, v) ->  [int||#{buildLigoValue' lang mode keyType k} -> #{buildLigoValue' lang mode valueType v}|]
        keyValues
    DpmEvaluated ->
      let
        mkTuple :: (LigoValue, LigoValue) -> LigoValue
        mkTuple (fstVal, sndVal) = LVRecord $ HM.fromList
          [ (LLabel "0", fstVal)
          , (LLabel "1", sndVal)
          ]

        moduleName = case typesFromConstantAndName of
          Just (_, toString -> name)
            | name == "map" -> "Map" :: Text
            | name == "big_map" -> "Big_map"
          _ -> "Map"

        listType = LigoType do
          keyTypeUnwrapped <- unLigoType keyType
          valueTypeUnwrapped <- unLigoType valueType

          pure $ mkConstantType "List" [mkPairType keyTypeUnwrapped valueTypeUnwrapped]

      in [int||#{moduleName}.literal(#{buildLigoValue' lang mode listType $ LVList (mkTuple <$> keyValues)})|]
  LVTypedAddress address -> build address
  LVMichelson _ -> "<internal: contract code>"
  LVMichelsonContract _ -> "<internal: michelson contract>"
  LVGen _ -> "<internal: generator>"
  LVLocation _ -> "<internal: heap location>"
  LVMutation _ -> "<internal: mutation>"
  LVAstContract _ -> "<internal: AST contract>"
  LVFuncVal -> "<fun>"
  where
    typesFromConstantAndName = do
      LTCConstant LigoTypeConstant{..} <- _lteTypeContent <$> unLigoType ligoType
      pure (_ltcParameters, T.toLower $ head _ltcInjection)

    innerTypeFromConstant = LigoType (listToMaybe . fst =<< typesFromConstantAndName)
    (keyType, valueType) = maybe (LigoType Nothing, LigoType Nothing) (bimap LigoType LigoType)
      case typesFromConstantAndName of
        Just ([k, v], _) -> pure (Just k, Just v)
        _ -> Nothing

buildLigoValue :: LigoType -> LigoValue -> Doc
buildLigoValue = buildLigoValue' Caml DpmNormal

buildConstant' :: Lang -> DebugPrintMode -> LigoConstant -> Doc
buildConstant' lang mode = \case
  LCString str -> build $ Debug.show @Text str
  LCBytes bts ->  [int||0x#{buildBytes bts}|]
  LCAddress addr -> case (mode, lang) of
    (DpmNormal, _) -> build addr
    (DpmEvaluated, Caml) ->  [int||(#{addr} : address)|]
    (DpmEvaluated, Js) ->  [int||(#{addr} as address)|]
  LCContract contract -> build contract
  LCNat n ->  [int||#{n}n|]
  LCTimestamp timestamp -> case (mode, lang) of
    (DpmNormal, _) ->  [int||timestamp(#{buildTimestamp timestamp})|]
    (DpmEvaluated, Caml) ->  [int||("#{buildTimestamp timestamp}" : timestamp)|]
    (DpmEvaluated, Js) ->  [int||("#{buildTimestamp timestamp}" as timestamp)|]
  LCKeyHash keyHash -> build keyHash
  LCKey key -> case (mode, lang) of
    (DpmNormal, _) -> build key
    (DpmEvaluated, Caml) ->  [int||("#{key}" : key)|]
    (DpmEvaluated, Js) ->  [int||("#{key}" as key)|]
  LCSignature sign -> case (mode, lang) of
    (DpmNormal, _) -> build sign
    (DpmEvaluated, Caml) ->  [int||("#{sign}" : signature)|]
    (DpmEvaluated, Js) ->  [int||("#{sign}" as signature)|]
  LCBls (LBls12381G1 bts) ->  [int||0x#{hexF bts}|]
  LCBls (LBls12381G2 bts) ->  [int||0x#{hexF bts}|]
  LCBls (LBls12381Fr bts) ->  [int||0x#{hexF bts}|]
  LCChainId bts -> buildChainId bts
  LCInt n -> build n
  LCInt64 n -> build n
  LCMutez n ->  [int||#{n}mutez|]
  LCBool bl -> build bl
  LCUnit -> case (mode, lang) of
    (DpmNormal, _) -> "()"
    (DpmEvaluated, Caml) -> "()"
    (DpmEvaluated, Js) -> "unit"
  where
    buildTimestamp :: Text -> Builder
    buildTimestamp (readMaybe @Int64 . toString -> Just n) = pretty $
      MkSystemTime n 0
        & systemToUTCTime
        & iso8601Show
        & build
    buildTimestamp other =
      throw (PluginCommunicationException [int||Unexpected string in timestamp: #{other}|])

    buildBytes :: Text -> Builder
    buildBytes bts = pretty $ hexF @ByteString $ encodeUtf8 bts

    buildChainId :: Text -> Doc
    buildChainId = build . T.UnsafeChainId . encodeUtf8

buildConstant :: LigoConstant -> Doc
buildConstant = buildConstant' Caml DpmNormal

instance Buildable LigoContract where
  build LigoContract{..} =  [int||#{lcAddress}#{ep}|]
    where
      ep = maybe ("" :: Builder) [int|m|(#{id})|] lcEntrypoint
