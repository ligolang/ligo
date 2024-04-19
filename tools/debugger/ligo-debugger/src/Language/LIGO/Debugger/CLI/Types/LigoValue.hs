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

-- | Represents a value either in LIGO or Michelson
-- format.
data LigoOrMichValue
  = LigoValue LigoType LigoValue
    -- ^ Value in LIGO format.
  | MichValue LigoType SomeValue
    -- ^ Value in Michelson format.
  | ToBeComputed
    -- ^ Since we compute @LIGO@ values in async manner
    -- we can use this stub if value isn't available yet.
  deriving stock (Generic, Show, Eq)

-- | LIGO value.
data LigoValue
  = LVCt LigoConstant
    -- ^ A literal value.
  | LVList [LigoValue]
    -- ^ A list of values.
  | LVRecord (HashMap LigoLabel LigoValue)
    -- ^ A record value.
  | LVConstructor (Text, LigoValue)
    -- ^ A sum value.
  | LVSet [LigoValue]
    -- ^ A set of values.
  | LVMap [(LigoValue, LigoValue)]
    -- ^ A map of values.
  | LVTypedAddress Text
    -- ^ A typed address value.
  | LVMichelson LigoValueMichelson
    -- ^ A raw Michelson value.
  | LVMichelsonContract Value
    -- ^ A Michelson contract value.
    -- It may occur only in the tests.
  | LVGen Value
    -- ^ A generator value.
    -- It may occur only in the tests.
  | LVLocation Value
    -- ^ A heap location value.
    -- It may occur only in the tests.
  | LVMutation Value
    -- ^ A mutation value.
    -- It may occur only in the tests.
  | LVAstContract Value
    -- ^ A contract's AST value.
    -- It may occur only in the tests.
  | LVFuncVal
    -- ^ A function value.
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass (NFData)

-- | A record's label.
data LigoLabel = LLabel Text
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass (NFData, Hashable, FromJSONKey)

-- | A contract value.
data LigoContract = LigoContract
  { lcAddress :: Text
    -- ^ A contract's address.
  , lcEntrypoint :: Maybe Text
    -- ^ A contract's entry point.
  } deriving stock (Generic, Show, Eq, Data)
    deriving anyclass (NFData)
    deriving (FromJSON) via LigoValueJSON 2 LigoContract

-- | A literal value.
data LigoConstant
  = LCString Text
    -- ^ String value.
  | LCBytes Text
    -- ^ Bytes value.
  | LCAddress Text
    -- ^ Address value.
  | LCContract LigoContract
    -- ^ Contract value.
  | LCNat Text
    -- ^ Natural number value.
  | LCTimestamp Text
    -- ^ Timestamp value.
  | LCKeyHash Text
    -- ^ Key hash value.
  | LCKey Text
    -- ^ Key value.
  | LCSignature Text
    -- ^ Signature value.
  | LCBls12_381Fr Text
    -- ^ Bls12_381_fr value.
  | LCBls12_381G1 Text
    -- ^ Bls12_381_g1 value.
  | LCBls12_381G2 Text
    -- ^ Bls12_381_g2 value.
  | LCChainId Text
    -- ^ Chain ID value.
  | LCInt Text
    -- ^ Integer number value.
  | LCInt64 Integer
    -- ^ Int64 number value.
  | LCMutez Text
    -- ^ Mutez value.
  | LCBool Bool
    -- ^ Boolean value.
  | LCUnit
    -- ^ Unit value.
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass (NFData)

-- | A typed Michelson code.
data LigoValueTyCode = LigoValueTyCode
  { lvtcAstTy :: Value
    -- ^ Contract's type.
  , lvtcMichelineRepr :: Value
    -- ^ Micheline representaion.
  } deriving stock (Generic, Show, Eq, Data)
    deriving anyclass (NFData)
    deriving (FromJSON) via LigoValueJSON 4 LigoValueTyCode

-- | A raw Michelson code.
data LigoValueMichelson
  = LVMUntypedCode Value
    -- ^ Untyped code.
  | LVMTyCode LigoValueTyCode
    -- ^ Typed code.
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass (NFData)

---------------
--- Helpers ---
---------------

-- TODO: replace it with @LigoJSON@.

-- | A convenient type that allows deriving @FromJSON@ instances.
-- @n@ stands for the number of capital letters in the type.
newtype LigoValueJSON (n :: Nat) a = LigoValueJSON a

instance (Generic a, GFromJSON Zero (Rep a), KnownNat n) => FromJSON (LigoValueJSON n a) where
  parseJSON = fmap LigoValueJSON . genericParseJSON defaultOptions
    { fieldLabelModifier = genericDrop (natVal (Proxy @n) + 1) . toSnakeCase
    }

-- | Capitalize the first letter of the string.
toUpperCase :: String -> String
toUpperCase = \case
  x : xs -> toUpper x : xs
  [] -> []

-- | @surround left right sep lst@ will add separators @sep@ into
-- the list @lst@ and surrond the result with @left@ and @right@.
--
-- E.g. @surround "[" "]" ";" [1, 2, 3] == "[1;2;3]"@.
surround :: (Buildable a) => Doc -> Doc -> Doc -> [a] -> Doc
surround left right sep lst = mconcat $ left : intersperse sep (build <$> lst) ++ [right]

-- | Tries to convert a record value into the tuple.
toTupleMaybe :: LigoValue -> Maybe [LigoValue]
toTupleMaybe (LVRecord record) = forM [0..HM.size record - 1] \i -> HM.lookup (LLabel $ pretty i) record
toTupleMaybe _ = Nothing

-- | Tries to pick a record's field type by its name.
-- Returns @LigoTypeUnresolved@ if passed type is not a record
-- or the given field does not exist.
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
  T.VChainId chainId -> mkConstant (LCChainId $ pretty chainId)
  T.VContract addr (T.SomeEpc T.EntrypointCall{..}) ->
    mkConstant
      (LCContract $ LigoContract (pretty addr)
      (guard (not $ U.isDefEpName epcName) >> pure (pretty epcName)))
  T.VInt n -> mkConstant (LCInt $ pretty n)
  T.VNat n -> mkConstant (LCNat $ pretty n)
  T.VString str -> mkConstant (LCString $ pretty str)
  T.VBytes bts -> mkConstant (LCBytes [int||0x#{hexF bts}|])
  T.VMutez mu -> mkConstant (LCMutez $ show $ T.unMutez mu)
  T.VBool bVal -> mkConstant (LCBool bVal)
  T.VKeyHash ha -> mkConstant (LCKeyHash $ pretty ha)
  T.VTimestamp (T.Timestamp (nominalDiffTimeToSeconds -> (MkFixed time))) ->
     mkConstant (LCTimestamp $ pretty (time `div` 1e12))
  T.VAddress T.EpAddress'{..} ->
    mkConstant
      (LCContract $ LigoContract (pretty eaAddress)
      (guard (not $ U.isDefEpName eaEntrypoint) >> pure (pretty eaEntrypoint)))
  T.VBls12381Fr bls -> mkConstant (LCBls12_381Fr $ [int||0x#{hexF $ toMichelsonBytes bls}|])
  T.VBls12381G1 bls -> mkConstant (LCBls12_381G1 $ [int||0x#{hexF $ toMichelsonBytes bls}|])
  T.VBls12381G2 bls -> mkConstant (LCBls12_381G2 $ [int||0x#{hexF $ toMichelsonBytes bls}|])
  _ -> Nothing
  where
    mkConstant :: LigoConstant -> Maybe LigoValue
    mkConstant = Just . LVCt

-- | Extract the type from @LigoOrMichValue@.
getLigoType :: LigoOrMichValue -> LigoType
getLigoType = \case
  LigoValue typ _ -> typ
  MichValue typ _ -> typ
  ToBeComputed -> LigoTypeUnresolved

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
      [String ctor, label, _]
        | ctor == "Label" -> LLabel <$> parseJSON label
      _ -> fail "Expected 2 elements array"

-- An explanation about constants in @drop@s.
-- First @drop@ (the rightmosts) removes type abbreviation in constructor.
-- The second one @drop 1@ removes an underscore. This is because
-- @toSnakeCase@ function adds an underscore if string starts with a capital letter
-- (e.g. SomeCtor -> _some_ctor).

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

-- | Prettifies @LigoValue@ using target dialect and a printing mode:
-- 1. @DpmNormal@. A pretty-printer mode for values in variables pane.
--    It doesn't depend on the dialect.
-- 2. @DpmEvaluated@. A pretty-printer mode for values that would be obtained
--    from @Copy Value@ button. The format matches with the target dialect.
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
      pure (_ltcParameters, T.toLower _ltcInjection)

    innerTypeFromConstant = LigoType (listToMaybe . fst =<< typesFromConstantAndName)
    (keyType, valueType) = maybe (LigoType Nothing, LigoType Nothing) (bimap LigoType LigoType)
      case typesFromConstantAndName of
        Just ([k, v], _) -> pure (Just k, Just v)
        _ -> Nothing

-- | A shortcut for @buildLigoValue' Caml DpmNormal@.
buildLigoValue :: LigoType -> LigoValue -> Doc
buildLigoValue = buildLigoValue' Caml DpmNormal

-- | Prettifies @LigoConstant@ using target dialect and a printing mode:
-- 1. @DpmNormal@. A pretty-printer mode for values in variables pane.
--    It doesn't depend on the dialect.
-- 2. @DpmEvaluated@. A pretty-printer mode for values that would be obtained
--    from @Copy Value@ button. The format matches with the target dialect.
buildConstant' :: Lang -> DebugPrintMode -> LigoConstant -> Doc
buildConstant' lang mode = \case
  LCString str -> build $ Debug.show @Text str
  LCBytes bts -> build bts
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
  LCBls12_381G1 bls -> build bls
  LCBls12_381G2 bls -> build bls
  LCBls12_381Fr bls -> build bls
  LCChainId chainId -> build chainId
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

-- | A shortcut for @buildConstant' Caml DpmNormal@.
buildConstant :: LigoConstant -> Doc
buildConstant = buildConstant' Caml DpmNormal

instance Buildable LigoContract where
  build LigoContract{..} =  [int||#{lcAddress}#{ep}|]
    where
      ep = maybe ("" :: Builder) [int|m|(#{id})|] lcEntrypoint
