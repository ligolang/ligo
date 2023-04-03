{-# LANGUAGE DeriveDataTypeable, UndecidableInstances #-}

-- | Ligo Value representation from
-- https://ligolang.org/assets/files/values.schema-11c1c8c46fa17d8c9eae1cf28c49a307.json
-- json-schema.
module Language.LIGO.Debugger.CLI.Types.LigoValue
  ( module Language.LIGO.Debugger.CLI.Types.LigoValue
  ) where

import Debug qualified

import Control.Exception (throw)
import Data.Aeson
  (FromJSON (parseJSON), GFromJSON, Options (..), SumEncoding (TwoElemArray), Value (..), Zero,
  defaultOptions, genericParseJSON, withText)
import Data.Aeson.Types (Parser)
import Data.Char (toUpper)
import Data.Data (Data)
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Time.Clock.System (SystemTime (MkSystemTime), systemToUTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Fmt (Buildable (build), Builder, hexF, pretty)
import GHC.Generics (Rep)
import GHC.TypeLits (Nat)
import Text.Interpolation.Nyan

import AST (Lang (Caml, Js))
import Cli.Json
  (LigoTableField (..), LigoTypeConstant (..), LigoTypeContent (LTCConstant, LTCRecord, LTCSum),
  LigoTypeExpression (..), LigoTypeTable (..), guardOneElemList, toSnakeCase)
import Cli.Json qualified as Cli

import Morley.Debugger.Core (DebugPrint (DebugPrint), DebugPrintMode (..))
import Morley.Michelson.Typed (Constrained (SomeValue), SomeValue)

import Language.LIGO.Debugger.CLI.Types

-------------
--- Types ---
-------------

data LigoOrMichValue
  = LigoValue LigoType LigoValue
  | MichValue SomeValue
  deriving stock (Generic, Show, Eq)

data LigoValue
  = LVCt LigoConstant
  | LVList [LigoValue]
  | LVRecord (HashMap Text LigoValue)
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
  { lfvRecName :: Maybe Cli.LigoVariable
  , lfvOrigLambda :: Value
  , lfvBody :: Value
  , lfvArgBinder :: Cli.LigoVariable
  , lfvArgMutFlag :: LigoMutFlag
  , lfvEnv :: [(Cli.LigoVariable, LigoEnvItem)]
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
  | LCBls12_381G1 Text
  | LCBls12_381G2 Text
  | LCBls12_381Fr Text
  | LCChainId Text
  | LCInt Text
  | LCInt64 Integer
  | LCMutez Text
  | LCBool Bool
  | LCUnit
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

surround :: (Buildable a) => Builder -> Builder -> Builder -> [a] -> Builder
surround left right sep lst = mconcat $ left : intersperse sep (build <$> lst) ++ [right]

toTupleMaybe :: LigoValue -> Maybe [LigoValue]
toTupleMaybe (LVRecord record) = forM [0..HM.size record - 1] \i -> HM.lookup (pretty i) record
toTupleMaybe _ = Nothing

getRecordOrderMb :: LigoType -> LigoValue -> Maybe [(Text, (LigoType, LigoValue))]
getRecordOrderMb ligoType = \case
  LVRecord record -> do
    LTCRecord LigoTypeTable{..} <- _lteTypeContent <$> unLigoType ligoType

    valsAndNums <-
      forM (toPairs record) \(k, v) -> do
        field <- HM.lookup k _lttFields
        let num = _ltfDeclPos field
        let innerType = LigoType (Just $ _ltfAssociatedType field)
        pure ((k, (innerType, v)), num)

    pure $ fst <$> sortBy (comparing snd) valsAndNums
  _ -> Nothing

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
    } val <|> parseFunction val
    where
      processCtor :: String -> String
      processCtor = \case
        "Ct" -> "Constant"
        "MichelsonContract" -> "ContractCode"
        "Gen" -> "Generator"
        other -> other

      parseFunction :: Value -> Parser LigoValue
      parseFunction = withText "Function" \txt -> guard (txt == "<fun>") >> pure LVFuncVal

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
    MichValue mich -> build mich

instance Buildable (DebugPrint (Lang, LigoOrMichValue)) where
  build (DebugPrint mode (lang, val)) =
    case val of
      MichValue (SomeValue michValue) -> build (DebugPrint mode michValue)
      LigoValue ligoType ligoValue -> buildLigoValue' lang mode ligoType ligoValue

buildLigoValue' :: Lang -> DebugPrintMode -> LigoType -> LigoValue -> Builder
buildLigoValue' lang mode ligoType = \case
  LVCt constant -> buildConstant' lang mode constant
  LVList values ->
    let listWithSep sep = surround "[" "]" sep (buildLigoValue' lang mode innerTypeFromConstant <$> values) in
    case (mode, lang) of
      (DpmNormal, _) -> listWithSep "; "
      (DpmEvaluated, Caml) -> listWithSep "; "
      (DpmEvaluated, Js) -> [int||list(#{listWithSep ", "})|]
  val@(LVRecord record) ->
    case toTupleMaybe val of
      Just tuple ->
        let
          innerTypes = maybe (repeat $ LigoType Nothing) (map (fst . snd)) (getRecordOrderMb ligoType val)
          builtTuple lb rb = surround lb rb ", " (zipWith (buildLigoValue' lang mode) innerTypes tuple)
        in
          case (mode, lang) of
            (DpmNormal, _) -> builtTuple "(" ")"
            (DpmEvaluated, Caml) -> builtTuple "(" ")"
            (DpmEvaluated, Js) -> builtTuple "[" "]"
      Nothing ->
        let
          buildRecord :: [(Text, (LigoType, LigoValue))] -> Builder
          buildRecord keysAndValues =
            let
              recordWithSep sep =
                surround @Builder "{" "}" sep
                  (map (\(l, (t, v)) -> [int||#{l} = #{buildLigoValue' lang mode t v}|]) keysAndValues)
            in
            case (mode, lang) of
              (DpmNormal, _) -> recordWithSep "; "
              (DpmEvaluated, Caml) -> recordWithSep "; "
              (DpmEvaluated, Js) -> recordWithSep ", "
        in
          case getRecordOrderMb ligoType val of
            Just order -> buildRecord order
            Nothing -> buildRecord $ map (\(t, v) -> (t, (LigoType Nothing, v))) (HM.toList record)
  LVConstructor (ctor, value) ->
    let
      innerType = LigoType do
        LTCSum LigoTypeTable{..} <- _lteTypeContent <$> unLigoType ligoType
        _ltfAssociatedType <$> HM.lookup ctor _lttFields
    in [int||#{ctor} (#{buildLigoValue' lang mode innerType value})|]
  LVSet values -> case mode of
    DpmNormal -> surround "{" "}" "; " (buildLigoValue' lang mode innerTypeFromConstant <$> values)
    DpmEvaluated -> [int||Set.literal(#{buildLigoValue' lang mode ligoType $ LVList values})|]
  LVMap keyValues -> case mode of
    DpmNormal ->
      surround @Builder "[" "]" "; "
        (map (\(k, v) -> [int||#{buildLigoValue' lang mode keyType k} -> #{buildLigoValue' lang mode valueType v}|]) keyValues)
    DpmEvaluated ->
      let
        mkTuple :: (LigoValue, LigoValue) -> LigoValue
        mkTuple (fstVal, sndVal) = LVRecord $ HM.fromList
          [ ("0", fstVal)
          , ("1", sndVal)
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

buildLigoValue :: LigoType -> LigoValue -> Builder
buildLigoValue = buildLigoValue' Caml DpmNormal

buildConstant' :: Lang -> DebugPrintMode -> LigoConstant -> Builder
buildConstant' lang mode = \case
  LCString str -> build $ Debug.show @Text str
  LCBytes bts -> [int||0x#{buildBytes bts}|]
  LCAddress addr -> case (mode, lang) of
    (DpmNormal, _) -> build addr
    (DpmEvaluated, Caml) -> [int||(#{addr} : address)|]
    (DpmEvaluated, Js) -> [int||(#{addr} as address)|]
  LCContract contract -> build contract
  LCNat n -> [int||#{n}n|]
  LCTimestamp timestamp -> case (mode, lang) of
    (DpmNormal, _) -> [int||timestamp(#{buildTimestamp timestamp})|]
    (DpmEvaluated, Caml) -> [int||(#{buildTimestamp timestamp} : timestamp)|]
    (DpmEvaluated, Js) -> [int||(#{buildTimestamp timestamp} as timestamp)|]
  LCKeyHash keyHash -> build keyHash
  LCKey key -> case (mode, lang) of
    (DpmNormal, _) -> build key
    (DpmEvaluated, Caml) -> [int||("#{key}" : key)|]
    (DpmEvaluated, Js) -> [int||("#{key}" as key)|]
  LCSignature sign -> case (mode, lang) of
    (DpmNormal, _) -> build sign
    (DpmEvaluated, Caml) -> [int||("#{sign}" : signature)|]
    (DpmEvaluated, Js) -> [int||("#{sign}" as signature)|]
  LCBls12_381G1 bts -> [int||0x#{buildBytes bts}|]
  LCBls12_381G2 bts -> [int||0x#{buildBytes bts}|]
  LCBls12_381Fr bts -> [int||0x#{buildBytes bts}|]
  LCChainId bts -> buildChainId bts
  LCInt n -> build n
  LCInt64 n -> build n
  LCMutez n -> [int||#{n}mutez|]
  LCBool bl -> build bl
  LCUnit -> case (mode, lang) of
    (DpmNormal, _) -> "()"
    (DpmEvaluated, Caml) -> "()"
    (DpmEvaluated, Js) -> "unit"
  where
    buildTimestamp :: Text -> Builder
    buildTimestamp (readMaybe @Int64 . toString -> Just n) =
      MkSystemTime n 0
        & systemToUTCTime
        & iso8601Show
        & build
    buildTimestamp other =
      throw (PluginCommunicationException [int||Unexpected string in timestamp: #{other}|])

    buildBytes :: Text -> Builder
    buildBytes bts = hexF @ByteString $ encodeUtf8 bts

    buildChainId :: Text -> Builder
    buildChainId = build . T.UnsafeChainId . encodeUtf8

buildConstant :: LigoConstant -> Builder
buildConstant = buildConstant' Caml DpmNormal

instance Buildable LigoContract where
  build LigoContract{..} = [int||#{lcAddress}(#{lcEntrypoint})|]
