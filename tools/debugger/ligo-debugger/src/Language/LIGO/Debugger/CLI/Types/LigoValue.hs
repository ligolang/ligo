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
import Data.ByteString qualified as BS
import Data.Char (toUpper)
import Data.Data (Data)
import Data.HashMap.Strict qualified as HM
import Data.Time.Clock.System (SystemTime (MkSystemTime), systemToUTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Vector qualified as V
import Fmt (Buildable (build), Builder, pretty)
import GHC.Generics (Rep)
import GHC.TypeLits (Nat)
import Text.Interpolation.Nyan
import Text.Printf (printf)

import AST (Lang (Caml, Js))
import Cli.Json
  (LigoTableField (..), LigoTypeConstant (..), LigoTypeContent (LTCConstant, LTCRecord, LTCSum),
  LigoTypeExpression (..), LigoTypeTable (..), guardOneElemList, toSnakeCase)
import Cli.Json qualified as Cli

import Morley.Debugger.Core (DebugPrint (DebugPrint))
import Morley.Michelson.Typed (Constrained (SomeValue), SomeValue)

import Language.LIGO.Debugger.CLI.Types

-----------------------------
--- Types
-----------------------------

data LigoOrMichValue
  = LigoValue LigoValue
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

-----------------------------
--- Helpers
-----------------------------

newtype LigoValueJSON (n :: Nat) a = LigoValueJSON a

instance (Generic a, GFromJSON Zero (Rep a), KnownNat n) => FromJSON (LigoValueJSON n a) where
  parseJSON = fmap LigoValueJSON . genericParseJSON defaultOptions
    { fieldLabelModifier = genericDrop (natVal (Proxy @n) + 1) . toSnakeCase
    }

guardOneElemList :: Text -> Value -> Parser ()
guardOneElemList expected = withArray (toString expected) \arr -> do
  case V.length arr of
    1 -> do
      String ctor <- pure $ V.unsafeIndex arr 0
      guard (ctor == expected)
    len -> fail $ "Expected array of size 1, got " <> show len

toUpperCase :: String -> String
toUpperCase = \case
  x : xs -> toUpper x : xs
  [] -> []

surround :: (Buildable a) => Builder -> Builder -> Builder -> [a] -> Builder
surround left right sep lst = mconcat $ left : intersperse sep (build <$> lst) ++ [right]

toTupleMaybe :: LigoValue -> Maybe [LigoValue]
toTupleMaybe (LVRecord record) = forM [0..HM.size record - 1] \i -> HM.lookup (pretty i) record
toTupleMaybe _ = Nothing

-----------------------------
--- Instances
-----------------------------

-----------------------------
--- FromJSON
-----------------------------

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

-----------------------------
--- Buildable
-----------------------------

instance Buildable LigoOrMichValue where
  build = \case
    LigoValue ligoValue -> build ligoValue
    MichValue mich -> build mich

instance Buildable (DebugPrint LigoOrMichValue) where
  build (DebugPrint mode val) =
    case val of
      MichValue (SomeValue michValue) -> build (DebugPrint mode michValue)
      LigoValue ligoValue -> build ligoValue

instance Buildable LigoValue where
  build = \case
    LVCt constant -> build constant
    LVList values -> surround "[" "]" "; " values
    val@(LVRecord record) ->
      case toTupleMaybe val of
        Just tuple -> surround "(" ")" ", " tuple
        Nothing -> surround @Builder "{" "}" "; " (map (\(l, v) -> [int||#{l} = #{v}|]) (HM.toList record))
    LVConstructor (ctor, value) -> [int||#{ctor} (#{value})|]
    LVSet values -> surround "{" "}" " ; " values
    LVMap keyValues -> surround @Builder "[" "]" "; " (map (\(k, v) -> [int||#{k} -> #{v}|]) keyValues)
    LVTypedAddress address -> build address
    LVMichelson _ -> "<internal: contract code>"
    LVMichelsonContract _ -> "<internal: michelson contract>"
    LVGen _ -> "<internal: generator>"
    LVLocation _ -> "<internal: heap location>"
    LVMutation _ -> "<internal: mutation>"
    LVAstContract _ -> "<internal: AST contract>"
    LVFuncVal -> "<fun>"

instance Buildable LigoConstant where
  build = \case
    LCString str -> build $ Debug.show @Text str
    LCBytes bts -> [int||0x#{buildBytes bts}|]
    LCAddress addr -> build addr
    LCContract contract -> build contract
    LCNat n -> [int||#{n}n|]
    LCTimestamp timestamp -> [int||timestamp(#{buildTimestamp timestamp})|]
    LCKeyHash keyHash -> build keyHash
    LCKey key -> build key
    LCSignature sign -> build sign
    LCBls12_381G1 bts -> build bts
    LCBls12_381G2 bts -> build bts
    LCBls12_381Fr bts -> build bts
    LCChainId bts -> build bts
    LCInt n -> build n
    LCInt64 n -> build n
    LCMutez n -> [int||#{n}mutez|]
    LCBool bl -> build bl
    LCUnit -> "()"
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
      buildBytes bts = bts
        & encodeUtf8
        & BS.unpack
        & concatMap (printf @(Word8 -> String) "%02x")
        & build

instance Buildable LigoContract where
  build LigoContract{..} = [int||#{lcAddress}(#{lcEntrypoint})|]
