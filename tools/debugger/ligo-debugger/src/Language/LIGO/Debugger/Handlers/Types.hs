{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# LANGUAGE DuplicateRecordFields, UndecidableInstances #-}

-- | Types related to DAP.
module Language.LIGO.Debugger.Handlers.Types
  ( LigoLaunchRequest (..)
  , LigoContractEnv (..)
  , VotingPowersConfig (..)
  , SimpleVotingPowersInfo (..)
  , LigoResolveConfigFromLigoRequest (..)
  , LigoInitializeLoggerRequest (..)
  , LigoSetLigoConfigRequest (..)
  , LigoSetProgramPathRequest (..)
  , LigoValidateModuleNameRequest (..)
  , LigoGetContractMetadataRequest (..)
  , LigoValidateEntrypointRequest (..)
  , LigoValidateValueRequest (..)
  , LigoValidateConfigRequest (..)

  , LigoValidateResponse (..)
  , LigoSetProgramPathResponse (..)
  , ligoValidateFromEither
  , ContractMetadata (..)
  ) where

import Data.Aeson (KeyValue ((.=)), ToJSON (..), Value, object)
import Data.Aeson.TH (deriveFromJSON, deriveToJSON)
import Data.Aeson.Types qualified as Aeson
import Data.Default (Default (def))
import Data.Map qualified as M
import Fmt.Buildable (Buildable, GenericBuildable (..), pretty)

import Protocol.DAP (IsRequest (..))

import Morley.Debugger.DAP.LanguageServer qualified as MD
import Morley.Debugger.DAP.Types (MichelsonJson (..))
import Morley.Debugger.DAP.Types.Morley (SimpleVotingPowersInfo (..), VotingPowersConfig (..))
import Morley.Michelson.Untyped qualified as U
import Morley.Tezos.Address (ContractAddress, L1Address)
import Morley.Tezos.Core (ChainId, Mutez, Timestamp)

data LigoLaunchRequest = LigoLaunchRequest
  { noDebug             :: Maybe Bool

    -- | Path to logs directory.
  , logDir              :: Maybe Text

    -- | Path to LIGO source file.
  , program             :: Maybe FilePath

    -- | Name of the entrypoint to debug.
  , entrypoint          :: Maybe Text

    -- | Storage value for contract.
  , storage             :: Maybe Text

    -- | Module name to compile the contract.
  , moduleName          :: Maybe Text

    -- | Parameter value for contract.
  , parameter           :: Maybe Text

  , contractEnv         :: Maybe LigoContractEnv
  } deriving stock (Eq, Show, Generic)
    deriving Buildable via (GenericBuildable LigoLaunchRequest)

data LigoContractEnv = LigoContractEnv
  { now          :: Maybe $ MichelsonJson Timestamp
  , balance      :: Maybe $ MichelsonJson Mutez
  , amount       :: Maybe $ MichelsonJson Mutez
  , self         :: Maybe ContractAddress
  , source       :: Maybe L1Address
  , sender       :: Maybe L1Address
  , chainId      :: Maybe $ MichelsonJson ChainId
  , level        :: Maybe $ MichelsonJson Natural
  , votingPowers :: Maybe VotingPowersConfig
  } deriving stock (Eq, Show, Generic)
    deriving Buildable via (GenericBuildable LigoContractEnv)

instance Default LigoContractEnv where
  def = LigoContractEnv
    Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing

data LigoResolveConfigFromLigoRequest = LigoResolveConfigFromLigoRequest
  { configPath :: FilePath
  } deriving stock (Eq, Show, Generic)

data LigoInitializeLoggerRequest = LigoInitializeLoggerRequest
  { file   :: FilePath
  , logDir :: Maybe FilePath
  } deriving stock (Eq, Show, Generic)

data LigoSetLigoConfigRequest = LigoSetLigoConfigRequest
  { binaryPath :: Maybe FilePath
  , maxSteps   :: Maybe Word64
  } deriving stock (Eq, Show, Generic)

data LigoSetProgramPathRequest = LigoSetProgramPathRequest
  { program :: FilePath
  } deriving stock (Eq, Show, Generic)

data LigoValidateModuleNameRequest = LigoValidateModuleNameRequest
  { moduleName :: Text
  } deriving stock (Eq, Show, Generic)

data LigoGetContractMetadataRequest = LigoGetContractMetadataRequest
  { moduleName :: Text
    -- ^ LIGO module name to be used.
  } deriving stock (Eq, Show, Generic)

data LigoValidateEntrypointRequest = LigoValidateEntrypointRequest
  { pickedEntrypoint :: Text
    -- ^ A @LIGO@ entrypoint that will be used.
  } deriving stock (Eq, Show, Generic)

data LigoValidateValueRequest = LigoValidateValueRequest
  { value            :: Text
    -- ^ Value to check.
  , category         :: Text
    -- ^ Category of the value (e.g. @parameter@).
  , valueLang        :: Text
    -- ^ Language of value (@LIGO@ or @Michelson@)
  } deriving stock (Eq, Show, Generic)

data LigoValidateConfigRequest = LigoValidateConfigRequest
  { parameter     :: Text
  , parameterLang :: Text
  , storage       :: Text
  , storageLang   :: Text
  } deriving stock (Eq, Show, Generic)

data LigoSetProgramPathResponse = LigoSetProgramPathResponse
  { moduleNames :: [(Text, Text)]
  } deriving stock (Show, Eq, Generic)

data LigoValidateResponse
  = LigoValidateOk
  | LigoValidateFailed Text
    -- ^ If there are issues, their description.
    -- This is a build-in that VSCode accounts for.
    deriving stock (Show, Eq, Generic)

ligoValidateFromEither :: Either Text () -> LigoValidateResponse
ligoValidateFromEither = \case
  Left e   -> LigoValidateFailed e
  Right () -> LigoValidateOk

data ContractMetadata = ContractMetadata
  { parameterMichelsonType :: MD.JsonFromBuildable U.ParameterType
  , storageMichelsonType   :: MD.JsonFromBuildable U.Ty
  , entrypoints            :: MD.Entrypoints
  } deriving stock (Eq, Generic, Show)
    deriving Buildable via (GenericBuildable ContractMetadata)

instance ToJSON LigoValidateResponse where
  toJSON = \case
    LigoValidateOk         -> Aeson.Null
    LigoValidateFailed err -> Aeson.object [ "errorMessage" Aeson..= err ]

-- @ToJSON@ instance for @MichelsonJSON@ is just a
-- @error "not implemented"@. So, we need to manually define
-- this instance here.
instance ToJSON LigoContractEnv where
  toJSON LigoContractEnv{..} = object
    [ "now" .= (pretty @_ @Text . unMichelsonJson <$> now)
    , "balance" .= (unMichelsonJson <$> balance)
    , "amount" .= (unMichelsonJson <$> amount)
    , "self" .= self
    , "source" .= source
    , "sender" .= sender
    , "chainId" .= (unMichelsonJson <$> chainId)
    , "level" .= (unMichelsonJson <$> level)
    , "votingPowers" .= (toJSONVotingPowers <$> votingPowers)
    ]
    where
      toJSONVotingPowers :: VotingPowersConfig -> Value
      toJSONVotingPowers = \case
        SimpleVotingPowers SimpleVotingPowersInfo{..} -> object
          [ "kind" .= ("simple" :: Text)
          , "contents" .= (unMichelsonJson <$> M.mapKeysMonotonic unMichelsonJson contents)
          ]

concatMapM (deriveToJSON Aeson.defaultOptions)
  [ ''ContractMetadata
  , ''LigoSetProgramPathResponse
  , ''LigoLaunchRequest
  ]

concatMapM (deriveFromJSON Aeson.defaultOptions)
  [ ''LigoContractEnv
  , ''LigoLaunchRequest
  , ''LigoResolveConfigFromLigoRequest
  , ''LigoInitializeLoggerRequest
  , ''LigoSetLigoConfigRequest
  , ''LigoSetProgramPathRequest
  , ''LigoValidateModuleNameRequest
  , ''LigoGetContractMetadataRequest
  , ''LigoValidateEntrypointRequest
  , ''LigoValidateValueRequest
  , ''LigoValidateConfigRequest
  ]

instance IsRequest LigoLaunchRequest where
  type CommandFor LigoLaunchRequest = "launch"
  type ResponseFor LigoLaunchRequest = ()

instance IsRequest LigoResolveConfigFromLigoRequest where
  type CommandFor LigoResolveConfigFromLigoRequest = "resolveConfigFromLigo"
  type ResponseFor LigoResolveConfigFromLigoRequest = LigoLaunchRequest

instance IsRequest LigoInitializeLoggerRequest where
  type CommandFor LigoInitializeLoggerRequest = "initializeLogger"
  type ResponseFor LigoInitializeLoggerRequest = ()

instance IsRequest LigoSetLigoConfigRequest where
  type CommandFor LigoSetLigoConfigRequest = "setLigoConfig"
  type ResponseFor LigoSetLigoConfigRequest = ()

instance IsRequest LigoSetProgramPathRequest where
  type CommandFor LigoSetProgramPathRequest = "setProgramPath"
  type ResponseFor LigoSetProgramPathRequest = LigoSetProgramPathResponse

instance IsRequest LigoValidateModuleNameRequest where
  type CommandFor LigoValidateModuleNameRequest = "validateModuleName"
  type ResponseFor LigoValidateModuleNameRequest = LigoValidateResponse

instance IsRequest LigoGetContractMetadataRequest where
  type CommandFor LigoGetContractMetadataRequest = "getContractMetadata"
  type ResponseFor LigoGetContractMetadataRequest = ContractMetadata

instance IsRequest LigoValidateEntrypointRequest where
  type CommandFor LigoValidateEntrypointRequest = "validateEntrypoint"
  type ResponseFor LigoValidateEntrypointRequest = LigoValidateResponse

instance IsRequest LigoValidateValueRequest where
  type CommandFor LigoValidateValueRequest = "validateValue"
  type ResponseFor LigoValidateValueRequest = LigoValidateResponse

instance IsRequest LigoValidateConfigRequest where
  type CommandFor LigoValidateConfigRequest = "validateConfig"
  type ResponseFor LigoValidateConfigRequest = ()
