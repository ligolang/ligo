{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# LANGUAGE DuplicateRecordFields, UndecidableInstances #-}

-- | Types related to DAP.
module Language.LIGO.Debugger.Handlers.Types
  ( LigoLaunchRequest (..)
  , LigoContractEnv (..)
  , VotingPowersConfig (..)
  , SimpleVotingPowersInfo (..)
  , LigoResolveConfigFromLigoRequest (..)
  , LigoInitializeLanguageServerStateRequest (..)
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

-- | Parameters for launching the debugger.
data LigoLaunchRequest = LigoLaunchRequest
  { -- | Launches the contract without enabling debugging.
    noDebug             :: Maybe Bool

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

    -- | Contract's environment.
  , contractEnv         :: Maybe LigoContractEnv
  } deriving stock (Eq, Show, Generic)
    deriving Buildable via (GenericBuildable LigoLaunchRequest)

-- | Tezos environment used while stepping through a contract.
data LigoContractEnv = LigoContractEnv
  { now          :: Maybe $ MichelsonJson Timestamp
    -- ^ Current time.
  , balance      :: Maybe $ MichelsonJson Mutez
    -- ^ Current balance.
  , amount       :: Maybe $ MichelsonJson Mutez
    -- ^ Amount of the current transaction.
  , self         :: Maybe ContractAddress
    -- ^ Address of the current contract.
  , source       :: Maybe L1Address
    -- ^ Address of the contract that initiated the current execution.
  , sender       :: Maybe L1Address
    -- ^ Address of the contract that initiated the current internal execution.
  , chainId      :: Maybe $ MichelsonJson ChainId
    -- ^ The identifier of the chain on which this contract is executed.
  , level        :: Maybe $ MichelsonJson Natural
    -- ^ The current block level.
  , votingPowers :: Maybe VotingPowersConfig
    -- ^ The voting power for the current contract.
  } deriving stock (Eq, Show, Generic)
    deriving Buildable via (GenericBuildable LigoContractEnv)

instance Default LigoContractEnv where
  def = LigoContractEnv
    Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing

-- | A message from a request indicating to resolve a configuration from LIGO.
data LigoResolveConfigFromLigoRequest = LigoResolveConfigFromLigoRequest
  { configPath :: FilePath
    -- ^ A path to the LIGO file containing the debugger configuration.
  } deriving stock (Eq, Show, Generic)

-- | A message from a request to initialize the language server state.
-- This is not related to LIGO LSP.
data LigoInitializeLanguageServerStateRequest = LigoInitializeLanguageServerStateRequest
  { binaryPath :: Maybe FilePath
    -- ^ A file path to the LIGO compiler.
  } deriving stock (Eq, Show, Generic)

-- | A request to initialize the logger.
data LigoInitializeLoggerRequest = LigoInitializeLoggerRequest
  { file   :: FilePath
    -- ^ Name of the log file.
  , logDir :: Maybe FilePath
    -- ^ The directory to be used for logging.
  } deriving stock (Eq, Show, Generic)

-- | A request to set configuration arguments.
data LigoSetLigoConfigRequest = LigoSetLigoConfigRequest
  { binaryPath :: Maybe FilePath
    -- ^ A path to the LIGO binary.
  , maxSteps   :: Maybe Word64
    -- ^ The maximum number of steps before the debugging is interrupted. An empty
    -- value indicates infinite steps.
  } deriving stock (Eq, Show, Generic)

-- | A request to set the path to the contract being debugged.
data LigoSetProgramPathRequest = LigoSetProgramPathRequest
  { program :: FilePath
    -- ^ The path to the contract being debugged.
  } deriving stock (Eq, Show, Generic)

-- | A request to validate the module name in the contract being debugged.
data LigoValidateModuleNameRequest = LigoValidateModuleNameRequest
  { moduleName :: Text
    -- ^ The module path to be validated.
  } deriving stock (Eq, Show, Generic)

-- | A request to fetch a contract's metadata.
data LigoGetContractMetadataRequest = LigoGetContractMetadataRequest
  { moduleName :: Text
    -- ^ LIGO module name to be used.
  } deriving stock (Eq, Show, Generic)

-- | A request to validate an entry point.
data LigoValidateEntrypointRequest = LigoValidateEntrypointRequest
  { pickedEntrypoint :: Text
    -- ^ A @LIGO@ entrypoint that will be used.
  } deriving stock (Eq, Show, Generic)

-- | A request to validate some user-provided value.
data LigoValidateValueRequest = LigoValidateValueRequest
  { value            :: Text
    -- ^ Value to check.
  , category         :: Text
    -- ^ Category of the value (e.g. @parameter@).
  , valueLang        :: Text
    -- ^ Language of value (@LIGO@ or @Michelson@)
  } deriving stock (Eq, Show, Generic)

-- | A request to validate a LIGO configuration.
data LigoValidateConfigRequest = LigoValidateConfigRequest
  { parameter     :: Text
    -- ^ Parameter value to be validated.
  , parameterLang :: Text
    -- ^ Whether to validate the input parameter interpreting it as a Michelson or
    -- LIGO value.
  , storage       :: Text
    -- ^ Storage value to be validated.
  , storageLang   :: Text
    -- ^ Whether to validate the input storage interpreting it as a Michelson or
    -- LIGO value.
  } deriving stock (Eq, Show, Generic)

-- | Response of the @LigoSetProgramPathRequest@.
data LigoSetProgramPathResponse = LigoSetProgramPathResponse
  { moduleNames :: [(Text, Text)]
    -- ^ A list of module name and its prettified version.
  } deriving stock (Show, Eq, Generic)

-- | Response of validation requests (e.g. @LigoValidateEntrypointRequest@).
data LigoValidateResponse
  = LigoValidateOk
    -- ^ Validation is OK.
  | LigoValidateFailed Text
    -- ^ If there are issues, their description.
    -- This is a build-in that VSCode accounts for.
    deriving stock (Show, Eq, Generic)

-- | Converts @Either Text ()@ to @LigoValidateResponse@.
ligoValidateFromEither :: Either Text () -> LigoValidateResponse
ligoValidateFromEither = \case
  Left e   -> LigoValidateFailed e
  Right () -> LigoValidateOk

-- | Represents contract's metadata.
data ContractMetadata = ContractMetadata
  { parameterMichelsonType :: MD.JsonFromBuildable U.ParameterType
    -- ^ Contract's parameter type.
  , storageMichelsonType   :: MD.JsonFromBuildable U.Ty
    -- ^ Contract's storage type.
  , entrypoints            :: MD.Entrypoints
    -- ^ A list of Michelson entry points.
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
  , ''LigoInitializeLanguageServerStateRequest
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

instance IsRequest LigoInitializeLanguageServerStateRequest where
  type CommandFor LigoInitializeLanguageServerStateRequest = "initializeLanguageServerState"
  type ResponseFor LigoInitializeLanguageServerStateRequest = ()

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
