{-# OPTIONS_GHC -Wno-orphans #-}

-- | Types related to DAP.
module Language.LIGO.Debugger.Handlers.Types
  ( LigoLaunchRequest (..)
  , LigoLaunchRequestArguments (..)
  , LigoContractEnvArguments (..)
  , LigoVotingPowers (..)
  , SimpleVotingPowersInfo (..)
  , LigoInitializeLoggerRequest (..)
  , LigoInitializeLoggerRequestArguments (..)
  , LigoSetLigoConfigRequest (..)
  , LigoSetLigoConfigRequestArguments (..)
  , LigoSetProgramPathRequest (..)
  , LigoSetProgramPathRequestArguments (..)
  , LigoValidateEntrypointRequest (..)
  , LigoValidateEntrypointRequestArguments (..)
  , LigoGetContractMetadataRequest (..)
  , LigoGetContractMetadataRequestArguments (..)
  , LigoValidateValueRequest (..)
  , LigoValidateValueRequestArguments (..)
  , LigoValidateConfigRequest (..)
  , LigoValidateConfigRequestArguments (..)
  , LigoSpecificRequest (..)

  , LigoInitializeLoggerResponse (..)
  , LigoSetLigoConfigResponse (..)
  , LigoSetProgramPathResponse (..)
  , LigoValidateEntrypointResponse (..)
  , ContractMetadata (..)
  , LigoGetContractMetadataResponse (..)
  , LigoValidateValueResponse (..)
  , LigoValidateConfigResponse (..)
  , LigoSpecificResponse (..)
  ) where

import Fmt (Buildable (..), GenericBuildable (..))

import Morley.Debugger.DAP.LanguageServer qualified as MD
import Morley.Debugger.DAP.TH (deriveSum, jsonfyMany)
import Morley.Debugger.DAP.Types (MichelsonJson (MichelsonJson))
import Morley.Michelson.Untyped qualified as U
import Morley.Tezos.Address (ContractAddress, L1Address)
import Morley.Tezos.Core (ChainId, Mutez, Timestamp)
import Morley.Tezos.Crypto (KeyHash)
import Data.Default (Default (def))

data LigoLaunchRequest = LigoLaunchRequest
  { seqLigoLaunchRequest :: Int
  , typeLigoLaunchRequest :: String
  , commandLigoLaunchRequest :: String
  , argumentsLigoLaunchRequest :: LigoLaunchRequestArguments
  } deriving stock (Eq, Show, Generic)
    deriving Buildable via (GenericBuildable LigoLaunchRequest)

data LigoLaunchRequestArguments = LigoLaunchRequestArguments
  { noDebugLigoLaunchRequestArguments :: Maybe Bool

    -- | Path to LIGO source file.
  , programLigoLaunchRequestArguments :: Maybe FilePath

    -- | Name of the michelson entrypoint to debug.
  , michelsonEntrypointLigoLaunchRequestArguments :: Maybe String

    -- | Storage value for contract.
  , storageLigoLaunchRequestArguments :: Maybe String

    -- | Entry point of the contract (@main@ method).
  , entrypointLigoLaunchRequestArguments :: Maybe String

    -- | Parameter value for contract.
  , parameterLigoLaunchRequestArguments :: Maybe String

    -- | Custom contract environment
  , contractEnvLigoLaunchRequestArguments :: Maybe LigoContractEnvArguments
  } deriving stock (Eq, Show, Generic)
    deriving Buildable via (GenericBuildable LigoLaunchRequestArguments)

data LigoContractEnvArguments = LigoContractEnvArguments
  { nowLigoContractEnvArguments :: Maybe $ MichelsonJson Timestamp
  , balanceLigoContractEnvArguments :: Maybe $ MichelsonJson Mutez
  , amountLigoContractEnvArguments :: Maybe $ MichelsonJson Mutez
  , selfLigoContractEnvArguments :: Maybe ContractAddress
  , sourceLigoContractEnvArguments :: Maybe L1Address
  , senderLigoContractEnvArguments :: Maybe L1Address
  , chainIdLigoContractEnvArguments :: Maybe $ MichelsonJson ChainId
  , levelLigoContractEnvArguments :: Maybe $ MichelsonJson Natural
  , votingPowersLigoContractEnvArguments :: Maybe LigoVotingPowers
  } deriving stock (Eq, Show, Generic)
    deriving Buildable via (GenericBuildable LigoContractEnvArguments)

instance Default LigoContractEnvArguments where
  def = LigoContractEnvArguments
    Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing

data LigoVotingPowers
  = SimpleVotingPowers SimpleVotingPowersInfo
  deriving stock (Eq, Show, Generic)
  deriving Buildable via (GenericBuildable LigoVotingPowers)

data SimpleVotingPowersInfo = SimpleVotingPowersInfo
  { contentsSimpleVotingPowersInfo :: Map (MichelsonJson KeyHash) (MichelsonJson Natural)
  } deriving stock (Eq, Show, Generic)
    deriving Buildable via (GenericBuildable SimpleVotingPowersInfo)

-- TODO: move it to morley-debugger
instance Buildable a => Buildable (MichelsonJson a) where
  build (MichelsonJson v) = build v

data LigoInitializeLoggerRequest = LigoInitializeLoggerRequest
  { seqLigoInitializeLoggerRequest :: Int
  , typeLigoInitializeLoggerRequest :: String
  , commandLigoInitializeLoggerRequest :: String
  , argumentsLigoInitializeLoggerRequest :: LigoInitializeLoggerRequestArguments
  } deriving stock (Eq, Show, Generic)
    deriving Buildable via (GenericBuildable LigoInitializeLoggerRequest)

-- | Log directory for the debugger.
data LigoInitializeLoggerRequestArguments = LigoInitializeLoggerRequestArguments
  { fileLigoInitializeLoggerRequestArguments :: FilePath
  , logDirLigoInitializeLoggerRequestArguments :: Maybe FilePath
  } deriving stock (Eq, Show, Generic)
    deriving Buildable via (GenericBuildable LigoInitializeLoggerRequestArguments)

data LigoSetLigoConfigRequest = LigoSetLigoConfigRequest
  { seqLigoSetLigoConfigRequest :: Int
  , typeLigoSetLigoConfigRequest :: String
  , commandLigoSetLigoConfigRequest :: String
  , argumentsLigoSetLigoConfigRequest :: LigoSetLigoConfigRequestArguments
  } deriving stock (Eq, Show, Generic)
    deriving Buildable via (GenericBuildable LigoSetLigoConfigRequest)

-- | Fields fetched from config.
data LigoSetLigoConfigRequestArguments = LigoSetLigoConfigRequestArguments
  { binaryPathLigoSetLigoConfigRequestArguments :: Maybe FilePath
  , maxStepsLigoSetLigoConfigRequestArguments :: Maybe Word64
  } deriving stock (Eq, Show, Generic)
    deriving Buildable via (GenericBuildable LigoSetLigoConfigRequestArguments)

data LigoSetProgramPathRequest = LigoSetProgramPathRequest
  { seqLigoSetProgramPathRequest :: Int
  , typeLigoSetProgramPathRequest :: String
  , commandLigoSetProgramPathRequest :: String
  , argumentsLigoSetProgramPathRequest :: LigoSetProgramPathRequestArguments
  } deriving stock (Eq, Show, Generic)
    deriving Buildable via (GenericBuildable LigoSetProgramPathRequest)

data LigoSetProgramPathRequestArguments = LigoSetProgramPathRequestArguments
  { programLigoSetProgramPathRequestArguments :: FilePath
  } deriving stock (Eq, Show, Generic)
    deriving Buildable via (GenericBuildable LigoSetProgramPathRequestArguments)

data LigoValidateEntrypointRequest = LigoValidateEntrypointRequest
  { seqLigoValidateEntrypointRequest :: Int
  , typeLigoValidateEntrypointRequest :: String
  , commandLigoValidateEntrypointRequest :: String
  , argumentsLigoValidateEntrypointRequest :: LigoValidateEntrypointRequestArguments
  } deriving stock (Eq, Show, Generic)
    deriving Buildable via (GenericBuildable LigoValidateEntrypointRequest)

data LigoValidateEntrypointRequestArguments = LigoValidateEntrypointRequestArguments
  { entrypointLigoValidateEntrypointRequestArguments :: String
  } deriving stock (Eq, Show, Generic)
    deriving Buildable via (GenericBuildable LigoValidateEntrypointRequestArguments)

data LigoGetContractMetadataRequest = LigoGetContractMetadataRequest
  { seqLigoGetContractMetadataRequest :: Int
  , typeLigoGetContractMetadataRequest :: String
  , commandLigoGetContractMetadataRequest :: String
  , argumentsLigoGetContractMetadataRequest :: LigoGetContractMetadataRequestArguments
  } deriving stock (Eq, Show, Generic)
    deriving Buildable via (GenericBuildable LigoGetContractMetadataRequest)

data LigoGetContractMetadataRequestArguments = LigoGetContractMetadataRequestArguments
  { entrypointLigoGetContractMetadataRequestArguments :: String
    -- ^ LIGO entrypoint to be used.
  } deriving stock (Eq, Show, Generic)
    deriving Buildable via (GenericBuildable LigoGetContractMetadataRequestArguments)

data LigoValidateValueRequest = LigoValidateValueRequest
  { seqLigoValidateValueRequest :: Int
  , typeLigoValidateValueRequest :: String
  , commandLigoValidateValueRequest :: String
  , argumentsLigoValidateValueRequest :: LigoValidateValueRequestArguments
  } deriving stock (Eq, Show, Generic)
    deriving Buildable via (GenericBuildable LigoValidateValueRequest)

data LigoValidateValueRequestArguments = LigoValidateValueRequestArguments
  { valueLigoValidateValueRequestArguments :: String
    -- ^ Value to check.
  , categoryLigoValidateValueRequestArguments :: String
    -- ^ Category of the value (e.g. @parameter@).
  , valueLangLigoValidateValueRequestArguments :: String
    -- ^ Language of value (@LIGO@ or @Michelson@)
  , pickedMichelsonEntrypointLigoValidateValueRequestArguments :: Maybe String
    -- ^ Special michelson entrypoint that will be used.
  } deriving stock (Eq, Show, Generic)
    deriving Buildable via (GenericBuildable LigoValidateValueRequestArguments)

data LigoValidateConfigRequest = LigoValidateConfigRequest
  { seqLigoValidateConfigRequest :: Int
  , typeLigoValidateConfigRequest :: String
  , commandLigoValidateConfigRequest :: String
  , argumentsLigoValidateConfigRequest :: LigoValidateConfigRequestArguments
  } deriving stock (Eq, Show, Generic)
    deriving Buildable via (GenericBuildable LigoValidateConfigRequest)

data LigoValidateConfigRequestArguments = LigoValidateConfigRequestArguments
  { michelsonEntrypointLigoValidateConfigRequestArguments :: Maybe String
  , parameterLigoValidateConfigRequestArguments :: String
  , parameterLangLigoValidateConfigRequestArguments :: String
  , storageLigoValidateConfigRequestArguments :: String
  , storageLangLigoValidateConfigRequestArguments :: String
  } deriving stock (Eq, Show, Generic)
    deriving Buildable via (GenericBuildable LigoValidateConfigRequestArguments)

data LigoSpecificRequest
  = InitializeLoggerRequest LigoInitializeLoggerRequest
  | SetLigoConfigRequest LigoSetLigoConfigRequest
  | SetProgramPathRequest LigoSetProgramPathRequest
  | ValidateEntrypointRequest LigoValidateEntrypointRequest
  | GetContractMetadataRequest LigoGetContractMetadataRequest
  | ValidateValueRequest LigoValidateValueRequest
  | ValidateConfigRequest LigoValidateConfigRequest
  deriving stock (Eq, Show, Generic)
  deriving Buildable via (GenericBuildable LigoSpecificRequest)

data LigoInitializeLoggerResponse = LigoInitializeLoggerResponse
  { seqLigoInitializeLoggerResponse :: Int
  , request_seqLigoInitializeLoggerResponse :: Int
  , successLigoInitializeLoggerResponse :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving Buildable via (GenericBuildable LigoInitializeLoggerResponse)

data LigoSetLigoConfigResponse = LigoSetLigoConfigResponse
  { seqLigoSetLigoConfigResponse :: Int
  , request_seqLigoSetLigoConfigResponse :: Int
  , successLigoSetLigoConfigResponse :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving Buildable via (GenericBuildable LigoSetLigoConfigResponse)

data LigoSetProgramPathResponse = LigoSetProgramPathResponse
  { seqLigoSetProgramPathResponse :: Int
  , request_seqLigoSetProgramPathResponse :: Int
  , successLigoSetProgramPathResponse :: Bool
  , entrypointsLigoSetProgramPathResponse :: [String]
  } deriving stock (Show, Eq, Generic)
    deriving Buildable via (GenericBuildable LigoSetProgramPathResponse)

data LigoValidateEntrypointResponse = LigoValidateEntrypointResponse
  { seqLigoValidateEntrypointResponse :: Int
  , request_seqLigoValidateEntrypointResponse :: Int
  , successLigoValidateEntrypointResponse :: Bool
  , messageLigoValidateEntrypointResponse :: Maybe String
    -- ^ If there are issues, their description.
    -- This is a build-in that VSCode accounts for.
  } deriving stock (Show, Eq, Generic)
    deriving Buildable via (GenericBuildable LigoValidateEntrypointResponse)

data ContractMetadata = ContractMetadata
  { parameterMichelsonTypeContractMetadata :: MD.JsonFromBuildable U.ParameterType
  , storageMichelsonTypeContractMetadata   :: MD.JsonFromBuildable U.Ty
  , michelsonEntrypointsContractMetadata   :: MD.Entrypoints
  } deriving stock (Eq, Generic, Show)
    deriving Buildable via (GenericBuildable ContractMetadata)

data LigoGetContractMetadataResponse = LigoGetContractMetadataResponse
  { seqLigoGetContractMetadataResponse :: Int
  , request_seqLigoGetContractMetadataResponse :: Int
  , successLigoGetContractMetadataResponse :: Bool
  , contractMetadataLigoGetContractMetadataResponse :: ContractMetadata
  } deriving stock (Show, Eq, Generic)
    deriving Buildable via (GenericBuildable LigoGetContractMetadataResponse)

data LigoValidateValueResponse = LigoValidateValueResponse
  { seqLigoValidateValueResponse :: Int
  , request_seqLigoValidateValueResponse :: Int
  , successLigoValidateValueResponse :: Bool
  , messageLigoValidateValueResponse :: Maybe String
    -- ^ If there are issues, their description.
    -- This is a build-in that VSCode accounts for.
  } deriving stock (Show, Eq, Generic)
    deriving Buildable via (GenericBuildable LigoValidateValueResponse)

data LigoValidateConfigResponse = LigoValidateConfigResponse
  { seqLigoValidateConfigResponse :: Int
  , request_seqLigoValidateConfigResponse :: Int
  , successLigoValidateConfigResponse :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving Buildable via (GenericBuildable LigoValidateConfigResponse)

data LigoSpecificResponse
  = InitializeLoggerResponse LigoInitializeLoggerResponse
  | SetLigoConfigResponse LigoSetLigoConfigResponse
  | SetProgramPathResponse LigoSetProgramPathResponse
  | ValidateEntrypointResponse LigoValidateEntrypointResponse
  | GetContractMetadataResponse LigoGetContractMetadataResponse
  | ValidateValueResponse LigoValidateValueResponse
  | ValidateConfigResponse LigoValidateConfigResponse
  deriving stock (Eq, Show, Generic)
  deriving Buildable via (GenericBuildable LigoSpecificResponse)

deriveSum
  [ (''LigoSpecificRequest, "Request", "command", [])
  , (''LigoSpecificResponse, "Response", "command", [])
  , (''LigoVotingPowers, "VotingPowers", "kind", [])
  ]
jsonfyMany
  [ ''LigoLaunchRequest
  , ''LigoLaunchRequestArguments
  , ''LigoContractEnvArguments
  ]
