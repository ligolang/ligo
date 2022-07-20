-- | Types related to DAP.
module Language.LIGO.Debugger.Handlers.Types
  ( LigoLaunchRequest (..)
  , LigoLaunchRequestArguments (..)
  , LigoInitializeLoggerRequest (..)
  , LigoInitializeLoggerRequestArguments (..)
  , LigoSetFileRequest (..)
  , LigoSetFileRequestArguments (..)
  , LigoSetEntrypointRequest (..)
  , LigoSetEntrypointRequestArguments (..)
  , LigoCompileContractRequest (..)
  , LigoGetContractMetadataRequest (..)
  , LigoValidateValueRequest (..)
  , LigoValidateValueRequestArguments (..)
  , LigoSpecificRequest (..)

  , LigoInitializeLoggerResponse (..)
  , ContractMetadata (..)
  , LigoSetFileResponse (..)
  , LigoSetEntrypointResponse (..)
  , LigoCompileContractResponse (..)
  , LigoGetContractMetadataResponse (..)
  , LigoValidateValueResponse (..)
  , LigoSpecificResponse (..)
  ) where

import Fmt (Buildable (..), GenericBuildable (..))

import Morley.Debugger.DAP.LanguageServer qualified as MD
import Morley.Debugger.DAP.TH (deriveSum, jsonfyMany)
import Morley.Debugger.DAP.Types ()
import Morley.Michelson.Untyped qualified as U

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

    -- | Parameter value for contract.
  , parameterLigoLaunchRequestArguments :: Maybe String
  } deriving stock (Eq, Show, Generic)
    deriving Buildable via (GenericBuildable LigoLaunchRequestArguments)

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

data LigoSetFileRequest = LigoSetFileRequest
  { seqLigoSetFileRequest :: Int
  , typeLigoSetFileRequest :: String
  , commandLigoSetFileRequest :: String
  , argumentsLigoSetFileRequest :: LigoSetFileRequestArguments
  } deriving stock (Eq, Show, Generic)
    deriving Buildable via (GenericBuildable LigoSetFileRequest)

data LigoSetFileRequestArguments = LigoSetFileRequestArguments
  { fileLigoSetFileRequestArguments :: FilePath
    -- ^ Path to the program being run.
  } deriving stock (Eq, Show, Generic)
    deriving Buildable via (GenericBuildable LigoSetFileRequestArguments)

data LigoSetEntrypointRequest = LigoSetEntrypointRequest
  { seqLigoSetEntrypointRequest :: Int
  , typeLigoSetEntrypointRequest :: String
  , commandLigoSetEntrypointRequest :: String
  , argumentsLigoSetEntrypointRequest :: LigoSetEntrypointRequestArguments
  } deriving stock (Eq, Show, Generic)
    deriving Buildable via (GenericBuildable LigoSetEntrypointRequest)

data LigoSetEntrypointRequestArguments = LigoSetEntrypointRequestArguments
  { entrypointLigoSetEntrypointRequestArguments :: String
    -- ^ Entrypoint to be used in the current program.
  } deriving stock (Eq, Show, Generic)
    deriving Buildable via (GenericBuildable LigoSetEntrypointRequestArguments)

data LigoCompileContractRequest = LigoCompileContractRequest
  { seqLigoCompileContractRequest :: Int
  , typeLigoCompileContractRequest :: String
  , commandLigoCompileContractRequest :: String
  } deriving stock (Eq, Show, Generic)
    deriving Buildable via (GenericBuildable LigoCompileContractRequest)

data LigoGetContractMetadataRequest = LigoGetContractMetadataRequest
  { seqLigoGetContractMetadataRequest :: Int
  , typeLigoGetContractMetadataRequest :: String
  , commandLigoGetContractMetadataRequest :: String
  } deriving stock (Eq, Show, Generic)
    deriving Buildable via (GenericBuildable LigoGetContractMetadataRequest)

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
  , pickedMichelsonEntrypointLigoValidateValueRequestArguments :: Maybe String
    -- ^ Special michelson entrypoint that will be used.
  } deriving stock (Eq, Show, Generic)
    deriving Buildable via (GenericBuildable LigoValidateValueRequestArguments)

data LigoSpecificRequest
  = InitializeLoggerRequest LigoInitializeLoggerRequest
  | SetFileRequest LigoSetFileRequest
  | SetEntrypointRequest LigoSetEntrypointRequest
  | CompileContractRequest LigoCompileContractRequest
  | GetContractMetadataRequest LigoGetContractMetadataRequest
  | ValidateValueRequest LigoValidateValueRequest
  deriving stock (Eq, Show, Generic)
  deriving Buildable via (GenericBuildable LigoSpecificRequest)

data LigoInitializeLoggerResponse = LigoInitializeLoggerResponse
  { seqLigoInitializeLoggerResponse :: Int
  , request_seqLigoInitializeLoggerResponse :: Int
  , successLigoInitializeLoggerResponse :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving Buildable via (GenericBuildable LigoInitializeLoggerResponse)

data ContractMetadata = ContractMetadata
  { parameterMichelsonTypeContractMetadata :: MD.JsonFromBuildable U.ParameterType
  , storageMichelsonTypeContractMetadata   :: MD.JsonFromBuildable U.Ty
  , michelsonEntrypointsContractMetadata   :: MD.Entrypoints
  } deriving stock (Eq, Generic, Show)
    deriving Buildable via (GenericBuildable ContractMetadata)

data LigoSetFileResponse = LigoSetFileResponse
  { seqLigoSetFileResponse :: Int
  , request_seqLigoSetFileResponse :: Int
  , successLigoSetFileResponse :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving Buildable via (GenericBuildable LigoSetFileResponse)

data LigoSetEntrypointResponse = LigoSetEntrypointResponse
  { seqLigoSetEntrypointResponse :: Int
  , request_seqLigoSetEntrypointResponse :: Int
  , successLigoSetEntrypointResponse :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving Buildable via (GenericBuildable LigoSetEntrypointResponse)

data LigoCompileContractResponse = LigoCompileContractResponse
  { seqLigoCompileContractResponse :: Int
  , request_seqLigoCompileContractResponse :: Int
  , successLigoCompileContractResponse :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving Buildable via (GenericBuildable LigoCompileContractResponse)

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

data LigoSpecificResponse
  = InitializeLoggerResponse LigoInitializeLoggerResponse
  | SetFileResponse LigoSetFileResponse
  | SetEntrypointResponse LigoSetEntrypointResponse
  | CompileContractResponse LigoCompileContractResponse
  | GetContractMetadataResponse LigoGetContractMetadataResponse
  | ValidateValueResponse LigoValidateValueResponse
  deriving stock (Eq, Show, Generic)
  deriving Buildable via (GenericBuildable LigoSpecificResponse)

deriveSum
  [ (''LigoSpecificRequest, "Request", "command", [])
  , (''LigoSpecificResponse, "Response", "command", [])
  ]
jsonfyMany
  [ ''LigoLaunchRequest
  , ''LigoLaunchRequestArguments
  ]
