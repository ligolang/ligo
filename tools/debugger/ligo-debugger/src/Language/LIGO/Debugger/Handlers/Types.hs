{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Types related to DAP.
module Language.LIGO.Debugger.Handlers.Types
  ( LigoLaunchRequest (..)
  , LigoContractEnv (..)
  , VotingPowersConfig (..)
  , SimpleVotingPowersInfo (..)
  , LigoInitializeLoggerRequest (..)
  , LigoSetLigoConfigRequest (..)
  , LigoSetProgramPathRequest (..)
  , LigoValidateEntrypointRequest (..)
  , LigoGetContractMetadataRequest (..)
  , LigoValidateValueRequest (..)
  , LigoValidateConfigRequest (..)

  , LigoValidateResponse (..)
  , LigoSetProgramPathResponse (..)
  , ligoValidateFromEither
  , ContractMetadata (..)
  ) where

import Data.Aeson (ToJSON (..))
import Data.Aeson.TH (deriveFromJSON, deriveToJSON)
import qualified Data.Aeson.Types as Aeson
import Data.Default (Default (def))
import Fmt.Buildable (Buildable, GenericBuildable (..))
import Protocol.DAP (IsRequest (..))

import qualified Morley.Debugger.DAP.LanguageServer as MD
import Morley.Debugger.DAP.Types (MichelsonJson (..))
import Morley.Debugger.DAP.Types.Morley (SimpleVotingPowersInfo (..), VotingPowersConfig (..))
import qualified Morley.Michelson.Untyped as U
import Morley.Tezos.Address (ContractAddress, L1Address)
import Morley.Tezos.Core (ChainId, Mutez, Timestamp)

data LigoLaunchRequest = LigoLaunchRequest
  { noDebug             :: Maybe Bool

    -- | Path to LIGO source file.
  , program             :: Maybe FilePath

    -- | Name of the michelson entrypoint to debug.
  , michelsonEntrypoint :: Maybe Text

    -- | Storage value for contract.
  , storage             :: Maybe Text

    -- | Entry point of the contract (@main@ method).
  , entrypoint          :: Maybe Text

    -- | Parameter value for contract.
  , parameter           :: Maybe Text

  , contractEnv         :: Maybe LigoContractEnv
  } deriving stock (Eq, Show, Generic)

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

data LigoValidateEntrypointRequest = LigoValidateEntrypointRequest
  { entrypoint :: Text
  } deriving stock (Eq, Show, Generic)

data LigoGetContractMetadataRequest = LigoGetContractMetadataRequest
  { entrypoint :: Text
    -- ^ LIGO entrypoint to be used.
  } deriving stock (Eq, Show, Generic)

data LigoValidateValueRequest = LigoValidateValueRequest
  { value                     :: Text
    -- ^ Value to check.
  , category                  :: Text
    -- ^ Category of the value (e.g. @parameter@).
  , valueLang                 :: Text
    -- ^ Language of value (@LIGO@ or @Michelson@)
  , pickedMichelsonEntrypoint :: Maybe Text
    -- ^ Special michelson entrypoint that will be used.
  } deriving stock (Eq, Show, Generic)

data LigoValidateConfigRequest = LigoValidateConfigRequest
  { michelsonEntrypoint :: Maybe Text
  , parameter           :: Text
  , parameterLang       :: Text
  , storage             :: Text
  , storageLang         :: Text
  } deriving stock (Eq, Show, Generic)

data LigoSetProgramPathResponse = LigoSetProgramPathResponse
  { entrypoints :: [Text]
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
  , michelsonEntrypoints   :: MD.Entrypoints
  } deriving stock (Eq, Generic, Show)
    deriving Buildable via (GenericBuildable ContractMetadata)

instance ToJSON LigoValidateResponse where
  toJSON = \case
    LigoValidateOk         -> Aeson.Null
    LigoValidateFailed err -> Aeson.object [ "errorMessage" Aeson..= err ]

deriveToJSON Aeson.defaultOptions ''ContractMetadata
deriveToJSON Aeson.defaultOptions ''LigoSetProgramPathResponse

concatMapM (deriveFromJSON Aeson.defaultOptions)
  [ ''LigoContractEnv
  , ''LigoLaunchRequest
  , ''LigoInitializeLoggerRequest
  , ''LigoSetLigoConfigRequest
  , ''LigoSetProgramPathRequest
  , ''LigoValidateEntrypointRequest
  , ''LigoGetContractMetadataRequest
  , ''LigoValidateValueRequest
  , ''LigoValidateConfigRequest
  ]

instance IsRequest LigoLaunchRequest where
  type CommandFor LigoLaunchRequest = "launch"
  type ResponseFor LigoLaunchRequest = ()

instance IsRequest LigoInitializeLoggerRequest where
  type CommandFor LigoInitializeLoggerRequest = "initializeLogger"
  type ResponseFor LigoInitializeLoggerRequest = ()

instance IsRequest LigoSetLigoConfigRequest where
  type CommandFor LigoSetLigoConfigRequest = "setLigoConfig"
  type ResponseFor LigoSetLigoConfigRequest = ()

instance IsRequest LigoSetProgramPathRequest where
  type CommandFor LigoSetProgramPathRequest = "setProgramPath"
  type ResponseFor LigoSetProgramPathRequest = LigoSetProgramPathResponse

instance IsRequest LigoValidateEntrypointRequest where
  type CommandFor LigoValidateEntrypointRequest = "validateEntrypoint"
  type ResponseFor LigoValidateEntrypointRequest = LigoValidateResponse

instance IsRequest LigoGetContractMetadataRequest where
  type CommandFor LigoGetContractMetadataRequest = "getContractMetadata"
  type ResponseFor LigoGetContractMetadataRequest = ContractMetadata

instance IsRequest LigoValidateValueRequest where
  type CommandFor LigoValidateValueRequest = "validateValue"
  type ResponseFor LigoValidateValueRequest = LigoValidateResponse

instance IsRequest LigoValidateConfigRequest where
  type CommandFor LigoValidateConfigRequest = "validateConfig"
  type ResponseFor LigoValidateConfigRequest = ()
