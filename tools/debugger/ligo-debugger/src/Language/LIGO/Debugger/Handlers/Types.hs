-- | Types related to debugger.
module Language.LIGO.Debugger.Handlers.Types
  ( LigoLaunchRequest (..)
  , LigoLaunchRequestArguments (..)
  , LigoInitializeLoggerRequest (..)
  , LigoInitializeLoggerRequestArguments (..)
  , LigoSpecificRequest (..)

  , LigoInitializeLoggerResponse (..)
  , LigoSpecificResponse (..)
  ) where

import Fmt (Buildable (..), GenericBuildable (..))

import Morley.Debugger.DAP.TH (deriveSum, jsonfyMany)

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

    -- | Name of the entrypoint to debug.
  , entrypointLigoLaunchRequestArguments :: Maybe String

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

data LigoSpecificRequest
  = InitializeLoggerRequest LigoInitializeLoggerRequest
  deriving stock (Eq, Show, Generic)
  deriving Buildable via (GenericBuildable LigoSpecificRequest)

data LigoInitializeLoggerResponse = LigoInitializeLoggerResponse
  { seqLigoInitializeLoggerResponse :: Int
  , request_seqLigoInitializeLoggerResponse :: Int
  , successLigoInitializeLoggerResponse :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving Buildable via (GenericBuildable LigoInitializeLoggerResponse)

data LigoSpecificResponse
  = InitializeLoggerResponse LigoInitializeLoggerResponse
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
