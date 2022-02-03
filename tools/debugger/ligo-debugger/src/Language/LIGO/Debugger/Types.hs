module Language.LIGO.Debugger.Types
  ( LigoLaunchRequest (..)
  , LigoLaunchRequestArguments (..)
  , LigoInitializeLoggerRequest (..)
  , LigoInitializeLoggerRequestArguments (..)
  , LigoSpecificRequest (..)

  , LigoInitializeLoggerResponse (..)
  , LigoSpecificResponse (..)
  ) where

import Morley.Debugger.DAP.TH (deriveSum, jsonfyMany)

data LigoLaunchRequest = LigoLaunchRequest
  { seqLigoLaunchRequest :: Int
  , typeLigoLaunchRequest :: String
  , commandLigoLaunchRequest :: String
  , argumentsLigoLaunchRequest :: LigoLaunchRequestArguments
  } deriving stock (Eq, Show, Generic)

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

data LigoInitializeLoggerRequest = LigoInitializeLoggerRequest
  { seqLigoInitializeLoggerRequest :: Int
  , typeLigoInitializeLoggerRequest :: String
  , commandLigoInitializeLoggerRequest :: String
  , argumentsLigoInitializeLoggerRequest :: LigoInitializeLoggerRequestArguments
  } deriving stock (Eq, Show, Generic)

-- | Log directory for the debugger.
data LigoInitializeLoggerRequestArguments = LigoInitializeLoggerRequestArguments
  { fileLigoInitializeLoggerRequestArguments :: FilePath
  , logDirLigoInitializeLoggerRequestArguments :: Maybe FilePath
  } deriving stock (Eq, Show, Generic)

data LigoSpecificRequest
  = InitializeLoggerRequest LigoInitializeLoggerRequest
  deriving stock (Eq, Show, Generic)

data LigoInitializeLoggerResponse = LigoInitializeLoggerResponse
  { seqLigoInitializeLoggerResponse :: Int
  , request_seqLigoInitializeLoggerResponse :: Int
  , successLigoInitializeLoggerResponse :: Bool
  } deriving stock (Show, Eq)

data LigoSpecificResponse
  = InitializeLoggerResponse LigoInitializeLoggerResponse
  deriving stock (Eq, Show, Generic)

deriveSum
  [ (''LigoSpecificRequest, "Request", "command", [])
  , (''LigoSpecificResponse, "Response", "command", [])
  ]
jsonfyMany
  [ ''LigoLaunchRequest
  , ''LigoLaunchRequestArguments
  ]

