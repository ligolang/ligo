{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
  ( startApp
  , mkApp
  , CompileRequest (..)
  , Config (..)
  )
where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, fieldLabelModifier, parseJSON, genericParseJSON, toJSON, genericToJSON, Result (..), fromJSON, Value)
import Control.Monad.Except (ExceptT)
import GHC.Generics
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Data.Char (toLower)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Data.Text (Text)
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Katip (Environment (..), KatipT, initLogEnv, runKatipT)
import Network.Wai.Handler.Warp (run)
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors (corsRequestHeaders, simpleCorsResourcePolicy, cors)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import System.IO (hClose)
import System.IO.Temp
import System.Process
import System.Exit (ExitCode (ExitSuccess, ExitFailure))

type API = "compile" :> ReqBody '[JSON] Value :> Post '[JSON] Text

data Config = Config
  { cLigoPath :: FilePath
  , cPort :: Int
  , cVerbose :: Bool
  }

data CompileRequest = CompileRequest
  { rFileExtension :: Text
  , rSource :: Text
  } deriving stock (Eq, Show, Ord, Generic)

prepareField :: Int -> String -> String
prepareField n = lowercaseInitial . drop n

lowercaseInitial :: String -> String
lowercaseInitial [] = []
lowercaseInitial (c:s) = toLower c : s

instance FromJSON CompileRequest where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = prepareField 1}

instance ToJSON CompileRequest where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 1}

type WebIDEM = KatipT (ReaderT Config (ExceptT ServerError IO))

startApp :: Config -> IO ()
startApp config = run (cPort config) (mkApp config)

-- | Allow Content-Type header with values other then allowed by simpleCors.
corsWithContentType :: Middleware
corsWithContentType = cors (const $ Just policy)
    where
      policy = simpleCorsResourcePolicy
        { corsRequestHeaders = ["Content-Type"] }

mkApp :: Config -> Application
mkApp config = maybeLogRequests . corsWithContentType $ serve api server
  where
    api :: Proxy API
    api = Proxy

    server :: ServerT API Handler
    server = hoistServer api hoist compile

    hoist :: WebIDEM a -> Handler a
    hoist x = Handler $ do
      logEnv <- liftIO $ initLogEnv "ligo-webide" (Environment "devel")
      runReaderT (runKatipT logEnv x) config

    maybeLogRequests :: Middleware
    maybeLogRequests =
      if cVerbose config
      then logStdoutDev
      else id

compile :: Value -> WebIDEM Text
compile input =
  case fromJSON input of
    Error err -> lift $ throwError $ err400 {errBody = BSL.pack $ "malformed request body: " ++ err}
    Success request ->
      let filename = "input." ++ Text.unpack (rFileExtension request)
       in withSystemTempFile filename $ \fp handle -> do
            liftIO $ hClose handle
            liftIO $ Text.writeFile fp (rSource request)
            ligoPath <- lift (asks cLigoPath)
            (ec, out, err) <- liftIO $
              readProcessWithExitCode ligoPath ["compile", "contract", fp] ""
            case ec of
              ExitSuccess -> pure (Text.pack out)
              ExitFailure _ -> pure (Text.pack err)
