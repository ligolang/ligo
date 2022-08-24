module Lib
  ( startApp
  , mkApp
  , CompileRequest (..)
  , Config (..)
  )
where

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Trans (lift)
import Data.Aeson
  (FromJSON, Result(..), ToJSON, Value, defaultOptions, fieldLabelModifier, fromJSON,
  genericParseJSON, genericToJSON, parseJSON, toJSON)
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Char (toLower)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import GHC.Generics
import Katip (Environment(..), KatipT, initLogEnv, runKatipT)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (cors, corsRequestHeaders, simpleCorsResourcePolicy)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.IO (hClose)
import System.IO.Temp
import System.Process

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
  where
    lowercaseInitial :: String -> String
    lowercaseInitial [] = []
    lowercaseInitial (c:s) = toLower c : s

instance FromJSON CompileRequest where
  parseJSON = genericParseJSON
    defaultOptions {fieldLabelModifier = prepareField 1}

instance ToJSON CompileRequest where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 1}

type WebIDEM = KatipT (ReaderT Config (ExceptT ServerError IO))

startApp :: Config -> IO ()
startApp config = run (cPort config) (mkApp config)

mkApp :: Config -> Application
mkApp config =
  maybeLogRequests . corsWithContentType $ serve (Proxy @API) server
  where
    maybeLogRequests :: Middleware
    maybeLogRequests =
      if cVerbose config
      then logStdoutDev
      else id

    -- Allow Content-Type header with values other then allowed by simpleCors.
    corsWithContentType :: Middleware
    corsWithContentType = cors (const $ Just policy)
      where
        policy = simpleCorsResourcePolicy
          {corsRequestHeaders = ["Content-Type"]}

    server :: ServerT API Handler
    server = hoistServer (Proxy @API) hoist compile

    hoist :: WebIDEM a -> Handler a
    hoist x = Handler $ do
      logEnv <- liftIO $ initLogEnv "ligo-webide" (Environment "devel")
      runReaderT (runKatipT logEnv x) config

compile :: Value -> WebIDEM Text
compile input =
  case fromJSON input of
    Error err -> lift $ throwError $
      err400 {errBody = BSL.pack $ "malformed request body: " ++ err}
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
