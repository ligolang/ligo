module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromMaybe)
import Data.Text.IO qualified as Text
import Lib (mkApp, CompileRequest (..), Config (..), Source (..))
import Network.HTTP.Types.Method (methodPost)
import Network.Wai.Test (SResponse, simpleBody)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Hspec
import Test.Hspec.Wai (WaiSession, request, with)

main :: IO ()
main = do
  ligoPath <- fromMaybe (error "need to set LIGO_PATH")
          <$> lookupEnv "LIGO_PATH"
  let config = Config
        { cLigoPath = ligoPath
        , cPort = 8080
        , cVerbose = False
        }
  hspec (spec config)

post :: BS.ByteString -> LBS.ByteString -> WaiSession () SResponse
post path =
  request
    methodPost
    path
    [ ("Accept", "application/json"),
      ("Content-type", "application/json")
    ]

contractsDir :: FilePath
contractsDir = "test/contracts"

spec :: Config -> Spec
spec config = with (return (mkApp config)) $ do
  describe "POST /compile" $ do
    it "compiles basic single-file input correctly" $ do
      source <- liftIO $ Text.readFile $ contractsDir </> "basic/main.mligo"
      let input =
            CompileRequest
              { rSources = [("main.mligo", Source source)],
                rMain = "main.mligo"
              }
      response <- post "/compile" (Aeson.encode input)
      expected <- liftIO $ Text.readFile (contractsDir </> "basic/output.tz")
      liftIO $
        case Aeson.decode (simpleBody response) of
          Nothing -> expectationFailure "could not decode response"
          Just actual -> actual `shouldBe` expected

    it "compiles multi-file input correctly" $ do
      let loadSource :: FilePath -> IO Source
          loadSource = fmap Source . Text.readFile . (contractsDir </>)
      typesSource <- liftIO (loadSource "multifile/dir/types.mligo")
      mainSource <- liftIO (loadSource "multifile/main.mligo")

      let input =
            CompileRequest
              { rSources =
                  [ ("dir/types.mligo", typesSource),
                    ("main.mligo", mainSource)
                  ],
                rMain = "main.mligo"
              }
      response <- post "/compile" (Aeson.encode input)
      expected <- liftIO $ Text.readFile (contractsDir </> "multifile/output.tz")
      liftIO $
        case Aeson.decode (simpleBody response) of
          Nothing -> expectationFailure "could not decode response"
          Just actual -> actual `shouldBe` expected
