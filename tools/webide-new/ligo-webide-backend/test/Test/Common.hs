module Test.Common (contractsDir, post, TestM, mkTest) where

import Data.Aeson (FromJSON, ToJSON)
import Network.HTTP.Req
  (POST(..), ReqBodyJson(..), defaultHttpConfig, http, jsonResponse, req, responseBody, runReq,
  (/:))
import Network.HTTP.Req qualified as Req (port)
import Network.Wai.Handler.Warp (Port)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.HUnit (testCase)

contractsDir :: FilePath
contractsDir = "test/contracts"

post
  :: (ToJSON a, FromJSON b)
  => Text
  -> a
  -> ReaderT Port IO b
post endpoint body = do
  port <- ask
  fmap responseBody . runReq defaultHttpConfig $
    req
      POST
      (http "localhost" /: endpoint)
      (ReqBodyJson body)
      jsonResponse
      (Req.port port)

type TestM = Reader Port

mkTest :: TestName -> ReaderT Port IO () -> Reader Port TestTree
mkTest name action = reader (testCase name . runReaderT action)
