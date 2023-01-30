{-# LANGUAGE PolyKinds #-}
module Test.Common.LSP
  ( getResponseResult
  , openLigoDoc
  , runHandlersTest
  , runHandlersTestWithConfig
  ) where

import Data.Aeson (toJSON)
import Language.LSP.Test
  (Session, SessionConfig (..), defaultConfig, fullCaps, openDoc, runSession, runSessionWithConfig)
import Language.LSP.Types (ResponseMessage, ResponseResult, TextDocumentIdentifier)
import Language.LSP.Types.Lens qualified as LSP (result)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

import Config (Config)

-- one can set LIGO_LSP_TEST_EXE env var before testing/benchmarking to test a different lsp server
-- with our requests
serverName :: String
serverName = unsafePerformIO $ fromMaybe "ligo-squirrel" <$>
  lookupEnv "LIGO_LSP_TEST_EXE"
{-# NOINLINE serverName #-}

getResponseResult :: ResponseMessage m -> ResponseResult m
getResponseResult rsp =
  case rsp ^. LSP.result of
    Right x -> x
    Left _ -> error "Should be able to parse ResponseMessage"

openLigoDoc :: FilePath -> Session TextDocumentIdentifier
openLigoDoc fp = openDoc fp "ligo"

runHandlersTest :: FilePath -> Session a -> IO a
runHandlersTest = runSession serverName fullCaps

runHandlersTestWithConfig :: Config -> FilePath -> Session a -> IO a
runHandlersTestWithConfig config =
  runSessionWithConfig (defaultConfig {lspConfig = Just $ toJSON config}) serverName fullCaps
