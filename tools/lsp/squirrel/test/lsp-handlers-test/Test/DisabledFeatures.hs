{-# LANGUAGE OverloadedLists #-}
module Test.DisabledFeatures
  ( unit_can't_hover_when_disabled
  , unit_can't_hover_after_disabling
  ) where

import Data.Aeson (toJSON)
import Data.Default (def)
import Language.LSP.Test (SessionException (..), getHover, sendNotification)
import Language.LSP.Types
  (DidChangeConfigurationParams (DidChangeConfigurationParams), ErrorCode (..), LspId (..),
  Position (..), ResponseError (..), SMethod (..), SomeClientMethod (..), SomeLspId (..))

import Test.HUnit (Assertion)

import Test.Common.Capabilities.Hover (contractsDir)
import Test.Common.FixedExpectations (shouldThrow)
import Test.Common.LSP (openLigoDoc, runHandlersTest, runHandlersTestWithConfig)

import Config (Config (..))

-- There is a test that is just like this one but expecting a hover in the
-- capabilities test. This one just expects it will fail to hover instead.
unit_can't_hover_when_disabled :: Assertion
unit_can't_hover_when_disabled = do
  let config = def {_cDisabledFeatures = fromList [SomeClientMethod STextDocumentHover]}
  hover <- runHandlersTestWithConfig config contractsDir do
    doc <- openLigoDoc "eq_bool.ligo"
    getHover doc (Position 3 11)
  seq hover pass `shouldThrow` expectedError

-- Ensure that we can disable hover by updating the configuration, even after startup.
unit_can't_hover_after_disabling :: Assertion
unit_can't_hover_after_disabling = do
  let config = def {_cDisabledFeatures = fromList [SomeClientMethod STextDocumentHover]}
  hover <- runHandlersTest contractsDir $ do
    doc <- openLigoDoc "eq_bool.ligo"
    sendNotification SWorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))
    getHover doc (Position 3 11)
  seq hover pass `shouldThrow` expectedError

expectedError :: SessionException -> Bool
expectedError = (==)
  $ UnexpectedResponseError
      (SomeLspId (IdInt 1))
      ResponseError
        { _code = RequestCancelled
        , _message = "Cannot handle STextDocumentHover: disabled by user."
        , _xdata = Nothing
        }
