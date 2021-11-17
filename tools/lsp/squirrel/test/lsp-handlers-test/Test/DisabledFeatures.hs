{-# LANGUAGE OverloadedLists #-}
module Test.DisabledFeatures
  ( unit_can't_hover_when_disabled
  ) where

import Data.Default (def)
import GHC.Exts (fromList)
import Language.LSP.Test (SessionException (..), getHover)
import Language.LSP.Types
  ( ErrorCode (..), LspId (..), Position (..), ResponseError (..)
  , SomeClientMethod (..), SomeLspId (..), SMethod (..)
  )

import Test.HUnit (Assertion)

import Test.Common.Capabilities.Hover (contractsDir)
import Test.Common.FixedExpectations (shouldThrow)
import Test.Common.LSP (openLigoDoc, runHandlersTestWithConfig)

import Config (Config (..))

-- There is a test that is just like this one but expecting a hover in the
-- capabilities test. This one just expects it will fail to hover instead.
unit_can't_hover_when_disabled :: Assertion
unit_can't_hover_when_disabled = do
  let config = def {_cDisabledFeatures = fromList [SomeClientMethod STextDocumentHover]}
  hover <- runHandlersTestWithConfig config contractsDir do
    doc <- openLigoDoc "eq_bool.ligo"
    getHover doc (Position 3 11)
  seq hover (pure ())
    `shouldThrow` \(UnexpectedResponseError
      (SomeLspId (IdInt 1))
      ResponseError
        { _code = RequestCancelled
        , _message = "Cannot handle STextDocumentHover: disabled by user."
        , _xdata = Nothing
        }) -> True
