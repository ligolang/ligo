module Test.Capabilities.Hover
  ( unit_hover
  , unit_hover_jsligo
  , unit_hover_type_inference_fallback
  , unit_hover_type_inference_standard
  ) where

import Data.Default (def)
import Data.Text qualified as Text
import Language.LSP.Test
import Language.LSP.Types (Hover (..), HoverContents (..), MarkupContent (..), Position (..))

import AST.Scope (ScopingSystem (..))
import Config (Config (_cScopingSystem))

import Test.HUnit (Assertion)

import Test.Common.Capabilities.Hover (contractsDir)
import Test.Common.FixedExpectations (shouldSatisfy)
import Test.Common.LSP (openLigoDoc, runHandlersTest, runHandlersTestWithConfig)

matchesHover :: Text -> Text -> Bool
matchesHover expected actual =
  let withoutDialect = Text.tail $ Text.dropWhile (/= '\n') actual in
  expected `Text.isPrefixOf` withoutDialect

unit_hover :: Assertion
unit_hover = do
  maybeHover <- runHandlersTest contractsDir $ do
    doc <- openLigoDoc "eq_bool.ligo"
    getHover doc (Position 3 11)
  shouldSatisfy maybeHover $ \case
    Just (Hover (HoverContents (MarkupContent _ hoverText)) _) ->
      "b : bool" `matchesHover` hoverText
    _ -> error "expected a hover"

unit_hover_jsligo :: Assertion
unit_hover_jsligo = do
  maybeHover <- runHandlersTest contractsDir $ do
    doc <- openLigoDoc "eq_bool.jsligo"
    getHover doc (Position 3 11)
  shouldSatisfy maybeHover $ \case
    Just (Hover (HoverContents (MarkupContent _ hoverText)) _) ->
      "b : bool" `matchesHover` hoverText
    _ -> error "expected a hover"

-- this also tests that scoping system from config
unit_hover_type_inference_fallback :: Assertion
unit_hover_type_inference_fallback = do
  maybeHover <- runHandlersTestWithConfig def{_cScopingSystem = FallbackScopes} contractsDir $ do
    doc <- openLigoDoc "simple.mligo"
    getHover doc (Position 0 4)
  shouldSatisfy maybeHover $ \case
    Just (Hover (HoverContents (MarkupContent _ hoverText)) _) ->
      "x : (* unknown *)" `matchesHover` hoverText
    _ -> error "expected a hover"

unit_hover_type_inference_standard :: Assertion
unit_hover_type_inference_standard = do
  maybeHover <- runHandlersTestWithConfig def{_cScopingSystem = StandardScopes} contractsDir $ do
    doc <- openLigoDoc "simple.mligo"
    getHover doc (Position 0 4)
  shouldSatisfy maybeHover $ \case
    Just (Hover (HoverContents (MarkupContent _ hoverText)) _) ->
      "x : int" `matchesHover` hoverText
    _ -> error "expected a hover"
