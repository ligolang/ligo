module Test.Capabilities.DocumentLink
  ( unit_document_link_b
  , unit_document_link_c
  ) where

import AST.Scope (Fallback)

import Test.Common.Capabilities.DocumentLink
import Test.HUnit (Assertion)

unit_document_link_b :: Assertion
unit_document_link_b = documentLinkBDriver @Fallback

unit_document_link_c :: Assertion
unit_document_link_c = documentLinkCDriver @Fallback
