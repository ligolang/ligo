module Test.Integrational.Capabilities.DocumentLink
  ( unit_document_link_b
  , unit_document_link_c
  ) where

import AST.Scope (FromCompiler, Standard)

import Test.Common.Capabilities.DocumentLink
import Test.HUnit (Assertion)

unit_document_link_b :: Assertion
unit_document_link_b = do
  documentLinkBDriver @Standard
  documentLinkBDriver @FromCompiler

unit_document_link_c :: Assertion
unit_document_link_c = do
  documentLinkCDriver @Standard
  documentLinkCDriver @FromCompiler
