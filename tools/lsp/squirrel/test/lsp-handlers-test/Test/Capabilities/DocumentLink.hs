module Test.Capabilities.DocumentLink
  ( unit_document_link_b
  , unit_document_link_c
  ) where

import Language.LSP.Test
import Language.LSP.Types

import Test.HUnit (Assertion)

import Test.Common.Capabilities.DocumentLink
  (getContractsDir, simplifiedBLinks, simplifiedCLinks, simplifyDocumentLink)
import Test.Common.FixedExpectations (shouldBe)
import Test.Common.LSP (getResponseResult, openLigoDoc, runHandlersTest)

-- lsp-test doesn't provide a function for testing signature help
getDocumentLinks :: TextDocumentIdentifier -> Session (List DocumentLink)
getDocumentLinks doc =
  let params = DocumentLinkParams Nothing Nothing doc
  in getResponseResult <$> request STextDocumentDocumentLink params

unit_document_link_b :: Assertion
unit_document_link_b = do
  let filename = "B1.jsligo"
  contractsDir <- getContractsDir
  documentLinks <- runHandlersTest contractsDir $ do
    doc <- openLigoDoc filename
    getDocumentLinks doc
  target <- List <$> simplifiedBLinks
  fmap simplifyDocumentLink documentLinks `shouldBe` target

unit_document_link_c :: Assertion
unit_document_link_c = do
  let filename = "C1.mligo"
  contractsDir <- getContractsDir
  documentLinks <- runHandlersTest contractsDir $ do
    doc <- openLigoDoc filename
    getDocumentLinks doc
  target <- List <$> simplifiedCLinks
  fmap simplifyDocumentLink documentLinks `shouldBe` target
