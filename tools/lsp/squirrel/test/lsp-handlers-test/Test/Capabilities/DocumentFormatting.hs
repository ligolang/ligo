module Test.Capabilities.DocumentFormatting
  ( unit_document_formatting
  , unit_format_dirty
  ) where

import Language.LSP.Test
import Language.LSP.Types
import System.FilePath ((</>))

import Test.HUnit (Assertion)

import Test.Common.Capabilities.Util qualified as Common (contractsDir)
import Test.Common.FixedExpectations (shouldBe)
import Test.Common.LSP (openLigoDoc, runHandlersTest)

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "document-format"

defaultFormattingOptions :: FormattingOptions
defaultFormattingOptions = FormattingOptions
  { _tabSize = 2
  , _insertSpaces = True
  , _trimTrailingWhitespace = Just True
  , _insertFinalNewline = Nothing
  , _trimFinalNewlines = Nothing
  }

unit_document_formatting :: Assertion
unit_document_formatting = do
  let filename = "trailing_space.ligo"
  let expectedFilename = "trailing_space_expected.ligo"

  (formattedDoc, expectedDoc) <- runHandlersTest contractsDir $ do
    doc <- openLigoDoc filename
    expectedDoc <- openLigoDoc expectedFilename
    formatDoc doc defaultFormattingOptions
    (,) <$> documentContents doc <*> documentContents expectedDoc
  formattedDoc `shouldBe` expectedDoc

unit_format_dirty :: Assertion
unit_format_dirty = do
  let filename = "dirty.mligo"
  let expectedFilename = "dirty_expected.mligo"

  (formattedContents, expectedContents) <- runHandlersTest contractsDir do
    doc <- openLigoDoc filename
    changeDoc doc
      [ TextDocumentContentChangeEvent
        (Just $ Range (Position 0 30) (Position 0 30))
        Nothing
        "2 + "
      ]
    formatDoc doc defaultFormattingOptions

    expectedDoc <- openLigoDoc expectedFilename
    (,) <$> documentContents doc <*> documentContents expectedDoc
  formattedContents `shouldBe` expectedContents
