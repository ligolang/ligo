module Test.Capabilities.DocumentSymbol
  ( unit_document_symbols_example_heap
  , unit_document_symbols_example_access
  ) where

import Control.Lens ((^.))
import Data.Text (Text)
import Language.LSP.Types
  (Location (Location, _range), SymbolInformation (SymbolInformation, _kind, _location, _name),
  SymbolKind (..), Uri (Uri))
import Language.LSP.Types.Lens (character, end, line, start)
import System.FilePath ((</>))

import Test.HUnit (Assertion)

import AST.Capabilities.DocumentSymbol (extractDocumentSymbols)
import AST.Scope (Fallback)

import qualified Test.Capabilities.Util as Common (contractsDir)
import Test.FixedExpectations (shouldBe)
import Test.Util (readContractWithScopes)
import Test.Util.LigoEnv ()
-- Test.Util.LigoEnv for "instance HasLigoClient IO"

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "document-symbol"

type SimpleSymInfo = (Text, SymbolKind, (Int, Int), (Int, Int))

simplify :: SymbolInformation -> SimpleSymInfo
simplify SymbolInformation{_name, _kind, _location = Location{_range}} =
  ( _name
  , _kind
  , (_range ^. start . line, _range ^. start . character)
  , (_range ^. end . line, _range ^. end . character)
  )

unit_document_symbols_example_heap :: Assertion
unit_document_symbols_example_heap = do
  tree <- readContractWithScopes @Fallback (contractsDir </> "heap.ligo")
  symbols <- extractDocumentSymbols (Uri "<test>") tree
  map simplify symbols `shouldBe`
    [ ("heap", SkTypeParameter, (3, 5), (3, 9))
    , ("is_empty", SkFunction, (5,9), (5,17))
    , ("get_top", SkFunction, (7, 9), (7, 16))
    , ("pop_switch", SkFunction, (9, 9), (9, 19))
    , ("pop_", SkFunction, (22, 9), (22, 13))
    , ("insert", SkFunction, (46, 9), (46, 15))
    , ("pop", SkFunction, (66, 9), (66, 12))
    , ("const empty",SkConstant,(105,6),(105,11))
    ]

unit_document_symbols_example_access :: Assertion
unit_document_symbols_example_access = do
  tree <- readContractWithScopes @Fallback (contractsDir </> "access.ligo")
  symbols <- extractDocumentSymbols (Uri "<test>") tree
  map simplify symbols `shouldBe`
    [ ("const owner", SkConstant , (2, 6), (2, 11))
    , ("main", SkFunction, (4,9), (4,13))
    ]
