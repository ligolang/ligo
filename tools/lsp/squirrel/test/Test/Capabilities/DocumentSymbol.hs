module Test.Capabilities.DocumentSymbol
  ( unit_document_symbols_example_heap
  ) where

import Control.Lens ((^.))
import Data.Text (Text)
import Language.Haskell.LSP.Types (Location (Location, _range), SymbolInformation (SymbolInformation, _kind, _location, _name), SymbolKind (..), Uri (Uri))
import Language.Haskell.LSP.Types.Lens (character, end, line, start)
import System.FilePath ((</>))

import Test.HUnit (Assertion)

import AST.Capabilities.DocumentSymbol (extractDocumentSymbols)
import AST.Scope (Fallback)

import Test.Capabilities.Util (contractsDir)
import Test.FixedExpectations (shouldBe)
import Test.Util (readContractWithScopes)


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
    ]
