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
    , ("const result", SkConstant, (11, 9), (11, 15))
    , ("const s", SkConstant, (12, 9), (12, 10))
    , ("const last", SkConstant, (13, 9), (13, 13))
    , ("pop_", SkFunction, (22, 9), (22, 13))
    , ("const result", SkConstant, (24, 10), (24, 16))
    , ("const s", SkConstant, (25, 10), (25, 11))
    , ("var current", SkVariable, (26, 8), (26, 15))
    , ("const i", SkConstant, (31, 10), (31, 11))
    , ("const left", SkConstant, (32, 10), (32, 14))
    , ("const right", SkConstant, (33, 10), (33, 15))
    , ("var largest", SkVariable, (36, 8), (36, 15))
    , ("const tmp", SkConstant, (37, 10), (37, 13))
    , ("insert", SkFunction, (46, 9), (46, 15))
    , ("var i", SkVariable, (48, 8), (48, 9))
    , ("var largest", SkVariable, (50, 8), (50, 15))
    , ("var parent", SkVariable, (51, 8), (51, 14))
    , ("const tmp", SkConstant, (58, 16), (58, 19))
    , ("pop", SkFunction, (66, 9), (66, 12))
    , ("const result", SkConstant, (68, 10), (68, 16))
    , ("var s", SkVariable, (69, 8), (69, 9))
    , ("const last", SkConstant, (70, 10), (70, 14))
    , ("var i", SkVariable, (74, 8), (74, 9))
    , ("var largest", SkVariable, (75, 8), (75, 15))
    , ("var left", SkVariable, (76, 8), (76, 12))
    , ("var right", SkVariable, (77, 8), (77, 13))
    , ("var c", SkVariable, (78, 8), (78, 9))
    , ("const tmp", SkConstant, (87, 16), (87, 19))
    , ("const tmp", SkConstant, (96, 18), (96, 21))
    ]
