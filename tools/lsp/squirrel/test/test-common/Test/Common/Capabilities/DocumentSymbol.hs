module Test.Common.Capabilities.DocumentSymbol
  ( documentSymbolsExampleHeapDriver
  , documentSymbolsExampleAccessDriver
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
import AST.Scope (HasScopeForest)

import qualified Test.Common.Capabilities.Util as Common (contractsDir)
import Test.Common.FixedExpectations (shouldBe)
import Test.Common.Util (readContractWithScopes)

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

documentSymbolsExampleHeapDriver :: forall impl. HasScopeForest impl IO => Assertion
documentSymbolsExampleHeapDriver = do
  tree <- readContractWithScopes @impl (contractsDir </> "heap.ligo")
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

documentSymbolsExampleAccessDriver :: forall impl. HasScopeForest impl IO => Assertion
documentSymbolsExampleAccessDriver = do
  tree <- readContractWithScopes @impl (contractsDir </> "access.ligo")
  symbols <- extractDocumentSymbols (Uri "<test>") tree
  map simplify symbols `shouldBe`
    [ ("const owner", SkConstant , (2, 6), (2, 11))
    , ("main", SkFunction, (4,9), (4,13))
    ]
