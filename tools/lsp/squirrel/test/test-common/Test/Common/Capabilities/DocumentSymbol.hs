module Test.Common.Capabilities.DocumentSymbol
  ( documentSymbolsExampleHeapDriver
  , documentSymbolsExampleAccessDriver
  , documentSymbolsExampleLetCamligoDriver
  , documentSymbolsExampleLetReligoDriver
  , documentSymbolsExampleLetJsligoDriver
  , documentSymbolsCameligoModules
  ) where

import Control.Lens ((^.))
import Data.Text (Text)
import Language.LSP.Types
  (Location (Location, _range), SymbolInformation (SymbolInformation, _kind, _location, _name),
  SymbolKind (..), UInt, Uri (Uri))
import Language.LSP.Types.Lens (character, end, line, start)
import System.Directory (makeAbsolute)
import System.FilePath ((</>))

import Test.HUnit (Assertion)

import AST.Capabilities.DocumentSymbol (extractDocumentSymbols)

import Test.Common.Capabilities.Util qualified as Common (contractsDir)
import Test.Common.FixedExpectations (shouldMatchList)
import Test.Common.Util (ScopeTester, readContractWithScopes)

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "document-symbol"

type SimpleSymInfo = (Text, SymbolKind, (UInt, UInt), (UInt, UInt))

simplify :: SymbolInformation -> SimpleSymInfo
simplify SymbolInformation{_name, _kind, _location = Location{_range}} =
  ( _name
  , _kind
  , (_range ^. start . line, _range ^. start . character)
  , (_range ^. end . line, _range ^. end . character)
  )

documentSymbolsExampleHeapDriver :: forall impl. ScopeTester impl => Assertion
documentSymbolsExampleHeapDriver = do
  fp <- makeAbsolute $ contractsDir </> "heap.ligo"
  tree <- readContractWithScopes @impl fp
  let symbols = extractDocumentSymbols (Uri "<test>") tree
  map simplify symbols `shouldMatchList`
    [ ("heap", SkTypeParameter, (3, 5), (3, 9))
    , ("is_empty", SkFunction, (5,9), (5,17))
    , ("get_top", SkFunction, (7, 9), (7, 16))
    , ("pop_switch", SkFunction, (9, 9), (9, 19))
    , ("pop_", SkFunction, (22, 9), (22, 13))
    , ("insert", SkFunction, (46, 9), (46, 15))
    , ("pop", SkFunction, (66, 9), (66, 12))
    , ("const empty",SkConstant,(105,6),(105,11))
    ]

documentSymbolsExampleAccessDriver :: forall impl. ScopeTester impl => Assertion
documentSymbolsExampleAccessDriver = do
  fp <- makeAbsolute $ contractsDir </> "access.ligo"
  tree <- readContractWithScopes @impl fp
  let symbols = extractDocumentSymbols (Uri "<test>") tree
  map simplify symbols `shouldMatchList`
    [ ("const owner", SkConstant , (2, 6), (2, 11))
    , ("main", SkFunction, (4,9), (4,13))
    ]

documentSymbolsExampleLetCamligoDriver :: forall impl. ScopeTester impl => Assertion
documentSymbolsExampleLetCamligoDriver = do
  fp <- makeAbsolute $ contractsDir </> "let.mligo"
  tree <- readContractWithScopes @impl fp
  let symbols = extractDocumentSymbols (Uri "<test>") tree
  map simplify symbols `shouldMatchList`
    [ ("const a", SkConstant, (0, 4), (0, 5))

    , ("const b", SkConstant, (1, 5), (1, 6))
    , ("const c", SkConstant, (1, 8), (1, 9))

    , ("const d", SkConstant, (2, 5), (2, 6))
    , ("const e", SkConstant, (2, 9), (2, 10))

    , ("const f", SkConstant, (3, 5), (3, 6))
    , ("const h", SkConstant, (3, 13), (3, 14))

    , ("const j", SkConstant, (4, 10), (4, 11))
    , ("const k", SkConstant, (4, 13), (4, 14))
    , ("const m", SkConstant, (4, 23), (4, 24))
    , ("const n", SkConstant, (4, 26), (4, 27))
    ]

documentSymbolsExampleLetReligoDriver :: forall impl. ScopeTester impl => Assertion
documentSymbolsExampleLetReligoDriver = do
  fp <- makeAbsolute $ contractsDir </> "let.religo"
  tree <- readContractWithScopes @impl fp
  let symbols = extractDocumentSymbols (Uri "<test>") tree
  map simplify symbols `shouldMatchList`
    [ ("const a", SkConstant, (0, 4), (0, 5))

    , ("const b", SkConstant, (1, 5), (1, 6))
    , ("const c", SkConstant, (1, 8), (1, 9))

    , ("const d", SkConstant, (2, 6), (2, 7))
    , ("const e", SkConstant, (2, 16), (2, 17))

    , ("const f", SkConstant, (3, 6), (3, 7))
    , ("const h", SkConstant, (3, 14), (3, 15))
    ]

documentSymbolsExampleLetJsligoDriver :: forall impl. ScopeTester impl => Assertion
documentSymbolsExampleLetJsligoDriver = do
  tree <- readContractWithScopes @impl (contractsDir </> "let.jsligo")
  let symbols = extractDocumentSymbols (Uri "<test>") tree
  map simplify symbols `shouldMatchList`
    [ ("const a", SkConstant, (0, 4), (0, 5))

    , ("const b", SkConstant, (1, 5), (1, 6))
    , ("const c", SkConstant, (1, 13), (1, 14))

    , ("const d", SkConstant, (2, 5), (2, 6))
    , ("const e", SkConstant, (2, 8), (2, 9))
    ]

documentSymbolsCameligoModules :: forall impl. ScopeTester impl => Assertion
documentSymbolsCameligoModules = do
  tree <- readContractWithScopes @impl (contractsDir </> "nested-modules.mligo")
  let symbols = extractDocumentSymbols (Uri "<test>") tree
  map simplify symbols `shouldMatchList`
    [ ("module A", SkModule, (0, 7), (0, 8))
    , ("const a", SkConstant, (1, 6), (1, 7))

    , ("module B", SkModule, (2, 9), (2, 10))
    , ("const b", SkConstant, (6, 8), (6, 9))

    , ("module C", SkModule, (3, 11), (3, 12))
    , ("const c", SkConstant, (4, 10), (4, 11))
    ]
