module Test.Common.Capabilities.DocumentSymbol
  ( documentSymbolsExampleLetCamligoDriver
  , documentSymbolsExampleLetJsligoDriver
  , documentSymbolsCameligoModules
  ) where

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
