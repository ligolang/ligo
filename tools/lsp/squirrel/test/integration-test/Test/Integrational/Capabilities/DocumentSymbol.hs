module Test.Integrational.Capabilities.DocumentSymbol
  ( unit_document_symbols_example_let_camligo
  , unit_document_symbols_example_let_jsligo
  , unit_document_symbols_cameligo_modules
  ) where

import AST.Scope (Standard)

import Test.Common.Capabilities.DocumentSymbol
import Test.HUnit (Assertion)

unit_document_symbols_example_let_camligo :: Assertion
unit_document_symbols_example_let_camligo = do
  documentSymbolsExampleLetCamligoDriver @Standard
  --documentSymbolsExampleLetCamligoDriver @FromCompiler  -- FIXME (LIGO-93) (LIGO-679)

unit_document_symbols_example_let_jsligo :: Assertion
unit_document_symbols_example_let_jsligo = do
  documentSymbolsExampleLetJsligoDriver @Standard

unit_document_symbols_cameligo_modules :: Assertion
unit_document_symbols_cameligo_modules = do
  documentSymbolsCameligoModules @Standard
  --documentSymbolsCameligoModules @FromCompiler  -- FIXME
