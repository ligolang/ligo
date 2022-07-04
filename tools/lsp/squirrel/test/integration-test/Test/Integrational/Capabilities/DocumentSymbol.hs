module Test.Integrational.Capabilities.DocumentSymbol
  ( unit_document_symbols_example_heap
  , unit_document_symbols_example_access
  , unit_document_symbols_example_let_camligo
  , unit_document_symbols_example_let_religo
  , unit_document_symbols_example_let_jsligo
  ) where

import AST.Scope (Standard)

import Test.Common.Capabilities.DocumentSymbol
import Test.HUnit (Assertion)

unit_document_symbols_example_heap :: Assertion
unit_document_symbols_example_heap = do
  documentSymbolsExampleHeapDriver @Standard
  --documentSymbolsExampleHeapDriver @FromCompiler  -- FIXME (LIGO-596)

unit_document_symbols_example_access :: Assertion
unit_document_symbols_example_access = do
  documentSymbolsExampleAccessDriver @Standard
  --documentSymbolsExampleAccessDriver @FromCompiler  -- FIXME (LIGO-93) (LIGO-505)

unit_document_symbols_example_let_camligo :: Assertion
unit_document_symbols_example_let_camligo = do
  documentSymbolsExampleLetCamligoDriver @Standard
  --documentSymbolsExampleLetCamligoDriver @FromCompiler  -- FIXME (LIGO-93) (LIGO-505)

unit_document_symbols_example_let_religo :: Assertion
unit_document_symbols_example_let_religo = do
  documentSymbolsExampleLetReligoDriver @Standard
  --documentSymbolsExampleLetReligoDriver @FromCompiler  -- FIXME (LIGO-596)

unit_document_symbols_example_let_jsligo :: Assertion
unit_document_symbols_example_let_jsligo = do
  documentSymbolsExampleLetJsligoDriver @Standard
