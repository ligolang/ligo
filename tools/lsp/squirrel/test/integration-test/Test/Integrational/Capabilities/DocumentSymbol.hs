module Test.Integrational.Capabilities.DocumentSymbol
  ( unit_document_symbols_example_heap
  , unit_document_symbols_example_access
  ) where

import AST.Scope (Standard)

import Test.Common.Capabilities.DocumentSymbol
import Test.HUnit (Assertion)

unit_document_symbols_example_heap :: Assertion
unit_document_symbols_example_heap = documentSymbolsExampleHeapDriver @Standard

unit_document_symbols_example_access :: Assertion
unit_document_symbols_example_access = documentSymbolsExampleAccessDriver @Standard
