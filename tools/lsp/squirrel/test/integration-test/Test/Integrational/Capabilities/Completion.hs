module Test.Integrational.Capabilities.Completion
  ( test_completion
  ) where

import AST.Scope (Standard)

import Test.Common.Capabilities.Completion
import Test.Tasty (TestTree, testGroup)

test_completion :: IO TestTree
test_completion = testGroup "Simple completion" <$> sequenceA
  [ completionDriver @Standard $ caseInfos @Standard
  --, completionDriver @FromCompiler $ caseInfos @FromCompiler -- FIXME (LIGO-596) (LIGO-679)
  ]
