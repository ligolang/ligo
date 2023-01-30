module Test.Integrational.Capabilities.SignatureHelp
  ( test_simpleFunctionCall
  ) where

import AST.Scope (Standard)

import Test.Common.Capabilities.SignatureHelp
import Test.Tasty (TestTree, testGroup)

-- We wish to ignore contracts with parse errors for FromCompiler scopes.
{-
validInfos :: [TestInfo]
validInfos = filter (not . (`elem` ignore) . tiContract) caseInfos
  where
    ignore = []
-}

test_simpleFunctionCall :: IO TestTree
test_simpleFunctionCall = testGroup "Simple function call" <$> sequenceA
  [ simpleFunctionCallDriver @Standard caseInfos
  --, simpleFunctionCallDriver @FromCompiler caseInfos  -- FIXME (LIGO-679)
  ]
