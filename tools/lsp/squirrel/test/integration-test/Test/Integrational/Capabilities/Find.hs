module Test.Integrational.Capabilities.Find
  ( test_findDefinitionAndGoToReferencesCorrespondence
  , unit_definitionOfId
  , unit_definitionOfLeft
  , unit_referenceOfId
  , unit_referenceOfLeft
  , unit_definitionOfXInWildcard
  , unit_referenceOfXInWildcard

  , unit_type_of_heap_arg
  , unit_type_of_heap_const
  , unit_type_of_let
  , unit_type_of_pascaligo_lambda_arg
  , unit_pascaligo_local_type
  ) where

import AST.Scope (Standard)

import Test.Common.Capabilities.Find
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion)

test_findDefinitionAndGoToReferencesCorrespondence :: TestTree
test_findDefinitionAndGoToReferencesCorrespondence = findDefinitionAndGoToReferencesCorrespondence @Standard

unit_definitionOfId :: Assertion
unit_definitionOfId = definitionOfId @Standard

unit_referenceOfId :: Assertion
unit_referenceOfId = referenceOfId @Standard

unit_definitionOfLeft :: Assertion
unit_definitionOfLeft = definitionOfLeft @Standard

unit_referenceOfLeft :: Assertion
unit_referenceOfLeft = referenceOfLeft @Standard

unit_definitionOfXInWildcard :: Assertion
unit_definitionOfXInWildcard = definitionOfXInWildcard @Standard

unit_referenceOfXInWildcard :: Assertion
unit_referenceOfXInWildcard = referenceOfXInWildcard @Standard

unit_type_of_heap_const :: Assertion
unit_type_of_heap_const = typeOfHeapConst @Standard

unit_type_of_heap_arg :: Assertion
unit_type_of_heap_arg = typeOfHeapArg @Standard

unit_type_of_let :: Assertion
unit_type_of_let = typeOfLet @Standard

unit_type_of_pascaligo_lambda_arg :: Assertion
unit_type_of_pascaligo_lambda_arg = typeOfPascaligoLambdaArg @Standard

unit_pascaligo_local_type :: Assertion
unit_pascaligo_local_type = pascaligoLocalType @Standard

-- See LIGO-110
-- unit_type_of_camligo_lambda_arg :: Assertion
-- unit_type_of_camligo_lambda_arg = typeOfCamligoLambdaArg @Standard
