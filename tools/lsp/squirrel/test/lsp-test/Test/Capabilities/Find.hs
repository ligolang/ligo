module Test.Capabilities.Find
  ( test_findDefinitionAndGoToReferencesCorrespondence
  , unit_definitionOfId
  , unit_referenceOfId
  , unit_definitionOfXInWildcard
  , unit_referenceOfXInWildcard

  , unit_type_of_let
  , unit_type_of_jsligo_lambda_arg
  , unit_cameligo_local_type
  ) where

import AST.Scope (Fallback)

import Test.Common.Capabilities.Find
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion)

test_findDefinitionAndGoToReferencesCorrespondence :: IO TestTree
test_findDefinitionAndGoToReferencesCorrespondence =
  findDefinitionAndGoToReferencesCorrespondence @Fallback invariants

unit_definitionOfId :: Assertion
unit_definitionOfId = definitionOfId @Fallback

unit_referenceOfId :: Assertion
unit_referenceOfId = referenceOfId @Fallback

unit_definitionOfXInWildcard :: Assertion
unit_definitionOfXInWildcard = definitionOfXInWildcard @Fallback

unit_referenceOfXInWildcard :: Assertion
unit_referenceOfXInWildcard = referenceOfXInWildcard @Fallback

unit_type_of_let :: Assertion
unit_type_of_let = typeOfLet @Fallback

unit_type_of_jsligo_lambda_arg :: Assertion
unit_type_of_jsligo_lambda_arg = typeOfJsligoLambdaArg @Fallback

unit_cameligo_local_type :: Assertion
unit_cameligo_local_type = cameligoLocalType @Fallback

-- See LIGO-110
-- unit_type_of_camligo_lambda_arg :: Assertion
-- unit_type_of_camligo_lambda_arg = typeOfCamligoLambdaArg @Fallback
