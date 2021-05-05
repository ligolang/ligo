{-# LANGUAGE RecordWildCards #-}

module Test.Capabilities.Find
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

import Data.Foldable (for_)
import System.FilePath (takeFileName, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)
import Text.Printf (printf)

import AST (Fallback)
import AST.Capabilities.Find (definitionOf, referencesOf, typeDefinitionAt)
import AST.Scope.Common (HasScopeForest)
import Range (Range (..), interval, point)

import qualified Test.Capabilities.Util as Common (contractsDir)
import Test.FixedExpectations
  (HasCallStack, expectationFailure, shouldBe, shouldContain, shouldMatchList)
import Test.Util (readContractWithScopes)
import Test.Util.LigoEnv ()
-- Test.Util.LigoEnv for "instance HasLigoClient IO"

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "find"

-- | Represents an invariant relation between references and a
-- definition of some LIGO entity (a variable, a type etc).
--
-- To be more precise, for a given definition 'd' and a set of
-- references 'rs', the following holds:
--
-- > ∀ r ∈ rs∪{d}. definitionOf(r) = d ∧ referencesOf(r) = rs
--
-- If a definition is not given (i.e. 'Nothing'), check that no
-- reference has a definition.
--
-- > ∀ r ∈ rs. ¬definitionOf(r)
data DefinitionReferenceInvariant = DefinitionReferenceInvariant
  { driFile :: FilePath
  -- ^ origin file
  , driDesc :: String
  -- ^ textual representation of the entity
  , driDef :: Maybe Range
  -- ^ definition range, 'Nothing' if there should be no definition
  , driRefs :: [Range]
  -- ^ references ranges
  }

-- | Check that the given @DefinitionReferenceInvariant@ holds.
checkDefinitionReferenceInvariant
  :: HasCallStack => DefinitionReferenceInvariant -> Assertion
checkDefinitionReferenceInvariant DefinitionReferenceInvariant{..}
  = test @Fallback -- *> test @FromCompiler -- TODO uncomment when compiler scopes are fixed
  where
    test :: forall parser. HasScopeForest parser IO => Assertion
    test = do
      tree <- readContractWithScopes @parser driFile
      case driDef of
        Nothing ->
          for_ driRefs' $ \mention ->
            definitionOf mention tree `shouldBe` Nothing
        Just (label driFile -> expectedDef) ->
          for_ (expectedDef : driRefs') $ \mention -> do
            definitionOf mention tree `shouldBe` Just expectedDef
            case referencesOf mention tree of
              Nothing -> expectationFailure $
                printf "References of '%s' from '%s' are not found." driDesc driFile
              Just actualRefs -> actualRefs `shouldMatchList` expectedDef : driRefs'

    -- a tree parser labels ranges with files, so we should too to
    -- preserve equality
    driRefs' = map (label driFile) driRefs

label :: FilePath -> Range -> Range
label filepath r = r{ rFile = filename }
  where
    filename = takeFileName filepath

-- | Check if the given range corresponds to a definition of the given
-- entity in the given file.
checkIfDefinition :: FilePath -> Range -> Range -> Assertion
checkIfDefinition filepath (label filepath -> expectedDef) mention
  = test @Fallback -- *> test @FromCompiler -- TODO uncomment when compiler scopes are fixed
  where
    test :: forall parser. HasScopeForest parser IO => Assertion
    test = do
      tree <- readContractWithScopes @parser filepath
      definitionOf mention tree `shouldBe` Just expectedDef

-- | Check if the given range corresponds to a reference of the given
-- entity in the given file.
checkIfReference :: FilePath -> Range -> Range -> Assertion
checkIfReference filepath (label filepath -> expectedRef) mention
  = test @Fallback -- *> test @FromCompiler -- TODO uncomment when compiler scopes are fixed
  where
    test :: forall parser. HasScopeForest parser IO => Assertion
    test = do
      tree <- readContractWithScopes @parser filepath
      case referencesOf mention tree of
        Nothing -> expectationFailure $
          printf "References in range '%s' from '%s' are not found."
            (show mention) filepath
        Just references -> references `shouldContain` [expectedRef]

invariants :: [DefinitionReferenceInvariant]
invariants =
  [ DefinitionReferenceInvariant
    { driFile = contractsDir </> "id.ligo"
    , driDesc = "i, parameter"
    , driDef = Just (interval 1 20 21)
    , driRefs = [interval 1 38 39]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "id.ligo"
    , driDesc = "id"
    , driDef = Just (interval 1 10 12)
    , driRefs = []
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "heap.ligo"
    , driDesc = "get_top"
    , driDef = Just (interval 8 10 17)
    , driRefs = [ interval 69 31 38
                , interval 25 31 38
                , interval 12 30 37
                ]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "heap.ligo"
    , driDesc = "left, local"
    , driDef = Just (interval 77 9 13)
    , driRefs = [ interval 99 15 19
                , interval 90 13 17
                , interval 89 30 34
                , interval 87 22 26
                , interval 86 36 40
                , interval 85 10 14
                , interval 84 16 20
                , interval 83 7 11
                ]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "params.mligo"
    , driDesc = "a, function"
    , driDef = Just (interval 1 5 6)
    , driRefs = []
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "params.mligo"
    , driDesc = "a, param"
    , driDef = Just (interval 3 11 12)
    , driRefs = [ interval 3 36 37 ]
    }

  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-attributes.ligo"
    , driDesc = "counter, type attribute"
    , driDef = Nothing -- type attributes don't have a declaration
    , driRefs = [interval 9 47 54, interval 13 34 41]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-attributes.ligo"
    , driDesc = "counter, function"
    , driDef = Just (interval 7 10 17)
    , driRefs = []
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-attributes-in-rec.ligo"
    , driDesc = "counter, type attribute"
    , driDef = Nothing -- type attributes don't have a declaration
    , driRefs = [interval 9 47 54, interval 13 34 41]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-attributes-in-rec.ligo"
    , driDesc = "counter, function"
    , driDef = Just (interval 7 20 27)
    , driRefs = []
    }

  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-attributes.mligo"
    , driDesc = "counter, type attribute"
    , driDef = Nothing  -- type attributes don't have a declaration
    , driRefs = [interval 9 3 10, interval 7 35 42]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-attributes.mligo"
    , driDesc = "counter, function"
    , driDef = Just (interval 6 5 12)
    , driRefs = []
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-attributes-in-rec.mligo"
    , driDesc = "counter, type attribute"
    , driDef = Nothing  -- type attributes don't have a declaration
    , driRefs = [interval 9 3 10, interval 7 35 42]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-attributes-in-rec.mligo"
    , driDesc = "counter, function"
    , driDef = Just (interval 6 9 16)
    , driRefs = []
    }

  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-attributes.religo"
    , driDesc = "counter, type attribute"
    , driDef = Nothing  -- type attributes don't have a declaration
    , driRefs = [interval 9 14 21, interval 7 35 42]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-attributes.religo"
    , driDesc = "counter, function"
    , driDef = Just (interval 6 5 12)
    , driRefs = []
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-attributes-in-rec.religo"
    , driDesc = "counter, type attribute"
    , driDef = Nothing  -- type attributes don't have a declaration
    , driRefs = [interval 9 14 21, interval 7 35 42]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-attributes-in-rec.religo"
    , driDesc = "counter, function"
    , driDef = Just (interval 6 9 16)
    , driRefs = []
    }

  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "recursion.ligo"
    , driDesc = "sum"
    , driDef = Just (interval 1 20 23)
    , driRefs = [interval 2 26 29]
    }

  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "recursion.mligo"
    , driDesc = "sum"
    , driDef = Just (interval 1 9 12)
    , driRefs = [interval 2 30 33]
    }

  -- * Does not pass because we have troubles with recursive functions
  -- * in ReasonLIGO: https://issues.serokell.io/issue/LIGO-70
  --
  -- , DefinitionReferenceInvariant
  --   { driFile = contractsDir </> "recursion.religo"
  --   , driDesc = "sum"
  --   , driDef = Just (interval 1 9 12)
  --   , driRefs = [interval 2 29 32]
  --   }

  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-constructor.ligo"
    , driDesc = "Increment, type constructor"
    , driDef = Just (interval 2 3 12)
    , driRefs = [interval 5 20 29]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-constructor.mligo"
    , driDesc = "Increment, type constructor"
    , driDef = Just (interval 2 3 12)
    , driRefs = [interval 5 18 27]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-constructor.religo"
    , driDesc = "Increment, type constructor"
    , driDef = Just (interval 2 5 14)
    , driRefs = [interval 5 18 27]
    }
  ]

test_findDefinitionAndGoToReferencesCorrespondence :: HasCallStack => TestTree
test_findDefinitionAndGoToReferencesCorrespondence =
  testGroup "Definition and References Correspondence" testCases
  where
    testCases = map makeTestCase invariants
    makeTestCase inv = testCase name (checkDefinitionReferenceInvariant inv)
      where name = driFile inv <> ": " <> driDesc inv

unit_definitionOfId :: Assertion
unit_definitionOfId = checkIfDefinition
                        (contractsDir </> "id.ligo")
                        (interval 1 20 21)
                        (interval 1 38 39)

unit_referenceOfId :: Assertion
unit_referenceOfId = checkIfReference
                       (contractsDir </> "id.ligo")
                       (interval 1 38 39)
                       (interval 1 20 21)

unit_definitionOfLeft :: Assertion
unit_definitionOfLeft = checkIfDefinition
                          (contractsDir </> "heap.ligo")
                          (interval 77 9 13)
                          (interval 86 36 40)

unit_referenceOfLeft :: Assertion
unit_referenceOfLeft = checkIfReference
                         (contractsDir </> "heap.ligo")
                         (interval 89 30 34)
                         (interval 77 9 13)

unit_definitionOfXInWildcard :: Assertion
unit_definitionOfXInWildcard = checkIfDefinition
  (contractsDir </> "wildcard.mligo")
  (interval 1 5 6)
  (interval 2 13 14)

unit_referenceOfXInWildcard :: Assertion
unit_referenceOfXInWildcard = checkIfReference
  (contractsDir </> "wildcard.mligo")
  (interval 2 13 14)
  (interval 1 5 6)

unit_type_of_heap_const :: Assertion
unit_type_of_heap_const = do
    tree <- readContractWithScopes @Fallback (contractsDir </> "heap.ligo")
    case typeDefinitionAt (point 106 8) tree of
      Nothing -> expectationFailure "Should find type definition"
      Just range -> range{rFile=""} `shouldBe` interval 4 6 10

unit_type_of_heap_arg :: Assertion
unit_type_of_heap_arg = do
    tree <- readContractWithScopes @Fallback (contractsDir </> "heap.ligo")
    case typeDefinitionAt (point 8 25) tree of
      Nothing -> expectationFailure "Should find type definition"
      Just range -> range{rFile=""} `shouldBe` interval 4 6 10

unit_type_of_let :: Assertion
unit_type_of_let = do
    tree <- readContractWithScopes @Fallback (contractsDir </> "type-attributes.mligo")
    case typeDefinitionAt (point 7 10) tree of
      Nothing -> expectationFailure "Should find type definition"
      Just range -> range{rFile=""} `shouldBe` interval 1 6 20

unit_type_of_pascaligo_lambda_arg :: Assertion
unit_type_of_pascaligo_lambda_arg = do
  tree <- readContractWithScopes @Fallback (contractsDir </> "lambda.ligo")
  case typeDefinitionAt (point 4 21) tree of
    Nothing -> expectationFailure "Should find type definition"
    Just range -> range{rFile=""} `shouldBe` interval 1 6 12

unit_pascaligo_local_type :: Assertion
unit_pascaligo_local_type = do
  tree <- readContractWithScopes @Fallback (contractsDir </> "local_type.ligo")
  case typeDefinitionAt (point 3 23) tree of
    Nothing -> expectationFailure "Should find type definition"
    Just range -> range{rFile=""} `shouldBe` interval 2 8 12

-- See LIGO-110
-- unit_type_of_camligo_lambda_arg :: Assertion
-- unit_type_of_camligo_lambda_arg = do
--     tree <- readContractWithScopes @Fallback (contractsDir </> "type-attributes.mligo")
--     case typeDefinitionAt (point 8 52) tree of
--       Nothing -> expectationFailure "Should find type definition"
--       Just range -> range{rFile=""} `shouldBe` interval 1 6 20
