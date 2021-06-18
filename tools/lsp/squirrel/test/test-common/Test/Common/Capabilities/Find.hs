{-# LANGUAGE RecordWildCards #-}

module Test.Common.Capabilities.Find
  ( findDefinitionAndGoToReferencesCorrespondence
  , definitionOfId
  , definitionOfLeft
  , referenceOfId
  , referenceOfLeft
  , definitionOfXInWildcard
  , referenceOfXInWildcard

  , typeOfHeapArg
  , typeOfHeapConst
  , typeOfLet
  , typeOfPascaligoLambdaArg
  , pascaligoLocalType
  ) where

import Data.Foldable (for_)
import System.FilePath ((</>))
import System.Directory (makeAbsolute)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)
import Text.Printf (printf)

import AST (HasScopeForest)
import AST.Capabilities.Find (definitionOf, referencesOf, typeDefinitionAt)
import Range (Range (..), interval, point)

import Test.Common.Capabilities.Util qualified as Common (contractsDir)
import Test.Common.FixedExpectations
  (expectationFailure, shouldBe, shouldContain, shouldMatchList)
import Test.Common.Util (readContractWithScopes)

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
  :: forall parser. HasScopeForest parser IO => DefinitionReferenceInvariant -> Assertion
checkDefinitionReferenceInvariant DefinitionReferenceInvariant{..} = test
  where
    test :: Assertion
    test = do
      driFile' <- makeAbsolute driFile
      -- a tree parser labels ranges with files, so we should too to preserve equality
      driRefs' <- traverse (label driFile') driRefs

      tree <- readContractWithScopes @parser driFile'
      case driDef of
        Nothing ->
          for_ driRefs' \mention ->
            definitionOf mention tree `shouldBe` Nothing
        Just expectedDef -> do
          expectedDef' <- label driFile' expectedDef
          definitionOf expectedDef' tree `shouldBe` Just expectedDef'
          case referencesOf expectedDef' tree of
            Nothing -> expectationFailure $
              printf "References of '%s' from '%s' are not found." driDesc driFile'
            Just actualRefs -> actualRefs `shouldMatchList` expectedDef' : driRefs'

label :: FilePath -> Range -> IO Range
label filepath r
  | null (rFile r) = pure r{ rFile = filepath }
  | otherwise      = fmap (\fp -> r{ rFile = fp }) (makeAbsolute $ rFile r)

-- | Check if the given range corresponds to a definition of the given
-- entity in the given file.
checkIfDefinition :: forall parser. HasScopeForest parser IO => FilePath -> Range -> Range -> Assertion
checkIfDefinition filepath expectedDef mention = test
  where
    test :: Assertion
    test = do
      filepath' <- makeAbsolute filepath
      expectedDef' <- label filepath' expectedDef
      mention' <- label filepath' mention
      tree <- readContractWithScopes @parser filepath'
      definitionOf mention' tree `shouldBe` Just expectedDef'

-- | Check if the given range corresponds to a reference of the given
-- entity in the given file.
checkIfReference :: forall parser. HasScopeForest parser IO => FilePath -> Range -> Range -> Assertion
checkIfReference filepath expectedRef mention = test
  where
    test :: Assertion
    test = do
      filepath' <- makeAbsolute filepath
      expectedRef' <- label filepath' expectedRef
      mention' <- label filepath' mention
      tree <- readContractWithScopes @parser filepath'
      case referencesOf mention' tree of
        Nothing -> expectationFailure $
          printf "References in range '%s' from '%s' are not found."
            (show mention') filepath'
        Just references -> references `shouldContain` [expectedRef']

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
    { driFile = contractsDir </> "type-attributes-tuple.mligo"
    , driDesc = "tuple member"
    , driDef = Just (interval 2 12 15)
    , driRefs = [interval 3 3 6]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-attributes-tuple.religo"
    , driDesc = "tuple member"
    , driDef = Just (interval 2 13 16)
    , driRefs = [interval 3 3 6]
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
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "includes" </> "A1.mligo"
    , driDesc = "a1, find references in other files"
    , driDef = Just (interval 1 5 7)
    , driRefs =
      [ (interval 3 10 12){rFile = contractsDir </> "includes" </> "A2.mligo"}
      , (interval 3 10 12){rFile = contractsDir </> "includes" </> "A3.mligo"}
      ]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "includes" </> "B3.ligo"
    , driDesc = "b3, relative directories"
    , driDef = Just (interval 1 7 9)
    , driRefs =
      [ (interval 3 21 23){rFile = contractsDir </> "includes" </> "B1.ligo"}
      , (interval 3 12 14){rFile = contractsDir </> "includes" </> "B2" </> "B2.ligo"}
      ]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "includes" </> "C2.religo"
    , driDesc = "c2, find references in other files"
    , driDef = Just (interval 1 5 7)
    , driRefs =
      [ (interval 4 15 17){rFile = contractsDir </> "includes" </> "C1.mligo"}
      ]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "includes" </> "C3.mligo"
    , driDesc = "c3, find references in other files"
    , driDef = Just (interval 1 5 7)
    , driRefs =
      [ (interval 4 10 12){rFile = contractsDir </> "includes" </> "C1.mligo"}
      ]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "includes" </> "D1.ligo"
    , driDesc = "d, no references"
    , driDef = Just (interval 1 7 8)
    , driRefs = []
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "includes" </> "D2.ligo"
    , driDesc = "d, no references"
    , driDef = Just (interval 1 7 8)
    , driRefs = []
    }
  ]

findDefinitionAndGoToReferencesCorrespondence :: forall impl. HasScopeForest impl IO => TestTree
findDefinitionAndGoToReferencesCorrespondence =
  testGroup "Definition and References Correspondence" testCases
  where
    testCases = map makeTestCase invariants
    makeTestCase inv = testCase name (checkDefinitionReferenceInvariant @impl inv)
      where name = driFile inv <> ": " <> driDesc inv

definitionOfId :: forall impl. HasScopeForest impl IO => Assertion
definitionOfId = checkIfDefinition @impl
                        (contractsDir </> "id.ligo")
                        (interval 1 20 21)
                        (interval 1 38 39)

referenceOfId :: forall impl. HasScopeForest impl IO => Assertion
referenceOfId = checkIfReference @impl
                       (contractsDir </> "id.ligo")
                       (interval 1 38 39)
                       (interval 1 20 21)

definitionOfLeft :: forall impl. HasScopeForest impl IO => Assertion
definitionOfLeft = checkIfDefinition @impl
                          (contractsDir </> "heap.ligo")
                          (interval 77 9 13)
                          (interval 86 36 40)

referenceOfLeft :: forall impl. HasScopeForest impl IO => Assertion
referenceOfLeft = checkIfReference @impl
                         (contractsDir </> "heap.ligo")
                         (interval 89 30 34)
                         (interval 77 9 13)

definitionOfXInWildcard :: forall impl. HasScopeForest impl IO => Assertion
definitionOfXInWildcard = checkIfDefinition @impl
  (contractsDir </> "wildcard.mligo")
  (interval 1 5 6)
  (interval 2 13 14)

referenceOfXInWildcard :: forall impl. HasScopeForest impl IO => Assertion
referenceOfXInWildcard = checkIfReference @impl
  (contractsDir </> "wildcard.mligo")
  (interval 2 13 14)
  (interval 1 5 6)

typeOf
  :: forall impl. HasScopeForest impl IO
  => FilePath
  -> Range
  -> Range
  -> Assertion
typeOf filepath mention definition = do
  mention' <- label filepath mention
  definition' <- label filepath definition
  tree <- readContractWithScopes @impl filepath
  case typeDefinitionAt mention' tree of
    Nothing -> expectationFailure "Should find type definition"
    Just range -> range{rFile=rFile mention'} `shouldBe` definition'

typeOfHeapConst :: forall impl. HasScopeForest impl IO => Assertion
typeOfHeapConst = typeOf @impl (contractsDir </> "heap.ligo") (point 106 8) (interval 4 6 10)

typeOfHeapArg :: forall impl. HasScopeForest impl IO => Assertion
typeOfHeapArg = typeOf @impl (contractsDir </> "heap.ligo") (point 8 25) (interval 4 6 10)

typeOfLet :: forall impl. HasScopeForest impl IO => Assertion
typeOfLet = typeOf @impl (contractsDir </> "type-attributes.mligo") (point 7 10) (interval 1 6 20)

typeOfPascaligoLambdaArg :: forall impl. HasScopeForest impl IO => Assertion
typeOfPascaligoLambdaArg = typeOf @impl (contractsDir </> "lambda.ligo") (point 4 21) (interval 1 6 12)

pascaligoLocalType :: forall impl. HasScopeForest impl IO => Assertion
pascaligoLocalType = typeOf @impl (contractsDir </> "local_type.ligo") (point 3 23) (interval 2 8 12)

-- See LIGO-110
-- typeOfCamligoLambdaArg :: Assertion
-- typeOfCamligoLambdaArg = typeOf (contractsDir </> "type-attributes.mligo") (point 8 52) (interval 1 6 20)
