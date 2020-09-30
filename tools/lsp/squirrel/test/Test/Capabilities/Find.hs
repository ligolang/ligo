{-# LANGUAGE RecordWildCards, TupleSections #-}

module Test.Capabilities.Find
  ( unit_findDefinitionAndGoToReferencesCorrespondence
  , unit_definitionOfId
  , unit_definitionOfLeft
  , unit_referenceOfId
  , unit_referenceOfLeft
  ) where

import Data.Foldable (for_)
import System.FilePath ((</>))
import System.FilePath (takeFileName)
import Test.Hspec.Expectations
  (HasCallStack, expectationFailure, shouldBe, shouldContain, shouldMatchList)
import Test.HUnit (Assertion)
import Text.Printf (printf)

import AST.Capabilities.Find (definitionOf, referencesOf)
import AST.Scope (addLocalScopes)
import Range (Range (..), interval)

import Test.Capabilities.Util (contractsDir)
import Test.Util (readContract)

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
checkDefinitionReferenceInvariant DefinitionReferenceInvariant{..} = do
  tree <- addLocalScopes <$> readContract driFile
  case driDef of
    Nothing -> do
      for_ driRefs' $ \mention -> do
        definitionOf mention tree `shouldBe` Nothing
    Just (label driFile -> expectedDef) -> do
      for_ (expectedDef : driRefs') $ \mention -> do
        definitionOf mention tree `shouldBe` Just expectedDef
        case referencesOf mention tree of
          Nothing -> expectationFailure $
            printf "References of '%s' from '%s' are not found." driDesc driFile
          Just actualRefs -> actualRefs `shouldMatchList` driRefs'
  where
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
checkIfDefinition filepath (label filepath -> expectedDef) mention = do
  tree <- addLocalScopes <$> readContract filepath
  definitionOf mention tree `shouldBe` Just expectedDef

-- | Check if the given range corresponds to a reference of the given
-- entity in the given file.
checkIfReference :: FilePath -> Range -> Range -> Assertion
checkIfReference filepath (label filepath -> expectedRef) mention = do
  tree <- addLocalScopes <$> readContract filepath
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
    { driFile = contractsDir </> "recursion.religo"
    , driDesc = "sum"
    , driDef = Just (interval 1 9 12)
    , driRefs = [interval 2 29 32]
    }
  ]

unit_findDefinitionAndGoToReferencesCorrespondence :: Assertion
unit_findDefinitionAndGoToReferencesCorrespondence =
  mapM_ checkDefinitionReferenceInvariant invariants

unit_definitionOfId :: Assertion
unit_definitionOfId = (checkIfDefinition
                        (contractsDir </> "id.ligo")
                        (interval 1 20 21)
                        (interval 1 38 39))

unit_referenceOfId :: Assertion
unit_referenceOfId = (checkIfReference
                       (contractsDir </> "id.ligo")
                       (interval 1 38 39)
                       (interval 1 20 21))

unit_definitionOfLeft :: Assertion
unit_definitionOfLeft = (checkIfDefinition
                          (contractsDir </> "heap.ligo")
                          (interval 77 9 13)
                          (interval 86 36 40))

unit_referenceOfLeft :: Assertion
unit_referenceOfLeft = (checkIfReference
                         (contractsDir </> "heap.ligo")
                         (interval 89 30 34)
                         (interval 77 9 13))
