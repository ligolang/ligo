module Bench.References
  ( ReferencesRequest(..)
  , requestReferences

  , bench_simple_references
  , bench_sequence_references
  ) where

import Criterion
import Language.LSP.Test qualified as LSP
import Language.LSP.Types qualified as LSP

import AST.Scope (ScopingSystem (..))

import Bench.Orphans ()
import Bench.Util

data ReferencesRequest = ReferencesRequest
  { rrProject :: FilePath
    -- ^ Absolute file path to ligo project
  , rrFile :: Doc
  -- ^ path to file (relative to project)
  , rrPos :: (LSP.UInt, LSP.UInt)
    -- ^ line/column as in VSCode (cursor is at the start of the word we want to get references)
  , rrExpectedAmount :: Int
    -- ^ number of references we expect, including definition
  } deriving stock Show

type instance PrettyShow ReferencesRequest = ()

requestReferences :: ReferencesRequest -> LSP.Session [LSP.Location]
requestReferences rr@ReferencesRequest{..} = do
  doc <- getDoc rrFile
  LSP.List refs <- LSP.getReferences doc (LSP.Position (line - 1) (col - 1)) True
  if length refs == rrExpectedAmount
    then return refs
    else fail $ "Request " <> show rr <> " returned unexpected amount of references: "
      <> show (length refs)
  where
    (line, col) = rrPos

bench_simple_references :: [Benchmark]
bench_simple_references =
  [ bgroup ("Simple references, " <> show scopingSystem)
    [ simpleReferencesBench rr scopingSystem
    | rr <- referencesBaseDAO <> referencesOneBigFile
    ]
  | scopingSystem <- [FallbackScopes, StandardScopes]
  ]

simpleReferencesBench :: ReferencesRequest -> ScopingSystem -> Benchmark
simpleReferencesBench rr@ReferencesRequest{..} =
  benchLspSession (show rrFile <> ", " <> show rrPos) rrProject $ requestReferences rr

referencesBaseDAO :: [ReferencesRequest]
referencesBaseDAO =
  [ ReferencesRequest -- fail_proposal_check
      { rrProject = baseDAO
      , rrFile = FileDoc "error_codes.mligo"
      , rrPos = (24, 15)
      , rrExpectedAmount = 4
      }
  , ReferencesRequest -- proposal_not_exist
      { rrProject = baseDAO
      , rrFile = FileDoc "error_codes.mligo"
      , rrPos = (27, 15)
      , rrExpectedAmount = 3
      }
  , ReferencesRequest -- not_enough_frozen_tokens
      { rrProject = baseDAO
      , rrFile = FileDoc "error_codes.mligo"
      , rrPos = (54, 15)
      , rrExpectedAmount = 4
      }
  ]

referencesOneBigFile :: [ReferencesRequest]
referencesOneBigFile =
  [ ReferencesRequest -- a012
      { rrProject = projectWithOneBigFile
      , rrFile = FileDoc "one_big_file.mligo"
      , rrPos = (13, 5)
      , rrExpectedAmount = 2
      }
  , ReferencesRequest -- a013
      { rrProject = projectWithOneBigFile
      , rrFile = FileDoc "one_big_file.mligo"
      , rrPos = (13, 5)
      , rrExpectedAmount = 2
      }
  , ReferencesRequest -- a014
      { rrProject = projectWithOneBigFile
      , rrFile = FileDoc "one_big_file.mligo"
      , rrPos = (13, 5)
      , rrExpectedAmount = 2
      }
  ]

-- request all references from a project in one session
-- to reduce influence of initializing costs
bench_sequence_references :: [Benchmark]
bench_sequence_references =
  [ benchLspSession
      ("sequence references " <> projectName<> ", " <> show scopingSystem)
      project
      (mapM requestReferences requests)
      scopingSystem
  | (projectName, project, requests) <-
    [ ("baseDAO", baseDAO, referencesBaseDAO)
    , ("one_big_file", projectWithOneBigFile, referencesOneBigFile)
    ]
  , scopingSystem <- [FallbackScopes, StandardScopes]
  ]
