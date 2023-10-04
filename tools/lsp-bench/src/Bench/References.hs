module Bench.References
  ( ReferencesRequest(..)
  , requestReferences
  , bench_simple_references
  , bench_sequence_references
  ) where

import Universum

import Criterion
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Test qualified as LSP

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

requestReferences :: ReferencesRequest -> LSP.Session [LSP.Location]
requestReferences rr@ReferencesRequest{..} = do
  doc <- getDoc rrFile
  refs <- LSP.getReferences doc (LSP.Position (line - 1) (col - 1)) True
  if length refs == rrExpectedAmount
    then return refs
    else fail $ "Request " <> show rr <> " returned unexpected amount of references: "
      <> show (length refs)
  where
    (line, col) = rrPos

bench_simple_references :: [Benchmark]
bench_simple_references =
  [ bgroup "References/simple"
    [ simpleReferencesBench rr
    | rr <- referencesOneBigFile <> referencesChecker
    ]
  ]

simpleReferencesBench :: ReferencesRequest -> Benchmark
simpleReferencesBench rr@ReferencesRequest{..} =
  benchLspSession OnDocumentLink
    (show rrFile <> "/" <> show rrPos)
    rrProject
    $ requestReferences rr

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

referencesChecker :: [ReferencesRequest]
referencesChecker =
  [
    ReferencesRequest -- ref_peek_front
      { rrProject = projectChecker
      , rrFile = FileDoc "avl.mligo"
      , rrPos = (535, 9)
      , rrExpectedAmount = 4
      }
  , ReferencesRequest -- parent_ptr
      { rrProject = projectChecker
      , rrFile = FileDoc "avl.mligo"
      , rrPos = (477, 7)
      , rrExpectedAmount = 6
      }
  , ReferencesRequest -- left
      { rrProject = projectChecker
      , rrFile = FileDoc "avl.mligo"
      , rrPos = (359, 7)
      , rrExpectedAmount = 7
      }
  ]

-- request many references from a file in one session
-- to check that our caching works correctly. It's expected that
-- the result will be close to case when we ask for one reference.
bench_sequence_references :: [Benchmark]
bench_sequence_references =
  [ benchSequence BenchmarkSequence
      { bsName = "References/sequence"
      , bsRequests = referencesOneBigFile
      , bsGetRequestDoc = rrFile
      , bsSetRequestDoc = \doc rr -> rr {rrFile = doc}
      , bsRunRequest = requestReferences
      , bsProject = projectWithOneBigFile
      }
  , benchSequence BenchmarkSequence
      { bsName = "References/sequence"
      , bsRequests = referencesChecker
      , bsGetRequestDoc = rrFile
      , bsSetRequestDoc = \doc rr -> rr {rrFile = doc}
      , bsRunRequest = requestReferences
      , bsProject = projectChecker
      }
  ]
