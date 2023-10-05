module Bench.Completion
  ( CompletionRequest (..),
    requestCompletion,
    bench_simple_completions,
  )
where

import Universum hiding ((^.))

import Bench.Util
import Criterion
import Language.LSP.Protocol.Lens qualified as LspLens
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Test qualified as LSP
import Lens.Micro ((^.))


data CompletionRequest = CompletionRequest
  { -- | Absolute file path to ligo project
    crProject :: FilePath,
    -- | path to file (relative to project)
    crFile :: Doc,
    -- | line/column as in VSCode (cursor is at the start of the word we want to get completion)
    crPos :: (LSP.UInt, LSP.UInt),
    -- | What completions do we expect?
    crExpectedSubset :: [Text]
  }
  deriving stock (Show)

requestCompletion :: CompletionRequest -> LSP.Session [LSP.CompletionItem]
requestCompletion cr@CompletionRequest {..} = do
  doc <- getDoc crFile
  completions <- LSP.getCompletions doc (LSP.Position (line - 1) (col - 1))
  mapM_ (checkContains completions) crExpectedSubset
  return completions
  where
    (line, col) = crPos

    checkContains completions element =
      unless (any ((== element) . (^. LspLens.label)) completions)
      $ fail (toString element <> " not found in completions for " <> show cr)


simpleCompletionBench :: CompletionRequest -> Benchmark
simpleCompletionBench cr@CompletionRequest {..} =
  benchLspSession
    (show crFile <> "/" <> show crPos)
    crProject
    $ requestCompletion cr

completionsOneSmallFile :: [CompletionRequest]
completionsOneSmallFile =
  [ CompletionRequest
      { crProject = projectWithOneSmallFile
      , crFile = FileDoc "one_small_file.mligo"
      , crPos = (5,14)
      , crExpectedSubset = ["cons", "fold"]
      }
  , CompletionRequest
      { crProject = projectWithOneSmallFile
      , crFile = FileDoc "one_small_file.mligo"
      , crPos = (7,14)
      , crExpectedSubset = ["abs", "assert"]
      }
  ]

completionsOneBigFile :: [CompletionRequest]
completionsOneBigFile =
  [ CompletionRequest
      { crProject = projectWithOneBigFile
      , crFile = FileDoc "one_big_file.mligo"
      , crPos = (9,14)
      , crExpectedSubset = ["a005", "a007"]
      }
  , CompletionRequest
      { crProject = projectWithOneBigFile
      , crFile = FileDoc "one_big_file.mligo"
      , crPos = (777,14)
      , crExpectedSubset = ["a771", "a772"]
      }
  ]

bench_simple_completions :: [Benchmark]
bench_simple_completions =
  [ bgroup
      "Completions/simple"
      [ simpleCompletionBench cr
      | cr <- completionsOneSmallFile <> completionsOneBigFile
      ]
  ]
