{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Bench.Hovers
  ( HoverRequest (..),
    requestHover,
    bench_simple_hovers,
    bench_sequence_hovers,
  )
where

import Universum

import Bench.Util
import Criterion
import Data.Row.Records (Label (..), pattern (:+), pattern (:==))
import Data.Text qualified as T
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Test qualified as LSP

data HoverRequest = HoverRequest
  { -- | Absolute file path to ligo project
    hrProject :: FilePath,
    -- | path to file (relative to project)
    hrFile :: Doc,
    -- | line/column as in VSCode (cursor is at the start of the word we want to get hover)
    hrPos :: (LSP.UInt, LSP.UInt),
    -- | Thing that we're hovering. To check that position is correct, we're checking that this is a
    -- prefix of the hover content
    hrExpectedName :: Text
  }
  deriving stock (Show)

requestHover :: HoverRequest -> LSP.Session LSP.Hover
requestHover hr@HoverRequest {..} = do
  doc <- getDoc hrFile
  LSP.getHover doc (LSP.Position (line - 1) (col - 1)) >>= \case
    Nothing -> fail $ "No hover for " <> show hr
    Just result@(LSP.Hover {_contents = LSP.InR (LSP.InR [LSP.MarkedString (LSP.InR ((_ :: Label "language") :== "cameligo" :+ _ :== msg))])})
      | hrExpectedName `T.isPrefixOf` msg -> return result
    otherHover -> fail $ "Hover for " <> show hr <> " is malformed (expected MarkedString): " <> show otherHover
  where
    (line, col) = hrPos

bench_simple_hovers :: [Benchmark]
bench_simple_hovers =
  [ bgroup
      "Hovers/simple"
      [ simpleHoverBench hr
        | hr <- hoversOneBigFile
      ]
  ]

simpleHoverBench :: HoverRequest -> Benchmark
simpleHoverBench hr@HoverRequest {..} =
  benchLspSession
    (show hrFile <> "/" <> toString hrExpectedName)
    hrProject
    $ requestHover hr

hoversOneBigFile :: [HoverRequest]
hoversOneBigFile =
  [ HoverRequest
      { hrProject = projectWithOneBigFile,
        hrFile = FileDoc "one_big_file.mligo",
        hrPos = (2, 12),
        hrExpectedName = "a000"
      },
    HoverRequest
      { hrProject = projectWithOneBigFile,
        hrFile = FileDoc "one_big_file.mligo",
        hrPos = (103, 12),
        hrExpectedName = "a101"
      },
    HoverRequest
      { hrProject = projectWithOneBigFile,
        hrFile = FileDoc "one_big_file.mligo",
        hrPos = (604, 12),
        hrExpectedName = "a602"
      },
    HoverRequest
      { hrProject = projectWithOneBigFile,
        hrFile = FileDoc "one_big_file.mligo",
        hrPos = (905, 12),
        hrExpectedName = "a903"
      }
  ]

-- request many hovers from a file in one session
-- to check that our caching works correctly. It's expected that
-- the result will be close to case when we ask for one hover.
bench_sequence_hovers :: [Benchmark]
bench_sequence_hovers =
  [ benchSequence BenchmarkSequence
      { bsName = "Hovers/sequence"
      , bsRequests = hoversOneBigFile
      , bsGetRequestDoc = hrFile
      , bsSetRequestDoc = \doc hr -> hr {hrFile = doc}
      , bsRunRequest = requestHover
      , bsProject = projectWithOneBigFile
      }
  ]
