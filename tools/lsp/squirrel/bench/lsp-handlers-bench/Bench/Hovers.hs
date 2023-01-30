{-# OPTIONS_GHC -Wno-orphans #-}

module Bench.Hovers
  ( HoverRequest (..)
  , requestHover

  , bench_simple_hovers
  , bench_sequence_hovers
  ) where

import Criterion
import Data.Text qualified as T
import Language.LSP.Test qualified as LSP
import Language.LSP.Types qualified as LSP

import AST.Scope

import Bench.Orphans ()
import Bench.Util
import Debug qualified

data HoverRequest = HoverRequest
  { hrProject :: FilePath
  -- ^ Absolute file path to ligo project
  , hrFile :: Doc
  -- ^ path to file (relative to project)
  , hrPos :: (LSP.UInt, LSP.UInt)
  -- ^ line/column as in VSCode (cursor is at the start of the word we want to get hover)
  , hrExpectedName :: Text
  -- ^ Thing that we're hovering. To check that position is correct, we're checking that this is a
  -- prefix of the hover content
  } deriving stock Show

type instance PrettyShow HoverRequest = ()

requestHover :: HoverRequest -> LSP.Session LSP.Hover
requestHover hr@HoverRequest{..} = do
  doc <- getDoc hrFile
  LSP.getHover doc (LSP.Position (line - 1) (col - 1)) >>= \case
    Nothing -> fail $ "No hover for " <> show hr
    Just result@(LSP.Hover (LSP.HoverContents (LSP.MarkupContent _ msg)) _)
      | hrExpectedName `T.isPrefixOf` withoutDialect msg -> return result
    otherHover -> fail $ "Hover for " <> show hr <> " is malformed: " <> Debug.show otherHover
  where
    (line, col) = hrPos
    withoutDialect = T.tail . T.dropWhile (/= '\n')

bench_simple_hovers :: [Benchmark]
bench_simple_hovers =
  [ bgroup ("Simple hovers, " <> show scopingSystem)
    [ simpleHoverBench hr scopingSystem
    | hr <- hoversBaseDAO <> hoversOneBigFile
    ]
  | scopingSystem <- [FallbackScopes, StandardScopes]
  ]

simpleHoverBench :: HoverRequest -> ScopingSystem -> Benchmark
simpleHoverBench hr@HoverRequest{..} =
  benchLspSession
    (show hrFile <> ", " <> toString hrExpectedName)
    hrProject
    $ requestHover hr

hoversBaseDAO :: [HoverRequest]
hoversBaseDAO =
  [ HoverRequest -- easiest one
    { hrProject = baseDAO
    , hrFile = FileDoc "types.mligo"
    , hrPos = (323,5)
    , hrExpectedName = "nil_op"
    }
  , HoverRequest -- defined in types.mligo
    { hrProject = baseDAO
    , hrFile = FileDoc "management.mligo"
    , hrPos = (19,7)
    , hrExpectedName = "nil_op"
    }
  , HoverRequest
    { hrProject = baseDAO  -- this file is really larger and have more imports
    , hrFile = FileDoc "proposal.mligo"
    , hrPos = (73,5)
    , hrExpectedName = "nil_op"
    }
  , HoverRequest
    { hrProject = baseDAO  -- biggest file, includes every file in project
    , hrFile = FileDoc "base_DAO.mligo"
    , hrPos = (24,7)
    , hrExpectedName = "Drop_proposal"
    }
  ]

hoversOneBigFile :: [HoverRequest]
hoversOneBigFile =
  [ HoverRequest
    { hrProject = projectWithOneBigFile
    , hrFile = FileDoc "one_big_file.mligo"
    , hrPos = (2,12)
    , hrExpectedName = "a000"
    }
  , HoverRequest
    { hrProject = projectWithOneBigFile
    , hrFile = FileDoc "one_big_file.mligo"
    , hrPos = (3,12)
    , hrExpectedName = "a001"
    }
  , HoverRequest
    { hrProject = projectWithOneBigFile
    , hrFile = FileDoc "one_big_file.mligo"
    , hrPos = (4,12)
    , hrExpectedName = "a002"
    }
  , HoverRequest
    { hrProject = projectWithOneBigFile
    , hrFile = FileDoc "one_big_file.mligo"
    , hrPos = (5,12)
    , hrExpectedName = "a003"
    }
  ]

-- request all hovers from a project in one session
-- to reduce influence of initializing costs
bench_sequence_hovers :: [Benchmark]
bench_sequence_hovers =
  [ benchLspSession
    ("sequence hovers " <> projectName <> ", " <> show scopingSystem)
    project
    (mapM requestHover requests)
    scopingSystem
  | (projectName, project, requests) <-
    [ ("baseDAO", baseDAO, hoversBaseDAO)
    , ("one_big_file", projectWithOneBigFile, hoversOneBigFile)
    ]
  , scopingSystem <- [FallbackScopes, StandardScopes]
  ]
