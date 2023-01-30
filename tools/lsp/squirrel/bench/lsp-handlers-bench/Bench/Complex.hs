module Bench.Complex
  ( bench_complex
  ) where

import Criterion

import AST.Scope (ScopingSystem (..))

import Test.Common.LSP (openLigoDoc)

import Bench.Hovers
import Bench.References
import Bench.Util
import Language.LSP.Test (documentContents)

bench_complex :: [Benchmark]
bench_complex =
  [ bgroup ("Complex benchmarks, " <> show scopingSystem)
    [ b scopingSystem
    | b <- [ complexBenchBaseDaoReferencesEdits
           , complexBenchBaseDaoReferencesKeystrokes
           , complexBenchBaseDaoHoversKeystrokes
           , complexBenchBaseDaoHoversKeystrokes2
           , complexBenchBigFileReferencesKeystrokes
           ]
    ]
  | scopingSystem <- [FallbackScopes, StandardScopes]
  ]

-- we need reindexing here
-- Optionally we can simulate keystrokes. It's a bit expensive itself,
-- but comparison of Fallback and Standard is fair
-- If reindexing happen on each update (and not before references request), we do a lot extra work
complexBenchBaseDaoReferencesEdits, complexBenchBaseDaoReferencesKeystrokes
  :: ScopingSystem -> Benchmark
(complexBenchBaseDaoReferencesEdits,complexBenchBaseDaoReferencesKeystrokes) =
  (runWithInsertMethod insertToDoc, runWithInsertMethod insertToDocKeystrokes)
  where
    runWithInsertMethod insert = benchLspSession "baseDAO references edits" baseDAO $ do
      errorCodesDoc <- OpenedDoc <$> openLigoDoc "error_codes.mligo"
      refs1 <- requestReferences $ ReferencesRequest -- fail_proposal_check
          { rrProject = baseDAO
          , rrFile = errorCodesDoc
          , rrPos = (24, 15)
          , rrExpectedAmount = 4
          }
      insert errorCodesDoc (95,1) "let q1 = fail_proposal_check"
      refs2 <- requestReferences $ ReferencesRequest -- fail_proposal_check
          { rrProject = baseDAO
          , rrFile = errorCodesDoc
          , rrPos = (24, 15)
          , rrExpectedAmount = 5
          }
      insert errorCodesDoc (96,1) "let q2 = fail_proposal_check"
      refs3 <- requestReferences $ ReferencesRequest -- fail_proposal_check
          { rrProject = baseDAO
          , rrFile = errorCodesDoc
          , rrPos = (24, 15)
          , rrExpectedAmount = 6
          }
      pure (refs1, refs2, refs3)

-- only hovers, so indexing not needed  here at all
complexBenchBaseDaoHoversKeystrokes :: ScopingSystem -> Benchmark
complexBenchBaseDaoHoversKeystrokes = benchLspSession "baseDAO hovers keystrokes" baseDAO $ do
  errorCodesDoc <- OpenedDoc <$> openLigoDoc "error_codes.mligo"
  h1 <- requestHover $ HoverRequest
      { hrProject = baseDAO
      , hrFile = errorCodesDoc
      , hrPos = (24, 15)
      , hrExpectedName = "fail_proposal_check"
      }
  insertToDocKeystrokes errorCodesDoc (95,1) "let q1 = fail_proposal_check"
  h2 <- requestHover $ HoverRequest
      { hrProject = baseDAO
      , hrFile = errorCodesDoc
      , hrPos = (24, 15)
      , hrExpectedName = "fail_proposal_check"
      }
  insertToDocKeystrokes errorCodesDoc (96,1) "let q2 = fail_proposal_check"
  h3 <- requestHover $ HoverRequest
      { hrProject = baseDAO
      , hrFile = errorCodesDoc
      , hrPos = (24, 15)
      , hrExpectedName = "fail_proposal_check"
      }
  pure (h1, h2, h3)

-- corner case - biggest get-scope JSON size
complexBenchBaseDaoHoversKeystrokes2 :: ScopingSystem -> Benchmark
complexBenchBaseDaoHoversKeystrokes2 = benchLspSession "baseDAO hovers keystrokes 2" baseDAO $ do
  baseDAOdoc <- OpenedDoc <$> openLigoDoc "base_DAO.mligo"
  insertToDocKeystrokes baseDAOdoc (60,1) "let q1 = nil_op"
  requestReferences $ ReferencesRequest
      { rrProject = baseDAO
      , rrFile = baseDAOdoc
      , rrPos = (60, 5)
      , rrExpectedAmount = 1
      }
  -- XXX : For some reason hover fail without references request above
  -- we should investigate this, e.g add logging for `virtualFileText`
  requestHover $ HoverRequest
      { hrProject = baseDAO
      , hrFile = baseDAOdoc
      , hrPos = (60, 10)
      , hrExpectedName = "nil_op"
      }


complexBenchBigFileReferencesKeystrokes :: ScopingSystem -> Benchmark
complexBenchBigFileReferencesKeystrokes  = benchLspSession "one_big_file references keystrokes" projectWithOneBigFile $ do
  theDoc <- OpenedDoc <$> openLigoDoc "one_big_file.mligo"
  refs1 <- requestReferences $ ReferencesRequest -- a022
    { rrProject = baseDAO
    , rrFile = theDoc
    , rrPos = (24, 15)
    , rrExpectedAmount = 2
    }
  insertToDocKeystrokes theDoc (1001, 1) "let q1 = 1234 + 5678 + 90"
  refs2 <- requestReferences $ ReferencesRequest -- a023
    { rrProject = baseDAO
    , rrFile = theDoc
    , rrPos = (25, 15)
    , rrExpectedAmount = 2
    }
  insertToDocKeystrokes theDoc (1002, 1) "let q2 = 1234 + 5678 + 90"
  refs3 <- requestReferences $ ReferencesRequest -- a024
    { rrProject = baseDAO
    , rrFile = theDoc
    , rrPos = (26, 15)
    , rrExpectedAmount = 2
    }
  pure (refs1, refs2, refs3)
