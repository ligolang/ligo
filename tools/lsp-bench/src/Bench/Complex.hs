module Bench.Complex
  ( bench_complex,
  )
where

import Universum

import Bench.References
import Bench.Util
import Criterion

bench_complex :: [Benchmark]
bench_complex =
  [ bgroup
      "Complex benchmarks"
      [ complexBenchBigFileReferencesEdits
      , complexBenchBigFileReferencesKeystrokes
      ]
  ]

-- we need reindexing here
-- Optionally we can simulate keystrokes. It's a bit expensive itself,
-- but we could still see the impact of our optimisations.
-- If reindexing happen on each update (and not before references request), we do a lot extra work

complexBenchBigFileReferencesEdits, complexBenchBigFileReferencesKeystrokes :: Benchmark
(complexBenchBigFileReferencesEdits, complexBenchBigFileReferencesKeystrokes) =
  withBothEditsAndKeystrokes complexBenchBigFileReferences
  where
  complexBenchBigFileReferences insertFuncName insert =
      benchLspSession ("one_big_file.mligo/" <> insertFuncName) projectWithOneBigFile $ do
        doc <- OpenedDoc <$> openLigoDoc "one_big_file.mligo"
        let rrFile = doc
            rrProject = projectWithOneBigFile
        refs1 <-
          requestReferences $
            ReferencesRequest -- a022
              { rrProject,
                rrFile,
                rrPos = (24, 15),
                rrExpectedAmount = 2
              }
        insert doc (1001, 1) "let q1 = 1234 + a023"
        refs2 <-
          requestReferences $
            ReferencesRequest -- a023
              { rrProject,
                rrFile,
                rrPos = (25, 15),
                rrExpectedAmount = 3
              }
        insert doc (1002, 1) "let q2 = 5678 + a024 + a024"
        refs3 <-
          requestReferences $
            ReferencesRequest -- a024
              { rrProject,
                rrFile,
                rrPos = (26, 15),
                rrExpectedAmount = 4
              }
        pure (refs1, refs2, refs3)
