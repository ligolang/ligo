{-# OPTIONS_GHC -Wno-orphans #-}
import Criterion.Main

import Bench.Complex
import Bench.Hovers
import Bench.References


-- | Before running @stack bench@ you need to pull git submodules.
-- @make bench@ handles this.
-- To run only specific benchmarks, you can use things like
-- @make bench filter="Complex"@,
-- @make bench filter="\\\"Complex benchmarks, Standard/one_big_file references keystrokes\\\""@
-- or @stack bench --ba "-v2 --match pattern  \"Complex\" "@.
-- Keep in mind that `benchLspSession` takes about 250ms for an empty session.
-- Other operations of lsp-test have some cost too, not sure if there is a way to eliminate it.
-- Anyway, comparing Fallback and Standard is fair.
-- Close VSCode to get more accurate results (more free RAM, less random CPU usage).
main :: IO ()
main = defaultMain
  $  bench_simple_hovers
  <> bench_sequence_hovers
  <> bench_simple_references
  <> bench_sequence_references
  <> bench_complex
