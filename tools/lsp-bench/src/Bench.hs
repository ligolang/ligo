{-# OPTIONS_GHC -Wno-orphans #-}
module Bench (main) where

import Universum

import Criterion.Main

import Bench.Completion
import Bench.Complex
import Bench.Diagnostics
import Bench.Hovers
import Bench.References

-- Makefile contains instructions for launching benchmarks.
-- Keep in mind that `benchLspSession` takes about 250ms for an empty session.
-- Close VSCode to get more accurate results (more free RAM, less random CPU usage).
main :: IO ()
main = defaultMain
  $  bench_simple_hovers
  <> bench_sequence_hovers
  <> bench_simple_diagnostics
  <> bench_diagnostics_edits
  <> bench_simple_references
  <> bench_sequence_references
  <> bench_simple_completions
  <> bench_complex
