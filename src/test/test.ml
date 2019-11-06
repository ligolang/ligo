(* -*- compile-command: "cd .. ; dune runtest" -*- *)

open Test_helpers

let () =
  Printexc.record_backtrace true ;
  run_test @@ test_suite "LIGO" [
    Integration_tests.main ;
    Compiler_tests.main ;
    Transpiler_tests.main ;
    Typer_tests.main ;
    Heap_tests.main ;
    Coase_tests.main ;
    Vote_tests.main ;
    Bin_tests.main ;
  ] ;
  ()
