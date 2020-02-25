(* -*- compile-command: "cd .. ; dune runtest" -*- *)

open Test_helpers

let () =
  Printexc.record_backtrace true ;
  run_test @@ test_suite "LIGO" [
    Integration_tests.main ;
    Transpiler_tests.main ;
    Typer_tests.main ;
    Coase_tests.main ;
    Vote_tests.main ;
    Id_tests.main ;
    Multisig_tests.main ;
    Multisig_v2_tests.main ;
    Replaceable_id_tests.main ;
    Time_lock_tests.main ;
    Hash_lock_tests.main ;
    Time_lock_repeat_tests.main ;
  ] ;
  ()
