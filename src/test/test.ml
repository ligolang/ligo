(* -*- compile-command: "cd .. ; dune runtest" -*- *)

open Test_helpers

let test enabled_for_typer_not_currently_in_use name f = enabled_for_typer_not_currently_in_use, test name f
let no test_suite = false, test_suite
let y test_suite = true, test_suite

let () =
  Printexc.record_backtrace true ;
    run_test @@ test_suite "LIGO"
    @@ (fun lst -> List.map snd @@ match typer_switch () with Ast_typed.New -> List.filter fst lst | _ -> lst) @@ [
    y Vendors.main ;
    y Heuristic_tc_fundep_tests.main ;
    y Heuristic_break_ctor_tests.main ;
    y Heuristic_specialize1_tests.main ;
    y Heuristic_access_label_tests.main ;
    y Typechecker_tests.main ;
    y Db_index_tests.main ;
    y Typer_tests.main ;
    y Integration_tests.main ;
    y Spilling_tests.main ;
    y Coase_tests.main ;
    y Vote_tests.main ;
    y Id_tests.main ;
    y Id_tests_p.main ;
    y Id_tests_r.main ;
    y Basic_multisig_tests.main;
    y Multisig_tests.main ;
    y Multisig_v2_tests.main ;
    y Replaceable_id_tests.main ;
    y Time_lock_tests.main ;
    y Hash_lock_tests.main ;
    y Time_lock_repeat_tests.main ;
    y Pledge_tests.main ;
    y Tzip12_tests.main ;
    y Positive_contract_tests.main ;
  ] ;
  ()
