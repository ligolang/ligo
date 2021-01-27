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
    y Typechecker_tests.main ;
    y Db_index_tests.main ;
    y Typer_tests.main ;
    y Integration_tests.main ;
    y Spilling_tests.main ;
    no Coase_tests.main ;
    no Vote_tests.main ;
    no Id_tests.main ;
    no Id_tests_p.main ;
    no Id_tests_r.main ;
    no Basic_multisig_tests.main;
    no Multisig_tests.main ;
    no Multisig_v2_tests.main ;
    no Replaceable_id_tests.main ;
    no Time_lock_tests.main ;
    no Hash_lock_tests.main ;
    no Time_lock_repeat_tests.main ;
    no Pledge_tests.main ;
    no Tzip12_tests.main ;
    no Positive_contract_tests.main ;
  ] ;
  ()
