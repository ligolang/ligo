(* -*- compile-command: "cd .. ; dune runtest" -*- *)

open Test_helpers

let () =
  Printexc.record_backtrace true;
  run_test
  @@ test_suite
       "LIGO (former print)"
       [

       (*   Ast_production.typed_prod *)
       (* ; Ast_production.core_prod *)
       (* ; Ast_production.agg_prod *)
       (* ; Ast_production.mini_c_prod *)
       (* ; Integration_tests.main *)
       (* ; Spilling_tests.main *)
       (* ; Ligo_init_tests.main *)
       (* ; Analytics_tests.main *)
        Ligo_install_tests.main
       ];
  ()
