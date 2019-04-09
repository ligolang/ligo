(* -*- compile-command: "cd .. ; dune runtest" -*- *)

let () =
  (* Printexc.record_backtrace true ; *)
  Alcotest.run "LIGO" [
    Multifix_tests.main ;
    Integration_tests.main ;
    Compiler_tests.main ;
    Transpiler_tests.main ;
    Typer_tests.main ;
    Heap_tests.main ;
  ] ;
  ()
