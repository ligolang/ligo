open Lsp_test_requests

let main =
  Printexc.record_backtrace true;
  Alcotest.run
    "lsp server tests"
    [ "definition", List.map ~f:Definition.get_definition_test Definition.test_cases ]
