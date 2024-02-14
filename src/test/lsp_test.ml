open Lsp_test_requests

let _main =
  Printexc.record_backtrace true;
  Alcotest.run "lsp_server_tests" [ Completion.tests ]
