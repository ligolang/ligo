open Lsp_test_requests

let main =
  Printexc.record_backtrace true;
  Alcotest.run
    "lsp server tests"
    [ Definition.tests
    ; Prepare_rename.tests
    ; Diagnostics.tests
    ; Document_link.tests
    ; Folding_range.tests
    ]
