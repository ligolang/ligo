open Lsp_test_requests

let _main =
  Printexc.record_backtrace true;
  Alcotest.run
    "lsp_server_tests"
    [ Definition.tests
    ; Prepare_rename.tests
    ; Diagnostics.tests
    ; Document_link.tests
    ; Folding_range.tests
    ; Formatting.tests
    ; Range_formatting.tests
    ; References.tests
    ; Cst_fold.tests
    ]
