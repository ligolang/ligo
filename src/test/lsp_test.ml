open Lsp_test_requests

let _main =
  Printexc.record_backtrace true;
  Alcotest.run
    "lsp_server_tests"
    [ Completion.tests
    ; Cst_fold.tests
    ; Definition.tests
    ; Diagnostics.tests
    ; Document_link.tests
    ; Folding_range.tests
    ; Formatting.tests
    ; Hover.tests
    ; Prepare_rename.tests
    ; Range_formatting.tests
    ; References.tests
    ; On_doc.tests
    ; Semantic_highlight.tests
    ]
