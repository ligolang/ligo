open Lsp_test_requests

let _main =
  Printexc.record_backtrace true;
  Ligo_unix.putenv ~key:"LIGO_GET_SCOPE_USE_NEW_IMP" ~data:"";
  Alcotest.run
    "lsp_server_tests"
    [ Definition.tests
    ; Prepare_rename.tests
    ; Diagnostics.tests
    ; Document_link.tests
    ; Folding_range.tests
    ; Formatting.tests
    ];
  Ligo_unix.putenv ~key:"LIGO_GET_SCOPE_USE_NEW_IMP" ~data:""
