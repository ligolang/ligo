open Cli_expect

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "x"
    ; "--init-file"
    ; test "preprocessor_define.mligo"
    ; "-D"
    ; "FOO"
    ];
  [%expect {|
    1 |}]

let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "x"
    ; "--init-file"
    ; test "preprocessor_define.mligo"
    ];
  [%expect {|
    Variable "x" not found. |}]
