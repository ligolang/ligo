open Cli_expect

let contract basename = "../../test/contracts/signature/" ^ basename

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "BI.f 42n"
    ; "--init-file"
    ; contract "simple.mligo"
    ];
  [%expect {|
    42 |}]


let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "Bar.Foo.x"
    ; "--init-file"
    ; contract "spath.mligo"
    ];
  [%expect {|
    42 |}]


let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; "X.x"
    ; "--init-file"
    ; contract "interface.jsligo"
    ];
  [%expect {|
    42 |}]
