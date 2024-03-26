open Cli_expect

let file fname = "../../test/contracts/import_tests/" ^ fname

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "(Import.v + 1)"
    ; "--init-file"
    ; file "main.mligo"
    ];
  [%expect {| 2 |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "type"; "Import.t list"; "--init-file"; file "main.mligo" ];
  [%expect {| (list int) |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "parameter"; file "counter.mligo"; "(Increment 1)" ];
  [%expect {|
    (Right 1) |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "storage"; file "counter.mligo"; "10" ];
  [%expect {|
    10 |}]
