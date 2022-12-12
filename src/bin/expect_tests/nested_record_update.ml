open Cli_expect

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "change_color_preference(acc, Green)"
    ; "--init-file"
    ; test "record.ligo"
    ];
  [%expect
    {| record[id -> 1 , preferences -> record[color -> Green(unit) , other -> 1]] |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "change_color_preference acc Green"
    ; "--init-file"
    ; test "record.mligo"
    ];
  [%expect
    {|
    record[id -> 1 , preferences -> record[color -> Green(unit) , other -> 1]] |}]

let%expect_test _ =
  run_ligo_good
    [ "run"; "interpret"; "lhs_expr_access1"; "--init-file"; test "record.ligo" ];
  [%expect {|
    2048 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"; "interpret"; "lhs_expr_access2"; "--init-file"; test "record.ligo" ];
  [%expect {|
    Green(unit) |}]

let%expect_test _ =
  run_ligo_good
    [ "run"; "interpret"; "lhs_expr_fupdate"; "--init-file"; test "record.ligo" ];
  [%expect {|
    record[color -> Green(unit) , other -> 2] |}]
