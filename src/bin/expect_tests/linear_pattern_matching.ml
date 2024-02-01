open Cli_expect

let%expect_test _ =
  run_ligo_bad
    [ "run"; "interpret"; "yy"; "--init-file"; bad_test "linear_pattern_matching.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/linear_pattern_matching.mligo", line 4, characters 34-35:
      3 | let yy : string = match { a = 1 ; b = 2n ; c = "33" } with
      4 |   | { a = a ;  b = b ; c = c } -> a
                                            ^

    This expression has type "int", but an expression was expected of type
    "string".
    Type "int" is not compatible with type "string". |}];
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "( (match (1,2n,\"3\") with | (a,b,c) -> a) : int )"
    ; "--syntax"
    ; "cameligo"
    ];
  [%expect {|
    1 |}];
  run_ligo_good
    [ "run"; "interpret"; "match (1,2) with | (a,b) -> a"; "--syntax"; "cameligo" ];
  [%expect {|
    1 |}]
