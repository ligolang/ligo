open Cli_expect

let%expect_test _ =
  run_ligo_good [ "compile"; "expression"; "cameligo"; "4n land 4n" ];
  [%expect {|
    4 |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "expression"; "cameligo"; "4n land 0n" ];
  [%expect {|
    0 |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "expression"; "cameligo"; "7 land 4n" ];
  [%expect {|
    4 |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "expression"; "cameligo"; "4n lor 4n" ];
  [%expect {|
    4 |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "expression"; "cameligo"; "4n lor 0n" ];
  [%expect {|
    4 |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "expression"; "cameligo"; "7n lor 4n" ];
  [%expect {|
    7 |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "expression"; "cameligo"; "4n lxor 4n" ];
  [%expect {|
    0 |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "expression"; "cameligo"; "4n lxor 0n" ];
  [%expect {|
    4 |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "expression"; "cameligo"; "7n lxor 4n" ];
  [%expect {|
    3 |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "expression"; "cameligo"; "4n lsl 0n" ];
  [%expect {|
    4 |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "expression"; "cameligo"; "7n lsl 1n" ];
  [%expect {|
    14 |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "expression"; "cameligo"; "7n lsl 2n" ];
  [%expect {|
    28 |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "expression"; "cameligo"; "4n lsr 0n" ];
  [%expect {|
    4 |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "expression"; "cameligo"; "14n lsr 1n" ];
  [%expect {|
    7 |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "expression"; "cameligo"; "14n lsr 2n" ];
  [%expect {|
    3 |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "expression"; "cameligo"; "14 land 2" ];
  [%expect
    {|
    This expression has type "int", but an expression was expected of type "nat".
    Type "int" is not compatible with type "nat". |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "expression"; "cameligo"; "14n lor 2" ];
  [%expect
    {|
    This expression has type "int", but an expression was expected of type "nat".
    Type "int" is not compatible with type "nat". |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "expression"; "cameligo"; "14 lor 2n" ];
  [%expect
    {|
    This expression has type "int", but an expression was expected of type "nat".
    Type "int" is not compatible with type "nat". |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "expression"; "cameligo"; "14n lxor 2" ];
  [%expect
    {|
    This expression has type "int", but an expression was expected of type "nat".
    Type "int" is not compatible with type "nat". |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "expression"; "cameligo"; "14 lxor 2n" ];
  [%expect
    {|
    This expression has type "int", but an expression was expected of type "nat".
    Type "int" is not compatible with type "nat". |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "expression"; "cameligo"; "4 lsr 0n" ];
  [%expect
    {|
    This expression has type "int", but an expression was expected of type "nat".
    Type "int" is not compatible with type "nat". |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "expression"; "cameligo"; "14n lsr 1" ];
  [%expect
    {|
    This expression has type "int", but an expression was expected of type "nat".
    Type "int" is not compatible with type "nat". |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "expression"; "cameligo"; "14 lsl 2n" ];
  [%expect
    {|
    This expression has type "int", but an expression was expected of type "nat".
    Type "int" is not compatible with type "nat". |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "expression"; "cameligo"; "14n lsl 2" ];
  [%expect
    {|
    This expression has type "int", but an expression was expected of type "nat".
    Type "int" is not compatible with type "nat". |}]
