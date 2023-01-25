open Cli_expect

let contract basename = "../../test/contracts/" ^ basename

let%expect_test _ =
  run_ligo_good [ "run"; "dry-run"; contract "subtle_nontail_fail.mligo"; "()"; "()" ];
  [%expect
    {|
    File "../../test/contracts/subtle_nontail_fail.mligo", line 1, characters 10-12:
      1 | let main (ps : unit * unit) : operation list * unit =
      2 |   if true
    :
    Warning: unused variable "ps".
    Hint: replace it by "_ps" to prevent this warning.

    failed with: "This contract always fails" |}]

let%expect_test _ =
  run_ligo_good [ "run"; "interpret"; "assert(1=1)"; "--syntax"; "cameligo" ];
  [%expect {|
    unit |}]

let%expect_test _ =
  run_ligo_good [ "run"; "interpret"; "assert(1=2)"; "--syntax"; "cameligo" ];
  [%expect {|
    failed with: "failed assertion" |}]

let%expect_test _ =
  run_ligo_good [ "run"; "interpret"; "(failwith (1,2n) : unit)"; "--syntax"; "cameligo" ];
  [%expect {|
    failed with: (Pair 1 2) |}]
