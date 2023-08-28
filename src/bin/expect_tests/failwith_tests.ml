open Cli_expect

let contract basename = "../../test/contracts/" ^ basename

let%expect_test _ =
  run_ligo_good [ "run"; "dry-run"; contract "subtle_nontail_fail.mligo"; "()"; "()" ];
  [%expect{| failed with: "This contract always fails" |}]

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
