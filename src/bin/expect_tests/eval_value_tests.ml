open Cli_expect

let%expect_test _ =
  run_ligo_good [ "evaluate-value" ; "../../test/contracts/evaluation_tests.ligo" ; "a" ] ;
  [%expect {|
    {foo = +0 , bar = "bar"} |} ];

  run_ligo_good [ "evaluate-value" ; "../../test/contracts/evaluation_tests.ligo" ; "b" ] ;
  [%expect {|
    2 |} ]