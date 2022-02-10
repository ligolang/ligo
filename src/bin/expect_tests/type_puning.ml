open Cli_expect


let%expect_test _ =
  run_ligo_good ["print" ; "ast-imperative" ; (test "type_puning.ligo") ] ;
  [%expect{|
    type foot = int
    type rfoot = {foot -> foot } |}]