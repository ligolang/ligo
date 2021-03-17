open Cli_expect


let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(test "record.ligo") ; "change_color_preference(acc, Green)"  ] ;
  [%expect{| record[id -> 1 , preferences -> record[color -> Green(unit) , other -> 1]] |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(test "record.mligo") ; "change_color_preference acc Green"  ] ;
  [%expect {|
    record[id -> 1 , preferences -> record[color -> Green(unit) , other -> 1]] |}]