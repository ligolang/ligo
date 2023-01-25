open Cli_expect

let%expect_test _ =
  run_ligo_good [ "compile"; "expression"; "cameligo"; " -100"; "--without-run" ];
  [%expect {| { PUSH int -100 } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "expression"; "cameligo"; "(fun () -> -1 : unit -> int)" ];
  [%expect {|
    { DROP ; PUSH int -1 } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "expression"; "cameligo"; " -100n"; "--without-run" ];
  [%expect {| { PUSH int -100 } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "expression"; "cameligo"; "(fun () -> -1n : unit -> int)" ];
  [%expect {|
    { DROP ; PUSH int -1 } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "expression"; "cameligo"; " -(int 100n)"; "--without-run" ];
  [%expect {| { PUSH int -100 } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "expression"; "cameligo"; "(fun () -> -(int 1n) : unit -> int)" ];
  [%expect {|
    { DROP ; PUSH int -1 } |}]
