open Cli_expect

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
    [ "run"; "interpret"; "test ()"; "--init-file"; test "nested_record_update.mligo" ];
  [%expect
    {|
      record[buz -> 5 ,
             l2 -> record[fiz -> 5 , l1 -> record[bar -> 5 , buz -> 0 , foo -> 5]]] |}]

(* TODO: uncomment the below test when tickets will be supported in
   nested_record_update *)
(* let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "contract"
    ; test "nested_record_update_ticket.mligo"
    ];
  [%expect
    {| |}] *)

(*
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
*)
