open Cli_expect

let contract basename = "../../test/contracts/" ^ basename

(* avoid pretty printing *)
let () = Ligo_unix.putenv ~key:"TERM" ~data:"dumb"

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "dry-run"
    ; contract "double_michelson_or.mligo"
    ; "unit"
    ; "(M_left (1) : storage)"
    ];
  [%expect
    {|
    File "../../test/contracts/double_michelson_or.mligo", line 8, characters 6-9:
      7 |   let foo = (M_right ("one") : storage) in
      8 |   let bar = (M_right 1 : foobar) in
                ^^^
      9 |   (([] : operation list), (foo: storage))
    :
    Warning: unused variable "bar".
    Hint: replace it by "_bar" to prevent this warning.

    File "../../test/contracts/double_michelson_or.mligo", line 6, characters 26-31:
      5 |
      6 | let main (action : unit) (store : storage) : return =
                                    ^^^^^
      7 |   let foo = (M_right ("one") : storage) in
    :
    Warning: unused variable "store".
    Hint: replace it by "_store" to prevent this warning.

    File "../../test/contracts/double_michelson_or.mligo", line 6, characters 10-16:
      5 |
      6 | let main (action : unit) (store : storage) : return =
                    ^^^^^^
      7 |   let foo = (M_right ("one") : storage) in
    :
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.

    ( LIST_EMPTY() , M_right("one") ) |}]
