open Cli_expect

let bad_contract basename = "../../test/contracts/negative/" ^ basename

(* avoid pretty printing *)

let () = Ligo_unix.putenv ~key:"TERM" ~data:"dumb"

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "bad_michelson_insertion_1.mligo" ];
  [%expect
    {test|
File "../../test/contracts/negative/bad_michelson_insertion_1.mligo", line 4, characters 29-72:
  3 | let main (p : nat) (s : nat) : operation list * nat =
  4 |   let f : nat * nat -> nat = [%Michelson ({| ADD |} : nat * nat -> nat)]
                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  5 |   in [], f (p, s)

Raw Michelson must be seq (with curly braces {}), got: ADD. |test}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "bad_michelson_insertion_2.mligo" ];
  [%expect
    {test|
File "../../test/contracts/negative/bad_michelson_insertion_2.mligo", line 5, characters 12-16:
  4 |   let f : nat -> nat -> nat = [%Michelson ({| ADD |} : nat -> nat -> nat)]
  5 |   in [], f (p, s)
                  ^^^^

Invalid type(s)
Cannot unify "( nat * nat )" with "nat". |test}]
