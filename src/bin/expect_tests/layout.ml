open Cli_expect

let negative = "../../test/contracts/negative/"
let layout = negative ^ "layout.mligo"
let layout1 = negative ^ "layout1.mligo"

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; layout ];
  [%expect
    {|
    File "../../test/contracts/negative/layout.mligo", line 17, character 2 to line 23, character 6:
     16 | let main (p : unit) (s : storage) : return =
     17 |   let s : storage1 = {
            ^^^^^^^^^^^^^^^^^^^^
     18 |     tata = 0;
          ^^^^^^^^^^^^^
     19 |     toto = 1;
          ^^^^^^^^^^^^^
     20 |     titi = 2;
          ^^^^^^^^^^^^^
     21 |     tutu = 3;
          ^^^^^^^^^^^^^
     22 |   } in
          ^^^^^^
     23 |   [],s
          ^^^^^^
     24 |

    Invalid type(s)
    Cannot unify "storage1" with "storage" due to differing layouts "(({ name: tata }, { name: titi }), ({ name: toto }, { name: tutu }))" and "({ name: tata }, { name: toto }, { name: titi }, { name: tutu })". |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; layout1 ];
  [%expect
    {|
    File "../../test/contracts/negative/layout1.mligo", line 5, characters 7-40:
      4 | let main (_ : unit) (_ : r1) : operation list * r1 =
      5 |   ([], ({bar = "bar"; foo = "foo"} : r2))
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    Invalid type(s)
    Cannot unify "r2" with "r1" due to differing layouts "({ name: bar }, { name: foo })" and "({ name: foo }, { name: bar })". |}]
