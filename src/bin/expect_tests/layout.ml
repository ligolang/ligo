open Cli_expect

let negative = "../../test/contracts/negative/"
let layout = negative ^ "layout.mligo"
let layout1 = negative ^ "layout1.mligo"

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; layout ];
  [%expect
    {|
    File "../../test/contracts/negative/layout.mligo", line 23, characters 5-6:
     22 |   } in
     23 |   [],s
               ^
     24 |

    This expression has type "storage1", but an expression was expected of type
    "storage".
    Type "storage1" is not compatible with type "storage" due to differing layouts "(({ name: tata }, { name: titi }), ({ name: toto }, { name: tutu }))" and "({ name: tata }, { name: toto }, { name: titi }, { name: tutu })". |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; layout1 ];
  [%expect
    {|
    File "../../test/contracts/negative/layout1.mligo", line 5, characters 7-40:
      4 | let main (_ : unit) (_ : r1) : operation list * r1 =
      5 |   ([], ({bar = "bar"; foo = "foo"} : r2))
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    This expression has type "r2", but an expression was expected of type "r1".
    Type "r2" is not compatible with type "r1" due to differing layouts "({ name: bar }, { name: foo })" and "({ name: foo }, { name: bar })". |}]
