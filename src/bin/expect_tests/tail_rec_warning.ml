open Cli_expect

let contract = test

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "unused_recursion.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage (pair (int %foo) (int %bar)) ;
      code { CDR ;
             PUSH int 0 ;
             PUSH int 1 ;
             PUSH int 2 ;
             ADD ;
             ADD ;
             UPDATE 2 ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; contract "unused_recursion.mligo"; "--warn-unused-rec" ];
  [%expect
    {|
    File "../../test/contracts/unused_recursion.mligo", line 12, characters 12-16:
     11 |
     12 |     let rec toto : int -> int =
                      ^^^^
     13 |       fun (toto : int) : int -> let number = toto in
    :
    Warning: unused recursion .
    Hint: remove recursion from the function "toto" to prevent this warning.

    File "../../test/contracts/unused_recursion.mligo", line 17, characters 12-15:
     16 |
     17 |     let rec foo : (int -> int) -> int =
                      ^^^
     18 |       fun (foo : (int -> int)) -> let foo = foo 0 in
    :
    Warning: unused recursion .
    Hint: remove recursion from the function "foo" to prevent this warning.

    File "../../test/contracts/unused_recursion.mligo", line 22, characters 12-15:
     21 |
     22 |     let rec bar : int -> t =
                      ^^^
     23 |       fun (x : int) -> let bar = x in
    :
    Warning: unused recursion .
    Hint: remove recursion from the function "bar" to prevent this warning.

    { parameter unit ;
      storage (pair (int %foo) (int %bar)) ;
      code { CDR ;
             PUSH int 0 ;
             PUSH int 1 ;
             PUSH int 2 ;
             ADD ;
             ADD ;
             UPDATE 2 ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "unused_recursion.jsligo" ];
  [%expect
    {|
    { parameter unit ;
      storage (pair (int %foo) (int %bar)) ;
      code { CDR ;
             PUSH int 0 ;
             PUSH int 1 ;
             PUSH int 2 ;
             ADD ;
             ADD ;
             UPDATE 2 ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; contract "unused_recursion.jsligo"; "--warn-unused-rec" ];
  [%expect
    {|
    { parameter unit ;
      storage (pair (int %foo) (int %bar)) ;
      code { CDR ;
             PUSH int 0 ;
             PUSH int 1 ;
             PUSH int 2 ;
             ADD ;
             ADD ;
             UPDATE 2 ;
             NIL operation ;
             PAIR } } |}]
