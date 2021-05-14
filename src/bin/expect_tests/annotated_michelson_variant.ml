open Cli_expect

let contract basename =
  "../../test/contracts/" ^ basename

(*COMB*)
let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "annotated_michelson_variant_comb.mligo" ; "main_comb_two" ] ;
  [%expect {|
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 21, characters 19-25:
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 24, characters 10-11:
             Warning: unused variable "j".
             Hint: replace it by "_j" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 23, characters 10-11:
             Warning: unused variable "i".
             Hint: replace it by "_i" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 28, characters 29-34:
             Warning: unused variable "store".
             Hint: replace it by "_store" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 28, characters 21-27:
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 32, characters 20-26:
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 38, characters 11-12:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 37, characters 11-12:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 34, characters 10-11:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 36, characters 12-13:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 35, characters 10-11:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.

             { parameter unit ;
               storage (or (int %anbfoo) (string %anabar)) ;
               code { CDR ;
                      IF_LEFT
                        { DROP ; PUSH string "foo" ; RIGHT int }
                        { DROP ; PUSH int 1 ; LEFT string } ;
                      NIL operation ;
                      PAIR } } |}];
  run_ligo_good [ "compile-contract" ; contract "annotated_michelson_variant_comb.mligo" ; "main_comb_three" ] ;
  [%expect {|
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 21, characters 19-25:
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 24, characters 10-11:
             Warning: unused variable "j".
             Hint: replace it by "_j" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 23, characters 10-11:
             Warning: unused variable "i".
             Hint: replace it by "_i" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 28, characters 29-34:
             Warning: unused variable "store".
             Hint: replace it by "_store" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 28, characters 21-27:
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 32, characters 20-26:
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 38, characters 11-12:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 37, characters 11-12:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 34, characters 10-11:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 36, characters 12-13:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 35, characters 10-11:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.

             { parameter unit ;
               storage (or (int %ana) (or (string %anb) (nat %anc))) ;
               code { DROP ; PUSH nat 1 ; RIGHT string ; RIGHT int ; NIL operation ; PAIR } } |}];
  run_ligo_good [ "compile-contract" ; contract "annotated_michelson_variant_comb.mligo" ; "main_comb_five" ] ;
  [%expect {|
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 21, characters 19-25:
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 24, characters 10-11:
             Warning: unused variable "j".
             Hint: replace it by "_j" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 23, characters 10-11:
             Warning: unused variable "i".
             Hint: replace it by "_i" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 28, characters 29-34:
             Warning: unused variable "store".
             Hint: replace it by "_store" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 28, characters 21-27:
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 32, characters 20-26:
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 38, characters 11-12:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 37, characters 11-12:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 34, characters 10-11:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 36, characters 12-13:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 35, characters 10-11:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.

             { parameter unit ;
               storage
                 (or (int %an_One)
                     (or (string %an_Two) (or (bool %an_Three) (or (nat %an_Four) (int %an_Five))))) ;
               code { CDR ;
                      IF_LEFT
                        { DROP ; PUSH int 1 ; RIGHT nat ; RIGHT bool ; RIGHT string ; RIGHT int }
                        { IF_LEFT
                            { DROP ; PUSH nat 2 ; LEFT int ; RIGHT bool ; RIGHT string ; RIGHT int }
                            { IF_LEFT
                                { DROP ; PUSH bool True ; LEFT (or nat int) ; RIGHT string ; RIGHT int }
                                { IF_LEFT
                                    { DROP ; PUSH string "lol" ; LEFT (or bool (or nat int)) ; RIGHT int }
                                    { DROP ; PUSH int 1 ; LEFT (or string (or bool (or nat int))) } } } } ;
                      NIL operation ;
                      PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "dry-run" ; contract "annotated_michelson_variant_comb.mligo" ; "main_comb_two" ; "()" ; "Foo(1)" ] ;
  [%expect{|
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 21, characters 19-25:
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 24, characters 10-11:
    Warning: unused variable "j".
    Hint: replace it by "_j" to prevent this warning.
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 23, characters 10-11:
    Warning: unused variable "i".
    Hint: replace it by "_i" to prevent this warning.
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 28, characters 29-34:
    Warning: unused variable "store".
    Hint: replace it by "_store" to prevent this warning.
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 28, characters 21-27:
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 32, characters 20-26:
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 38, characters 11-12:
    Warning: unused variable "a".
    Hint: replace it by "_a" to prevent this warning.
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 37, characters 11-12:
    Warning: unused variable "a".
    Hint: replace it by "_a" to prevent this warning.
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 34, characters 10-11:
    Warning: unused variable "a".
    Hint: replace it by "_a" to prevent this warning.
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 36, characters 12-13:
    Warning: unused variable "a".
    Hint: replace it by "_a" to prevent this warning.
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 35, characters 10-11:
    Warning: unused variable "a".
    Hint: replace it by "_a" to prevent this warning.

    ( LIST_EMPTY() , Bar("foo") ) |}];
  run_ligo_good [ "dry-run" ; contract "annotated_michelson_variant_comb.mligo" ; "main_comb_three" ; "()" ; "A(1)" ] ;
  [%expect{|
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 21, characters 19-25:
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 24, characters 10-11:
    Warning: unused variable "j".
    Hint: replace it by "_j" to prevent this warning.
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 23, characters 10-11:
    Warning: unused variable "i".
    Hint: replace it by "_i" to prevent this warning.
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 28, characters 29-34:
    Warning: unused variable "store".
    Hint: replace it by "_store" to prevent this warning.
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 28, characters 21-27:
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 32, characters 20-26:
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 38, characters 11-12:
    Warning: unused variable "a".
    Hint: replace it by "_a" to prevent this warning.
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 37, characters 11-12:
    Warning: unused variable "a".
    Hint: replace it by "_a" to prevent this warning.
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 34, characters 10-11:
    Warning: unused variable "a".
    Hint: replace it by "_a" to prevent this warning.
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 36, characters 12-13:
    Warning: unused variable "a".
    Hint: replace it by "_a" to prevent this warning.
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 35, characters 10-11:
    Warning: unused variable "a".
    Hint: replace it by "_a" to prevent this warning.

    ( LIST_EMPTY() , C(+1) ) |}];
  run_ligo_good [ "dry-run" ; contract "annotated_michelson_variant_comb.mligo" ; "main_comb_five" ; "()" ; "One(1)"] ;
  [%expect{|
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 21, characters 19-25:
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 24, characters 10-11:
    Warning: unused variable "j".
    Hint: replace it by "_j" to prevent this warning.
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 23, characters 10-11:
    Warning: unused variable "i".
    Hint: replace it by "_i" to prevent this warning.
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 28, characters 29-34:
    Warning: unused variable "store".
    Hint: replace it by "_store" to prevent this warning.
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 28, characters 21-27:
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 32, characters 20-26:
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 38, characters 11-12:
    Warning: unused variable "a".
    Hint: replace it by "_a" to prevent this warning.
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 37, characters 11-12:
    Warning: unused variable "a".
    Hint: replace it by "_a" to prevent this warning.
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 34, characters 10-11:
    Warning: unused variable "a".
    Hint: replace it by "_a" to prevent this warning.
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 36, characters 12-13:
    Warning: unused variable "a".
    Hint: replace it by "_a" to prevent this warning.
    File "../../test/contracts/annotated_michelson_variant_comb.mligo", line 35, characters 10-11:
    Warning: unused variable "a".
    Hint: replace it by "_a" to prevent this warning.

    ( LIST_EMPTY() , Five(1) ) |}]

(*TREE*)
let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "annotated_michelson_variant_tree.mligo" ; "main_comb_two" ] ;
  [%expect {|
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 21, characters 19-25:
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 24, characters 10-11:
             Warning: unused variable "j".
             Hint: replace it by "_j" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 23, characters 10-11:
             Warning: unused variable "i".
             Hint: replace it by "_i" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 28, characters 29-34:
             Warning: unused variable "store".
             Hint: replace it by "_store" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 28, characters 21-27:
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 32, characters 20-26:
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 38, characters 11-12:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 37, characters 11-12:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 34, characters 10-11:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 36, characters 12-13:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 35, characters 10-11:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.

             { parameter unit ;
               storage (or (string %anabar) (int %anbfoo)) ;
               code { CDR ;
                      IF_LEFT
                        { DROP ; PUSH int 1 ; RIGHT string }
                        { DROP ; PUSH string "foo" ; LEFT int } ;
                      NIL operation ;
                      PAIR } } |}];
  run_ligo_good [ "compile-contract" ; contract "annotated_michelson_variant_tree.mligo" ; "main_comb_three" ] ;
  [%expect {|
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 21, characters 19-25:
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 24, characters 10-11:
             Warning: unused variable "j".
             Hint: replace it by "_j" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 23, characters 10-11:
             Warning: unused variable "i".
             Hint: replace it by "_i" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 28, characters 29-34:
             Warning: unused variable "store".
             Hint: replace it by "_store" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 28, characters 21-27:
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 32, characters 20-26:
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 38, characters 11-12:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 37, characters 11-12:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 34, characters 10-11:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 36, characters 12-13:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 35, characters 10-11:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.

             { parameter unit ;
               storage (or (or (int %ana) (string %anb)) (nat %anc)) ;
               code { DROP ; PUSH nat 1 ; RIGHT (or int string) ; NIL operation ; PAIR } } |}];
  run_ligo_good [ "compile-contract" ; contract "annotated_michelson_variant_tree.mligo" ; "main_comb_five" ] ;
  [%expect {|
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 21, characters 19-25:
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 24, characters 10-11:
             Warning: unused variable "j".
             Hint: replace it by "_j" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 23, characters 10-11:
             Warning: unused variable "i".
             Hint: replace it by "_i" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 28, characters 29-34:
             Warning: unused variable "store".
             Hint: replace it by "_store" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 28, characters 21-27:
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 32, characters 20-26:
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 38, characters 11-12:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 37, characters 11-12:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 34, characters 10-11:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 36, characters 12-13:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 35, characters 10-11:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.

             { parameter unit ;
               storage
                 (or (or (or (int %an_Five) (nat %an_Four)) (or (int %an_One) (bool %an_Three)))
                     (string %an_Two)) ;
               code { CDR ;
                      IF_LEFT
                        { IF_LEFT
                            { IF_LEFT
                                { DROP ; PUSH int 1 ; LEFT bool ; RIGHT (or int nat) ; LEFT string }
                                { DROP ; PUSH string "lol" ; RIGHT (or (or int nat) (or int bool)) } }
                            { IF_LEFT
                                { DROP ; PUSH int 1 ; LEFT nat ; LEFT (or int bool) ; LEFT string }
                                { DROP ; PUSH bool True ; RIGHT int ; RIGHT (or int nat) ; LEFT string } } }
                        { DROP ; PUSH nat 2 ; RIGHT int ; LEFT (or int bool) ; LEFT string } ;
                      NIL operation ;
                      PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "dry-run" ; contract "annotated_michelson_variant_tree.mligo" ; "main_comb_two" ; "()" ; "Foo(1)" ] ;
  [%expect {|
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 21, characters 19-25:
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 24, characters 10-11:
             Warning: unused variable "j".
             Hint: replace it by "_j" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 23, characters 10-11:
             Warning: unused variable "i".
             Hint: replace it by "_i" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 28, characters 29-34:
             Warning: unused variable "store".
             Hint: replace it by "_store" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 28, characters 21-27:
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 32, characters 20-26:
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 38, characters 11-12:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 37, characters 11-12:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 34, characters 10-11:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 36, characters 12-13:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 35, characters 10-11:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.

             ( LIST_EMPTY() , Bar("foo") ) |}];
  run_ligo_good [ "dry-run" ; contract "annotated_michelson_variant_tree.mligo" ; "main_comb_three" ; "()" ; "A(2)" ] ;
  [%expect {|
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 21, characters 19-25:
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 24, characters 10-11:
             Warning: unused variable "j".
             Hint: replace it by "_j" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 23, characters 10-11:
             Warning: unused variable "i".
             Hint: replace it by "_i" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 28, characters 29-34:
             Warning: unused variable "store".
             Hint: replace it by "_store" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 28, characters 21-27:
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 32, characters 20-26:
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 38, characters 11-12:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 37, characters 11-12:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 34, characters 10-11:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 36, characters 12-13:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 35, characters 10-11:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.

             ( LIST_EMPTY() , C(+1) ) |}];
  run_ligo_good [ "dry-run" ; contract "annotated_michelson_variant_tree.mligo" ; "main_comb_five" ; "()" ; "One(1)"] ;
  [%expect {|
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 21, characters 19-25:
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 24, characters 10-11:
             Warning: unused variable "j".
             Hint: replace it by "_j" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 23, characters 10-11:
             Warning: unused variable "i".
             Hint: replace it by "_i" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 28, characters 29-34:
             Warning: unused variable "store".
             Hint: replace it by "_store" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 28, characters 21-27:
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 32, characters 20-26:
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 38, characters 11-12:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 37, characters 11-12:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 34, characters 10-11:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 36, characters 12-13:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.
             File "../../test/contracts/annotated_michelson_variant_tree.mligo", line 35, characters 10-11:
             Warning: unused variable "a".
             Hint: replace it by "_a" to prevent this warning.

             ( LIST_EMPTY() , Five(1) ) |}]