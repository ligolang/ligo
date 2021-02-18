open Cli_expect

let contract basename =
  "../../test/contracts/" ^ basename

(*COMB*)
let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "annotated_michelson_variant_comb.mligo" ; "main_comb_two" ] ;
  [%expect {|
             { parameter unit ;
               storage (or (int %anbfoo) (string %anabar)) ;
               code { UNPAIR ;
                      DROP ;
                      IF_LEFT
                        { DROP ; PUSH string "foo" ; RIGHT int }
                        { DROP ; PUSH int 1 ; LEFT string } ;
                      NIL operation ;
                      PAIR } } |}];
  run_ligo_good [ "compile-contract" ; contract "annotated_michelson_variant_comb.mligo" ; "main_comb_three" ] ;
  [%expect {|
             { parameter unit ;
               storage (or (int %ana) (or (string %anb) (nat %anc))) ;
               code { DROP ; PUSH nat 1 ; RIGHT string ; RIGHT int ; NIL operation ; PAIR } } |}];
  run_ligo_good [ "compile-contract" ; contract "annotated_michelson_variant_comb.mligo" ; "main_comb_five" ] ;
  [%expect {|
             { parameter unit ;
               storage
                 (or (int %an_One)
                     (or (string %an_Two) (or (bool %an_Three) (or (nat %an_Four) (int %an_Five))))) ;
               code { UNPAIR ;
                      DROP ;
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
  [%expect{| ( LIST_EMPTY() , Bar("foo") ) |}];
  run_ligo_good [ "dry-run" ; contract "annotated_michelson_variant_comb.mligo" ; "main_comb_three" ; "()" ; "A(1)" ] ;
  [%expect{| ( LIST_EMPTY() , C(+1) ) |}];
  run_ligo_good [ "dry-run" ; contract "annotated_michelson_variant_comb.mligo" ; "main_comb_five" ; "()" ; "One(1)"] ;
  [%expect{| ( LIST_EMPTY() , Five(1) ) |}]

(*TREE*)
let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "annotated_michelson_variant_tree.mligo" ; "main_comb_two" ] ;
  [%expect {|
             { parameter unit ;
               storage (or (string %anabar) (int %anbfoo)) ;
               code { UNPAIR ;
                      DROP ;
                      IF_LEFT
                        { DROP ; PUSH int 1 ; RIGHT string }
                        { DROP ; PUSH string "foo" ; LEFT int } ;
                      NIL operation ;
                      PAIR } } |}];
  run_ligo_good [ "compile-contract" ; contract "annotated_michelson_variant_tree.mligo" ; "main_comb_three" ] ;
  [%expect {|
             { parameter unit ;
               storage (or (or (int %ana) (string %anb)) (nat %anc)) ;
               code { DROP ; PUSH nat 1 ; RIGHT (or int string) ; NIL operation ; PAIR } } |}];
  run_ligo_good [ "compile-contract" ; contract "annotated_michelson_variant_tree.mligo" ; "main_comb_five" ] ;
  [%expect {|
             { parameter unit ;
               storage
                 (or (or (or (int %an_Five) (nat %an_Four)) (or (int %an_One) (bool %an_Three)))
                     (string %an_Two)) ;
               code { UNPAIR ;
                      DROP ;
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
             ( LIST_EMPTY() , Bar("foo") ) |}];
  run_ligo_good [ "dry-run" ; contract "annotated_michelson_variant_tree.mligo" ; "main_comb_three" ; "()" ; "A(2)" ] ;
  [%expect {|
             ( LIST_EMPTY() , C(+1) ) |}];
  run_ligo_good [ "dry-run" ; contract "annotated_michelson_variant_tree.mligo" ; "main_comb_five" ; "()" ; "One(1)"] ;
  [%expect {|
             ( LIST_EMPTY() , Five(1) ) |}]