open Cli_expect

let contract basename =
  "../../test/contracts/" ^ basename

(*COMB*)
let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "annotated_michelson_record_comb.mligo" ; "main_comb_two" ] ;
  [%expect {|
             { parameter unit ;
               storage (pair (int %anbfoo) (string %anabar)) ;
               code { CDR ; DUP ; CAR ; SWAP ; CDR ; SWAP ; PAIR ; NIL operation ; PAIR } } |}];
  run_ligo_good [ "compile-contract" ; contract "annotated_michelson_record_comb.mligo" ; "main_comb_three" ] ;
  [%expect {|
             { parameter unit ;
               storage (pair (int %ana) (pair (string %anb) (nat %anc))) ;
               code { DROP ;
                      PUSH nat 1 ;
                      PUSH string "" ;
                      PAIR ;
                      PUSH int 1 ;
                      PAIR ;
                      NIL operation ;
                      PAIR } } |}];
  run_ligo_good [ "compile-contract" ; contract "annotated_michelson_record_comb.mligo" ; "main_comb_five" ] ;
  [%expect {|
             { parameter unit ;
               storage
                 (pair (int %an_One)
                       (pair (string %an_Two) (pair (bool %an_Three) (pair (nat %an_Four) (int %an_Five))))) ;
               code { CDR ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "dry-run" ; contract "annotated_michelson_record_comb.mligo" ; "main_comb_two" ; "()" ; "{ foo = 2 ; bar = \"bar\" }" ] ;
  [%expect {|
             ( LIST_EMPTY() , record[bar -> "bar" , foo -> 2] ) |}];
  run_ligo_good [ "dry-run" ; contract "annotated_michelson_record_comb.mligo" ; "main_comb_three" ; "()" ; "{ a = 2 ; b = \"\" ; c = 1n }" ] ;
  [%expect {|
             ( LIST_EMPTY() , record[a -> 1 , b -> "" , c -> +1] ) |}];
  run_ligo_good [ "dry-run" ; contract "annotated_michelson_record_comb.mligo" ; "main_comb_five" ; "()" ; "{ one = 1 ; two = \"\" ; three = true ; four = 2n ; five = 1 }"] ;
  [%expect {|
             ( LIST_EMPTY() ,
               record[five -> 1 , four -> +2 , one -> 1 , three -> true(unit) , two -> ""] ) |}]

(*TREE*)
let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "annotated_michelson_record_tree.mligo" ; "main_comb_two" ] ;
  [%expect {|
             { parameter unit ;
               storage (pair (string %anbar) (int %anfoo)) ;
               code { CDR ; DUP ; CDR ; SWAP ; CAR ; PAIR ; NIL operation ; PAIR } } |}];
  run_ligo_good [ "compile-contract" ; contract "annotated_michelson_record_tree.mligo" ; "main_comb_three" ] ;
  [%expect {|
             { parameter unit ;
               storage (pair (pair (int %ana) (string %anb)) (nat %anc)) ;
               code { DROP ;
                      PUSH nat 1 ;
                      PUSH string "" ;
                      PUSH int 1 ;
                      PAIR ;
                      PAIR ;
                      NIL operation ;
                      PAIR } } |}];
  run_ligo_good [ "compile-contract" ; contract "annotated_michelson_record_tree.mligo" ; "main_comb_five" ] ;
  [%expect {|
             { parameter unit ;
               storage
                 (pair (pair (pair (int %an_Five) (nat %an_Four)) (pair (int %an_One) (bool %an_Three)))
                       (string %an_Two)) ;
               code { CDR ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "dry-run" ; contract "annotated_michelson_record_tree.mligo" ; "main_comb_two" ; "()" ; "{ foo = 2 ; bar = \"bar\" }" ] ;
  [%expect {|
             ( LIST_EMPTY() , record[bar -> "bar" , foo -> 2] ) |}];
  run_ligo_good [ "dry-run" ; contract "annotated_michelson_record_tree.mligo" ; "main_comb_three" ; "()" ; "{ a = 2 ; b = \"\" ; c = 1n }" ] ;
  [%expect {|
             ( LIST_EMPTY() , record[a -> 1 , b -> "" , c -> +1] ) |}];
  run_ligo_good [ "dry-run" ; contract "annotated_michelson_record_tree.mligo" ; "main_comb_five" ; "()" ; "{ one = 1 ; two = \"\" ; three = true ; four = 2n ; five = 1 }"] ;
  [%expect {|
             ( LIST_EMPTY() ,
               record[five -> 1 , four -> +2 , one -> 1 , three -> true(unit) , two -> ""] ) |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(contract "annotated_michelson_record_comb.mligo") ; "accesses " ] ;
  [%expect {|
             ( 1 , "" , true(unit) , +1 , 2 ) |}]