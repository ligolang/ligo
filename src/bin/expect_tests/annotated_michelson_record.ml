open Cli_expect

let contract basename = "../../test/contracts/" ^ basename

(*COMB*)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "contract"
    ; contract "annotated_michelson_record_comb.mligo"
    ; "-m"
    ; "Main_comb_two"
    ];
  [%expect
    {|
    { parameter unit ;
      storage (pair (int %anbfoo) (string %anabar)) ;
      code { CDR ; DUP ; CAR ; UPDATE 1 ; NIL operation ; PAIR } } |}];
  run_ligo_good
    [ "compile"
    ; "contract"
    ; contract "annotated_michelson_record_comb.mligo"
    ; "-m"
    ; "Main_comb_three"
    ];
  [%expect
    {|
    { parameter unit ;
      storage (pair (int %ana) (string %anb) (nat %anc)) ;
      code { DROP ;
             PUSH nat 1 ;
             PUSH string "" ;
             PUSH int 1 ;
             PAIR 3 ;
             NIL operation ;
             PAIR } } |}];
  run_ligo_good
    [ "compile"
    ; "contract"
    ; contract "annotated_michelson_record_comb.mligo"
    ; "-m"
    ; "Main_comb_five"
    ];
  [%expect
    {|
    { parameter unit ;
      storage
        (pair (int %an_One) (string %an_Two) (bool %an_Three) (nat %an_Four) (int %an_Five)) ;
      code { CDR ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "dry-run"
    ; contract "annotated_michelson_record_comb.mligo"
    ; "()"
    ; "{ foo = 2 ; bar = \"bar\" }"
    ; "-m"
    ; "Main_comb_two"
    ];
  [%expect {| ( LIST_EMPTY() , record[bar -> "bar" , foo -> 2] ) |}];
  run_ligo_good
    [ "run"
    ; "dry-run"
    ; contract "annotated_michelson_record_comb.mligo"
    ; "()"
    ; "{ a = 2 ; b = \"\" ; c = 1n }"
    ; "-m"
    ; "Main_comb_three"
    ];
  [%expect {| ( LIST_EMPTY() , record[a -> 1 , b -> "" , c -> +1] ) |}];
  run_ligo_good
    [ "run"
    ; "dry-run"
    ; contract "annotated_michelson_record_comb.mligo"
    ; "()"
    ; "{ one = 1 ; two = \"\" ; three = true ; four = 2n ; five = 1 }"
    ; "-m"
    ; "Main_comb_five"
    ];
  [%expect
    {|
    ( LIST_EMPTY() ,
      record[five -> 1 , four -> +2 , one -> 1 , three -> True(unit) , two -> ""] ) |}]

(*TREE*)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "contract"
    ; contract "annotated_michelson_record_tree.mligo"
    ; "-m"
    ; "Main_comb_two"
    ];
  [%expect
    {|
    { parameter unit ;
      storage (pair (string %anbar) (int %anfoo)) ;
      code { CDR ; DUP ; CDR ; UPDATE 2 ; NIL operation ; PAIR } } |}];
  run_ligo_good
    [ "compile"
    ; "contract"
    ; contract "annotated_michelson_record_tree.mligo"
    ; "-m"
    ; "Main_comb_three"
    ];
  [%expect
    {|
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
  run_ligo_good
    [ "compile"
    ; "contract"
    ; contract "annotated_michelson_record_tree.mligo"
    ; "-m"
    ; "Main_comb_five"
    ];
  [%expect
    {|
    { parameter unit ;
      storage
        (pair (pair (pair (int %an_Five) (nat %an_Four)) (int %an_One) (bool %an_Three))
              (string %an_Two)) ;
      code { CDR ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "dry-run"
    ; contract "annotated_michelson_record_tree.mligo"
    ; "()"
    ; "{ foo = 2 ; bar = \"bar\" }"
    ; "-m"
    ; "Main_comb_two"
    ];
  [%expect {| ( LIST_EMPTY() , record[bar -> "bar" , foo -> 2] ) |}];
  run_ligo_good
    [ "run"
    ; "dry-run"
    ; contract "annotated_michelson_record_tree.mligo"
    ; "()"
    ; "{ a = 2 ; b = \"\" ; c = 1n }"
    ; "-m"
    ; "Main_comb_three"
    ];
  [%expect {| ( LIST_EMPTY() , record[a -> 1 , b -> "" , c -> +1] ) |}];
  run_ligo_good
    [ "run"
    ; "dry-run"
    ; contract "annotated_michelson_record_tree.mligo"
    ; "()"
    ; "{ one = 1 ; two = \"\" ; three = true ; four = 2n ; five = 1 }"
    ; "-m"
    ; "Main_comb_five"
    ];
  [%expect
    {|
    ( LIST_EMPTY() ,
      record[five -> 1 , four -> +2 , one -> 1 , three -> True(unit) , two -> ""] ) |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "accesses "
    ; "--init-file"
    ; contract "annotated_michelson_record_comb.mligo"
    ];
  [%expect {| ( 1 , "" , True(unit) , +1 , 2 ) |}]
