open Cli_expect

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; (test "sub_mutez_new.mligo") ; "--protocol" ; "ithaca" ] ;
  [%expect{|
    { parameter unit ;
      storage mutez ;
      code { CDR ;
             PUSH mutez 1000000 ;
             SWAP ;
             SUB_MUTEZ ;
             IF_NONE { PUSH string "option is None" ; FAILWITH } {} ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; (test "sub_mutez_new.ligo") ; "--protocol" ; "ithaca" ] ;
  [%expect{|
    { parameter unit ;
      storage mutez ;
      code { CDR ;
             PUSH mutez 1000000 ;
             SWAP ;
             SUB_MUTEZ ;
             IF_NONE { PUSH string "option is None" ; FAILWITH } {} ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; (test "sub_mutez_new.religo") ; "--protocol" ; "ithaca" ] ;
  [%expect{|
    { parameter unit ;
      storage mutez ;
      code { CDR ;
             PUSH mutez 1000000 ;
             SWAP ;
             SUB_MUTEZ ;
             IF_NONE { PUSH string "option is None" ; FAILWITH } {} ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; (test "sub_mutez_new.jsligo") ; "--protocol" ; "ithaca" ] ;
  [%expect{|
    { parameter unit ;
      storage mutez ;
      code { PUSH mutez 1000000 ;
             SWAP ;
             CDR ;
             SUB_MUTEZ ;
             IF_NONE { PUSH string "option is None" ; FAILWITH } {} ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; (test "sub_mutez_new.mligo") ; "--protocol" ; "ithaca" ] ;
  [%expect{|
    const sub = lambda (gen#3) return  match gen#3 with
                                        | ( store , delta ) ->
                                        SUB_MUTEZ(store , delta)
    const main = lambda (gen#4) return  match gen#4 with
                                         | ( gen#5 , store ) ->
                                         ( LIST_EMPTY() , UNOPT((sub)@(( store , 1000000mutez ))) ) |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; (test "sub_mutez_new.ligo") ; "--protocol" ; "ithaca" ] ;
  [%expect{|
    const sub = lambda (parameters#3) return  match parameters#3 with
                                               | ( store , delta ) ->
                                               SUB_MUTEZ(store , delta)
    const main = lambda (parameters#5) return  match parameters#5 with
                                                | ( _#4 , store ) ->
                                                ( LIST_EMPTY() , UNOPT((sub)@(( store , 1000000mutez ))) ) |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; (test "sub_mutez_new.religo") ; "--protocol" ; "ithaca" ] ;
  [%expect{|
    const sub = lambda (gen#3) return  match gen#3 with
                                        | ( store , delta ) ->
                                        SUB_MUTEZ(store , delta)
    const main = lambda (gen#4) return  match gen#4 with
                                         | ( gen#5 , store ) ->
                                         ( LIST_EMPTY() , UNOPT((sub)@(( store , 1000000mutez ))) ) |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; (test "sub_mutez_new.jsligo") ; "--protocol" ; "ithaca" ] ;
  [%expect{|
    const sub = lambda (gen#3) return let store = gen#3.0 in let delta = gen#3.1 in SUB_MUTEZ(store ,
    delta)[@private]
    const main = lambda (gen#5) return let _#4 = gen#5.0 in let store = gen#5.1 in
    ( LIST_EMPTY() , UNOPT((sub)@(( store , 1000000mutez ))) )[@private] |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-core" ; (test "sub_mutez_new.mligo") ] ;
  [%expect{|
    const sub : ( tez * tez ) -> option (tez) =
      lambda (gen#1 : ( tez * tez )) : option (tez) return  match gen#1 with
                                                             | (store,delta) ->
                                                             C_POLYMORPHIC_SUB
                                                             (store ,
                                                              delta)
    const main : ( unit * tez ) -> ( list (operation) * tez ) =
      lambda (gen#2 : ( unit * tez )) : ( list (operation) * tez ) return
       match gen#2 with
        | (gen#3,store) -> ( LIST_EMPTY() : list (operation) ,
                             UNOPT((sub)@(( store , 1000000mutez ))) ) |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-core" ; (test "sub_mutez_new.ligo") ] ;
  [%expect{|
    const sub : ( tez * tez ) -> option (tez) =
      lambda (parameters#1 : ( tez * tez )) : option (tez) return  match
                                                                    parameters#1 with
                                                                    | (store : tez,delta : tez) ->
                                                                    C_POLYMORPHIC_SUB
                                                                    (store ,
                                                                     delta)
    const main : ( unit * tez ) -> ( list (operation) * tez ) =
      lambda (parameters#3 : ( unit * tez )) : ( list (operation) * tez ) return
       match parameters#3 with
        | (_#2 : unit,store : tez) -> ( LIST_EMPTY() : list (operation) ,
                                        UNOPT((sub)@(( store , 1000000mutez ))) ) |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-core" ; (test "sub_mutez_new.religo") ] ;
  [%expect{|
    const sub =
      lambda (gen#1 : ( tez * tez )) : option (tez) return  match gen#1 with
                                                             | (store,delta) ->
                                                             C_POLYMORPHIC_SUB
                                                             (store ,
                                                              delta)
    const main =
      lambda (gen#2 : ( unit * tez )) : ( list (operation) * tez ) return
       match gen#2 with
        | (gen#3,store) -> ( LIST_EMPTY() : list (operation) ,
                             UNOPT((sub)@(( store , 1000000mutez ))) ) |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-core" ; (test "sub_mutez_new.jsligo") ] ;
  [%expect{|
    const sub[@var] =
      rec (sub:( tez * tez ) -> option (tez) => lambda (gen#1 : ( tez * tez )) : option (tez) return
      let store = gen#1.0 in
      let delta = gen#1.1 in C_POLYMORPHIC_SUB(store , delta) )[@private]
    const main[@var] =
      rec (main:( unit * tez ) -> ( list (operation) * tez ) => lambda (gen#3 :
      ( unit * tez )) : ( list (operation) * tez ) return let _#2 = gen#3.0 in
                                                          let store = gen#3.1 in
                                                          ( LIST_EMPTY() : list (operation) ,
                                                            UNOPT((sub)@(
                                                                  ( store ,
                                                                    1000000mutez ))) ) )[@private] |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; (test "sub_mutez_old.mligo") ; "--disable-michelson-typechecking" ] ;
  [%expect{|
    { parameter unit ;
      storage mutez ;
      code { CDR ; PUSH mutez 1000000 ; SWAP ; SUB ; NIL operation ; PAIR } }
  |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; (test "sub_mutez_old.ligo") ; "--disable-michelson-typechecking" ] ;
  [%expect{|
    { parameter unit ;
      storage mutez ;
      code { CDR ; PUSH mutez 1000000 ; SWAP ; SUB ; NIL operation ; PAIR } }
  |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; (test "sub_mutez_old.religo") ; "--disable-michelson-typechecking" ] ;
  [%expect{|
    { parameter unit ;
      storage mutez ;
      code { CDR ; PUSH mutez 1000000 ; SWAP ; SUB ; NIL operation ; PAIR } }
  |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; (test "sub_mutez_old.jsligo") ; "--disable-michelson-typechecking" ] ;
  [%expect{|
    { parameter unit ;
      storage mutez ;
      code { PUSH mutez 1000000 ; SWAP ; CDR ; SUB ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; (test "sub_mutez_old.mligo") ] ;
  [%expect{|
    const sub = lambda (gen#3) return  match gen#3 with
                                        | ( store , delta ) ->
                                        SUB(store , delta)
    const main = lambda (gen#4) return  match gen#4 with
                                         | ( gen#5 , store ) ->
                                         ( LIST_EMPTY() , (sub)@(( store , 1000000mutez )) )
  |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; (test "sub_mutez_old.ligo") ] ;
  [%expect{|
    const sub = lambda (parameters#3) return  match parameters#3 with
                                               | ( store , delta ) ->
                                               SUB(store , delta)
    const main = lambda (parameters#5) return  match parameters#5 with
                                                | ( _#4 , store ) ->
                                                ( LIST_EMPTY() , (sub)@(( store , 1000000mutez )) )
  |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; (test "sub_mutez_old.religo") ] ;
  [%expect{|
    const sub = lambda (gen#3) return  match gen#3 with
                                        | ( store , delta ) ->
                                        SUB(store , delta)
    const main = lambda (gen#4) return  match gen#4 with
                                         | ( gen#5 , store ) ->
                                         ( LIST_EMPTY() , (sub)@(( store , 1000000mutez )) )
  |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; (test "sub_mutez_old.jsligo") ] ;
  [%expect{|
    const sub = lambda (gen#3) return let store = gen#3.0 in let delta = gen#3.1 in SUB(store ,
    delta)[@private]
    const main = lambda (gen#5) return let _#4 = gen#5.0 in let store = gen#5.1 in
    ( LIST_EMPTY() , (sub)@(( store , 1000000mutez )) )[@private]
  |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-core" ; (test "sub_mutez_old.mligo") ] ;
  [%expect{|
    const sub : ( tez * tez ) -> tez =
      lambda (gen#1 : ( tez * tez )) : tez return  match gen#1 with
                                                    | (store,delta) -> C_POLYMORPHIC_SUB
                                                                       (store ,
                                                                        delta)
    const main : ( unit * tez ) -> ( list (operation) * tez ) =
      lambda (gen#2 : ( unit * tez )) : ( list (operation) * tez ) return
       match gen#2 with
        | (gen#3,store) -> ( LIST_EMPTY() : list (operation) ,
                             (sub)@(( store , 1000000mutez )) )
  |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-core" ; (test "sub_mutez_old.ligo") ] ;
  [%expect{|
    const sub : ( tez * tez ) -> tez =
      lambda (parameters#1 : ( tez * tez )) : tez return  match parameters#1 with
                                                           | (store : tez,delta : tez) ->
                                                           C_POLYMORPHIC_SUB
                                                           (store ,
                                                            delta)
    const main : ( unit * tez ) -> ( list (operation) * tez ) =
      lambda (parameters#3 : ( unit * tez )) : ( list (operation) * tez ) return
       match parameters#3 with
        | (_#2 : unit,store : tez) -> ( LIST_EMPTY() : list (operation) ,
                                        (sub)@(( store , 1000000mutez )) )
  |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-core" ; (test "sub_mutez_old.religo") ] ;
  [%expect{|
    const sub =
      lambda (gen#1 : ( tez * tez )) : tez return  match gen#1 with
                                                    | (store,delta) -> C_POLYMORPHIC_SUB
                                                                       (store ,
                                                                        delta)
    const main =
      lambda (gen#2 : ( unit * tez )) : ( list (operation) * tez ) return
       match gen#2 with
        | (gen#3,store) -> ( LIST_EMPTY() : list (operation) ,
                             (sub)@(( store , 1000000mutez )) )
 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-core" ; (test "sub_mutez_old.jsligo") ] ;
  [%expect{|
    const sub[@var] =
      rec (sub:( tez * tez ) -> tez => lambda (gen#1 : ( tez * tez )) : tez return
      let store = gen#1.0 in
      let delta = gen#1.1 in C_POLYMORPHIC_SUB(store , delta) )[@private]
    const main[@var] =
      rec (main:( unit * tez ) -> ( list (operation) * tez ) => lambda (gen#3 :
      ( unit * tez )) : ( list (operation) * tez ) return let _#2 = gen#3.0 in
                                                          let store = gen#3.1 in
                                                          ( LIST_EMPTY() : list (operation) ,
                                                            (sub)@(( store ,
                                                                     1000000mutez )) ) )[@private]
  |}]