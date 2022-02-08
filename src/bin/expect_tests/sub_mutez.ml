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
  [%expect{xxx|
    const sub =
      lambda (gen#7) return  match gen#7 with
                              | ( store , delta ) ->
                              SUB_MUTEZ(store , delta)
    const main =
      lambda (gen#11) return  match gen#11 with
                               | ( _#12 , store ) ->
                               ( LIST_EMPTY() , (Option.unopt@{tez})@((sub)@(( store , 1000000mutez ))) ) |xxx}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; (test "sub_mutez_new.ligo") ; "--protocol" ; "ithaca" ] ;
  [%expect{xxx|
    const sub =
      lambda (parameters#7) return  match parameters#7 with
                                     | ( store , delta ) ->
                                     SUB_MUTEZ(store , delta)
    const main =
      lambda (parameters#11) return  match parameters#11 with
                                      | ( _#12 , store ) ->
                                      ( LIST_EMPTY() , (Option.unopt@{tez})@((sub)@(( store , 1000000mutez ))) ) |xxx}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; (test "sub_mutez_new.religo") ; "--protocol" ; "ithaca" ] ;
  [%expect{xxx|
    const sub =
      lambda (gen#7) return  match gen#7 with
                              | ( store , delta ) ->
                              SUB_MUTEZ(store , delta)
    const main =
      lambda (gen#11) return  match gen#11 with
                               | ( _#12 , store ) ->
                               ( LIST_EMPTY() , (Option.unopt@{tez})@((sub)@(( store , 1000000mutez ))) ) |xxx}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; (test "sub_mutez_new.jsligo") ; "--protocol" ; "ithaca" ] ;
  [%expect{xxx|
    const sub =
      lambda (gen#6) return let store = gen#6.0 in let delta = gen#6.1 in SUB_MUTEZ(store ,
      delta)[@private]
    const main =
      lambda (gen#11) return let _#12 = gen#11.0 in let store = gen#11.1 in
      ( LIST_EMPTY() , (Option.unopt@{tez})@((sub)@(( store , 1000000mutez ))) )[@private] |xxx}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-core" ; (test "sub_mutez_new.mligo") ] ;
  [%expect{|
const sub : ( tez * tez ) -> option (tez) =
  lambda (gen#7 : ( tez * tez )) : option (tez) return  match gen#7 with
                                                         | (store,delta) ->
                                                         C_POLYMORPHIC_SUB
                                                         (store ,
                                                          delta)
const main : ( unit * tez ) -> ( list (operation) * tez ) =
  lambda (gen#11 : ( unit * tez )) : ( list (operation) * tez ) return
   match gen#11 with
    | (_#12,store) -> ( LIST_EMPTY() : list (operation) ,
                        (Option.unopt)@((sub)@(( store , 1000000mutez ))) ) |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-core" ; (test "sub_mutez_new.ligo") ] ;
  [%expect{|
const sub : ( tez * tez ) -> option (tez) =
  lambda (parameters#7 : ( tez * tez )) : option (tez) return  match
                                                                parameters#7 with
                                                                | (store : tez,delta : tez) ->
                                                                C_POLYMORPHIC_SUB
                                                                (store ,
                                                                 delta)
const main : ( unit * tez ) -> ( list (operation) * tez ) =
  lambda (parameters#11 : ( unit * tez )) : ( list (operation) * tez ) return
   match parameters#11 with
    | (_#12 : unit,store : tez) -> ( LIST_EMPTY() : list (operation) ,
                                     (Option.unopt)@((sub)@(( store ,
                                                              1000000mutez ))) ) |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-core" ; (test "sub_mutez_new.religo") ] ;
  [%expect{|
const sub =
  lambda (gen#7 : ( tez * tez )) : option (tez) return  match gen#7 with
                                                         | (store,delta) ->
                                                         C_POLYMORPHIC_SUB
                                                         (store ,
                                                          delta)
const main =
  lambda (gen#11 : ( unit * tez )) : ( list (operation) * tez ) return
   match gen#11 with
    | (_#12,store) -> ( LIST_EMPTY() : list (operation) ,
                        (Option.unopt)@((sub)@(( store , 1000000mutez ))) ) |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-core" ; (test "sub_mutez_new.jsligo") ] ;
  [%expect{|
    const sub[@var] =
      rec (sub:( tez * tez ) -> option (tez) => lambda (gen#6 : ( tez * tez )) : option (tez) return
      let store = gen#6.0 in
      let delta = gen#6.1 in C_POLYMORPHIC_SUB(store , delta) )[@private]
    const main[@var] =
      rec (main:( unit * tez ) -> ( list (operation) * tez ) => lambda (gen#11 :
      ( unit * tez )) : ( list (operation) * tez ) return let _#12 = gen#11.0 in
                                                          let store = gen#11.1 in
                                                          ( LIST_EMPTY() : list (operation) ,
                                                            (Option.unopt)@(
                                                            (sub)@(( store ,
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
  [%expect{xxx|
const sub =
  lambda (gen#7) return  match gen#7 with
                          | ( store , delta ) ->
                          SUB(store , delta)
const main =
  lambda (gen#11) return  match gen#11 with
                           | ( _#12 , store ) ->
                           ( LIST_EMPTY() , (sub)@(( store , 1000000mutez )) )
  |xxx}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; (test "sub_mutez_old.ligo") ] ;
  [%expect{xxx|
    const sub =
      lambda (parameters#7) return  match parameters#7 with
                                     | ( store , delta ) ->
                                     SUB(store , delta)
    const main =
      lambda (parameters#11) return  match parameters#11 with
                                      | ( _#12 , store ) ->
                                      ( LIST_EMPTY() , (sub)@(( store , 1000000mutez )) )
  |xxx}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; (test "sub_mutez_old.religo") ] ;
  [%expect{xxx|
    const sub =
      lambda (gen#7) return  match gen#7 with
                              | ( store , delta ) ->
                              SUB(store , delta)
    const main =
      lambda (gen#11) return  match gen#11 with
                               | ( _#12 , store ) ->
                               ( LIST_EMPTY() , (sub)@(( store , 1000000mutez )) )
  |xxx}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; (test "sub_mutez_old.jsligo") ] ;
  [%expect{xxx|
const sub =
  lambda (gen#6) return let store = gen#6.0 in let delta = gen#6.1 in SUB(store ,
  delta)[@private]
const main =
  lambda (gen#11) return let _#12 = gen#11.0 in let store = gen#11.1 in
  ( LIST_EMPTY() , (sub)@(( store , 1000000mutez )) )[@private]
  |xxx}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-core" ; (test "sub_mutez_old.mligo") ] ;
  [%expect{|
    const sub : ( tez * tez ) -> tez =
      lambda (gen#7 : ( tez * tez )) : tez return  match gen#7 with
                                                    | (store,delta) -> C_POLYMORPHIC_SUB
                                                                       (store ,
                                                                        delta)
    const main : ( unit * tez ) -> ( list (operation) * tez ) =
      lambda (gen#11 : ( unit * tez )) : ( list (operation) * tez ) return
       match gen#11 with
        | (_#12,store) -> ( LIST_EMPTY() : list (operation) ,
                            (sub)@(( store , 1000000mutez )) )
  |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-core" ; (test "sub_mutez_old.ligo") ] ;
  [%expect{|
    const sub : ( tez * tez ) -> tez =
      lambda (parameters#7 : ( tez * tez )) : tez return  match parameters#7 with
                                                           | (store : tez,delta : tez) ->
                                                           C_POLYMORPHIC_SUB
                                                           (store ,
                                                            delta)
    const main : ( unit * tez ) -> ( list (operation) * tez ) =
      lambda (parameters#11 : ( unit * tez )) : ( list (operation) * tez ) return
       match parameters#11 with
        | (_#12 : unit,store : tez) -> ( LIST_EMPTY() : list (operation) ,
                                         (sub)@(( store , 1000000mutez )) )
  |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-core" ; (test "sub_mutez_old.religo") ] ;
  [%expect{|
    const sub =
      lambda (gen#7 : ( tez * tez )) : tez return  match gen#7 with
                                                    | (store,delta) -> C_POLYMORPHIC_SUB
                                                                       (store ,
                                                                        delta)
    const main =
      lambda (gen#11 : ( unit * tez )) : ( list (operation) * tez ) return
       match gen#11 with
        | (_#12,store) -> ( LIST_EMPTY() : list (operation) ,
                            (sub)@(( store , 1000000mutez )) )
 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-core" ; (test "sub_mutez_old.jsligo") ] ;
  [%expect{|
    const sub[@var] =
      rec (sub:( tez * tez ) -> tez => lambda (gen#6 : ( tez * tez )) : tez return
      let store = gen#6.0 in
      let delta = gen#6.1 in C_POLYMORPHIC_SUB(store , delta) )[@private]
    const main[@var] =
      rec (main:( unit * tez ) -> ( list (operation) * tez ) => lambda (gen#11 :
      ( unit * tez )) : ( list (operation) * tez ) return let _#12 = gen#11.0 in
                                                          let store = gen#11.1 in
                                                          ( LIST_EMPTY() : list (operation) ,
                                                            (sub)@(( store ,
                                                                     1000000mutez )) ) )[@private]
  |}]
