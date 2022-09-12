open Cli_expect

let contract basename =
  "../../test/contracts/modules_and_free_vars/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; contract "simple.mligo" ] ;
  [%expect {|
module Tezo = struct
              const amoun : tez = 1000000mutez
              endconst balanc : tez = 2000000mutezconst size : int = 10
const bal : tez = ADD(balanc , 1000000mutez)
const amt : tez = ADD(Tezo.amoun , 1000000mutez)
type parameter = sum[Decrement -> unit , Increment -> unit]type storage = tez
type return = ( list (operation) * tez )
const main : ( sum[Decrement -> unit , Increment -> unit] * tez ) ->
  ( list (operation) * tez ) =
  lambda (gen#2( sum[Decrement -> unit , Increment -> unit] * tez ))( list (operation) * tez ) return
   match gen#2 with
    | ( action : sum[Decrement -> unit , Increment -> unit] , _#3 : tez ) ->
    ( LIST_EMPTY() ,
       match action with
        | Decrement unit_proj#4 ->
          amt | Increment unit_proj#5 ->
                bal ) |}]
let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; contract "nested_modules.mligo" ] ;
  [%expect {|
module Tezo =
  struct
  module X = struct
             module Y = struct
                        const amoun : tez = 1000000mutez
                        end
             end
  endconst balanc : tez = 2000000mutezconst size : int = 10
const bal : tez = ADD(balanc , 1000000mutez)
const amt : tez = ADD(Tezo.X.Y.amoun , 1000000mutez)
type parameter = sum[Decrement -> unit , Increment -> unit]type storage = tez
type return = ( list (operation) * tez )
const main : ( sum[Decrement -> unit , Increment -> unit] * tez ) ->
  ( list (operation) * tez ) =
  lambda (gen#2( sum[Decrement -> unit , Increment -> unit] * tez ))( list (operation) * tez ) return
   match gen#2 with
    | ( action : sum[Decrement -> unit , Increment -> unit] , _#3 : tez ) ->
    ( LIST_EMPTY() ,
       match action with
        | Decrement unit_proj#4 ->
          amt | Increment unit_proj#5 ->
                bal ) |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; contract "module_with_free_vars.mligo" ] ;
  [%expect {|
const x : tez = 1000000mutezmodule Tezo = struct
                                          const amoun : tez = x
                                          end
const balanc : tez = 2000000mutezconst size : int = 10
const bal : tez = ADD(balanc , 1000000mutez)
const amt : tez = ADD(Tezo.amoun , 1000000mutez)
type parameter = sum[Decrement -> unit , Increment -> unit]type storage = tez
type return = ( list (operation) * tez )
const main : ( sum[Decrement -> unit , Increment -> unit] * tez ) ->
  ( list (operation) * tez ) =
  lambda (gen#2( sum[Decrement -> unit , Increment -> unit] * tez ))( list (operation) * tez ) return
   match gen#2 with
    | ( action : sum[Decrement -> unit , Increment -> unit] , _#3 : tez ) ->
    ( LIST_EMPTY() ,
       match action with
        | Decrement unit_proj#4 ->
          amt | Increment unit_proj#5 ->
                bal ) |}]

let%expect_test _ =
run_ligo_good [ "print" ; "ast-typed" ; contract "nested_modules_with_free_vars.mligo" ] ;
[%expect{|
const used : tez = 1000000mutezconst unused : tez = 2000000mutez
module Tezo =
  struct
  const used : tez = used
  const unused : tez = unused
  module X =
    struct
    const used : tez = used
    const unused : tez = unused
    module Y = struct
               const used : tez = used
               const unused : tez = unused
               end
    end
  endconst used : tez = Tezo.X.Y.usedconst unused : tez = Tezo.X.Y.unused
type parameter = sum[Decrement -> unit , Increment -> unit]type storage = tez
type return = ( list (operation) * tez )
const main : ( sum[Decrement -> unit , Increment -> unit] * tez ) ->
  ( list (operation) * tez ) =
  lambda (gen#2( sum[Decrement -> unit , Increment -> unit] * tez ))( list (operation) * tez ) return
   match gen#2 with
    | ( action : sum[Decrement -> unit , Increment -> unit] , _#3 : tez ) ->
    ( LIST_EMPTY() ,
       match action with
        | Decrement unit_proj#4 ->
          1000000mutez
        | Increment unit_proj#5 ->
          used ) |}]
