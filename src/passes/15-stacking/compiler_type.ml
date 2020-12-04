open Errors
open Trace
open Mini_c.Types

module O = Tezos_utils.Michelson

let base_type : type_base -> (_ O.michelson , stacking_error) result =
  function
  | TB_unit -> ok @@ O.prim "unit"
  | TB_bool -> ok @@ O.prim "bool"
  | TB_int -> ok @@ O.prim "int"
  | TB_nat -> ok @@ O.prim "nat"
  | TB_mutez -> ok @@ O.prim "mutez"
  | TB_string -> ok @@ O.prim "string"
  | TB_address -> ok @@ O.prim "address"
  | TB_timestamp -> ok @@ O.prim "timestamp"
  | TB_bytes -> ok @@ O.prim "bytes"
  | TB_operation -> ok @@ O.prim "operation"
  | TB_signature -> ok @@ O.prim "signature"
  | TB_key -> ok @@ O.prim "key"
  | TB_key_hash -> ok @@ O.prim "key_hash"
  | TB_chain_id -> ok @@ O.prim "chain_id"
  | TB_baker_hash -> ok @@ O.prim "baker_hash"
  | TB_pvss_key -> ok @@ O.prim "pvss_key"
  | TB_sapling_transaction -> ok @@ O.prim "sapling_transaction"
  | TB_sapling_state -> ok @@ O.prim "sapling_state"
  | TB_baker_operation -> ok @@ O.prim "baker_operation"
  | TB_bls12_381_g1 -> ok @@ O.prim "bls12_381_g1"
  | TB_bls12_381_g2 -> ok @@ O.prim "bls12_381_g2"
  | TB_bls12_381_fr -> ok @@ O.prim "bls12_381_fr"

let rec type_ : type_expression -> (_ O.michelson , stacking_error) result =
  fun te -> match te.type_content with
  | T_base b -> base_type b
  | T_pair (t, t') -> (
      annotated t >>? fun t ->
      annotated t' >>? fun t' ->
      ok @@ O.prim ~children:[t;t'] "pair"
    )
  | T_or (t, t') -> (
      annotated t >>? fun t ->
      annotated t' >>? fun t' ->
      ok @@ O.prim ~children:[t;t'] "or"
    )
  | T_map kv ->
      let%bind (k', v') = bind_map_pair type_ kv in
      ok @@ O.prim ~children:[k';v'] "map"
  | T_big_map kv ->
      let%bind (k', v') = bind_map_pair type_ kv in
      ok @@ O.prim ~children:[k';v'] "big_map"
  | T_list t ->
      let%bind t' = type_ t in
      ok @@ O.prim ~children:[t'] "list"
  | T_set t ->
      let%bind t' = type_ t in
      ok @@ O.prim ~children:[t'] "set"
  | T_option o ->
      let%bind o' = type_ o in
      ok @@ O.prim ~children:[o'] "option"
  | T_contract o ->
      let%bind o' = type_ o in
      ok @@ O.prim ~children:[o'] "contract"
  | T_function (arg, ret) ->
      let%bind arg = type_ arg in
      let%bind ret = type_ ret in
      ok @@ O.prim ~children:[arg;ret] "lambda"

and annotated : type_expression annotated -> (_ O.michelson , stacking_error) result =
  function
  | (Some ann, o) ->
     let%bind o' = type_ o in
     ok (O.annotate ("%" ^ ann) o')
  | (None, o) -> type_ o

and environment_element (name, tyv) =
  let%bind michelson_type = type_ tyv in
  ok @@ O.annotate ("@" ^ name) michelson_type

and environment = fun env ->
  bind_map_list type_
  @@ List.map snd env

and lambda_closure = fun (c , arg , ret) ->
  let%bind (lambda , _arg' , _ret') =
    lambda_closure_with_ty (c , arg , ret) in
  ok lambda

and lambda_closure_with_ty = fun (c , arg , ret) ->
  let%bind arg = type_ arg in
  let%bind ret = type_ ret in
  match c with
  | [] -> ok @@ (O.t_lambda arg ret , arg , ret)
  | _ :: _ ->
    let%bind capture = environment_closure c in
    let arg' = O.t_pair capture arg in
    ok @@ (O.t_lambda arg' ret , arg' , ret)

and lcomb =
  function
  | [] -> fail @@ corner_case ~loc:"TODO" "Type of empty env"
  | [a] -> type_ @@ a
  | a :: b ->
      let%bind a = type_ @@ a in
      let%bind b = lcomb b in
      ok @@ O.t_pair b a

and environment_closure c =
  lcomb (List.rev c)

let%expect_test _ =
  let wrap a = {type_content = a; location = Location.dummy} in
  (match to_stdlib_result @@ environment_closure [wrap (T_base TB_nat); wrap (T_base TB_int); wrap (T_base TB_timestamp)] with
   | Error _ -> Format.printf "ERROR"
   | Ok (t, _) ->
     Format.printf "%a" Michelson.pp t);
  [%expect {| (pair (pair nat int) timestamp) |}]
