open Trace
open Mini_c.Types

open Proto_alpha_utils.Memory_proto_alpha
open Script_ir_translator

module O = Tezos_utils.Michelson
module Contract_types = Meta_michelson.Types

module Ty = struct

  let not_comparable name () = error (thunk "not a comparable type") (fun () -> name) ()
  let not_compilable_type name () = error (thunk "not a compilable type") (fun () -> name) ()

  let comparable_type_base : type_base -> ex_comparable_ty result = fun tb ->
    let open Contract_types in
    let return x = ok @@ Ex_comparable_ty x in
    match tb with
    | Base_unit -> fail (not_comparable "unit")
    | Base_void -> fail (not_comparable "void")
    | Base_bool -> fail (not_comparable "bool")
    | Base_nat -> return nat_k
    | Base_tez -> return tez_k
    | Base_int -> return int_k
    | Base_string -> return string_k
    | Base_address -> return address_k
    | Base_timestamp -> return timestamp_k
    | Base_bytes -> return bytes_k
    | Base_operation -> fail (not_comparable "operation")

  let comparable_type : type_value -> ex_comparable_ty result = fun tv ->
    match tv with
    | T_base b -> comparable_type_base b
    | T_deep_closure _ -> fail (not_comparable "deep closure")
    | T_function _ -> fail (not_comparable "function")
    | T_or _ -> fail (not_comparable "or")
    | T_pair _ -> fail (not_comparable "pair")
    | T_map _ -> fail (not_comparable "map")
    | T_list _ -> fail (not_comparable "list")
    | T_set _ -> fail (not_comparable "set")
    | T_option _ -> fail (not_comparable "option")
    | T_contract _ -> fail (not_comparable "contract")

  let base_type : type_base -> ex_ty result = fun b ->
    let open Contract_types in
    let return x = ok @@ Ex_ty x in
    match b with
    | Base_unit -> return unit
    | Base_void -> fail (not_compilable_type "void")
    | Base_bool -> return bool
    | Base_int -> return int
    | Base_nat -> return nat
    | Base_tez -> return tez
    | Base_string -> return string
    | Base_address -> return address
    | Base_timestamp -> return timestamp
    | Base_bytes -> return bytes
    | Base_operation -> return operation

  let rec type_ : type_value -> ex_ty result =
    function
    | T_base b -> base_type b
    | T_pair (t, t') -> (
        type_ t >>? fun (Ex_ty t) ->
        type_ t' >>? fun (Ex_ty t') ->
        ok @@ Ex_ty (Contract_types.pair t t')
      )
    | T_or (t, t') -> (
        type_ t >>? fun (Ex_ty t) ->
        type_ t' >>? fun (Ex_ty t') ->
        ok @@ Ex_ty (Contract_types.union t t')
      )
    | T_function (arg, ret) ->
        let%bind (Ex_ty arg) = type_ arg in
        let%bind (Ex_ty ret) = type_ ret in
        ok @@ Ex_ty (Contract_types.lambda arg ret)
    | T_deep_closure (c, arg, ret) ->
        let%bind (Ex_ty capture) = environment_representation c in
        let%bind (Ex_ty arg) = type_ arg in
        let%bind (Ex_ty ret) = type_ ret in
        ok @@ Ex_ty Contract_types.(pair (lambda (pair arg capture) ret) capture)
    | T_map (k, v) ->
        let%bind (Ex_comparable_ty k') = comparable_type k in
        let%bind (Ex_ty v') = type_ v in
        ok @@ Ex_ty Contract_types.(map k' v')
    | T_list t ->
        let%bind (Ex_ty t') = type_ t in
        ok @@ Ex_ty Contract_types.(list t')
    | T_set t -> (
        let%bind (Ex_comparable_ty t') = comparable_type t in
        ok @@ Ex_ty Contract_types.(set t')
      )
    | T_option t ->
        let%bind (Ex_ty t') = type_ t in
        ok @@ Ex_ty Contract_types.(option t')
    | T_contract t ->
        let%bind (Ex_ty t') = type_ t in
        ok @@ Ex_ty Contract_types.(contract t')

  and environment_representation = fun e ->
    match List.rev_uncons_opt e with
    | None -> ok @@ Ex_ty Contract_types.unit
    | Some (hds , tl) -> (
        let%bind tl_ty = type_ @@ snd tl in
        let aux (Ex_ty prec_ty) cur =
          let%bind (Ex_ty cur_ty) = type_ @@ snd cur in
          ok @@ Ex_ty Contract_types.(pair prec_ty cur_ty)
        in
        bind_fold_right_list aux tl_ty hds
      )

  and environment : environment -> Meta_michelson.Stack.ex_stack_ty result = fun env ->
    let open Meta_michelson in
    let%bind lst =
      bind_map_list type_
      @@ List.map snd env in
    let aux (Stack.Ex_stack_ty st) (Ex_ty cur) =
      Stack.Ex_stack_ty (Stack.stack cur st)
    in
    ok @@ List.fold_right' aux (Ex_stack_ty Stack.nil) lst

end


let base_type : type_base -> O.michelson result =
  function
  | Base_unit -> ok @@ O.prim T_unit
  | Base_void -> fail (Ty.not_compilable_type "void")
  | Base_bool -> ok @@ O.prim T_bool
  | Base_int -> ok @@ O.prim T_int
  | Base_nat -> ok @@ O.prim T_nat
  | Base_tez -> ok @@ O.prim T_mutez
  | Base_string -> ok @@ O.prim T_string
  | Base_address -> ok @@ O.prim T_address
  | Base_timestamp -> ok @@ O.prim T_timestamp
  | Base_bytes -> ok @@ O.prim T_bytes
  | Base_operation -> ok @@ O.prim T_operation

let rec type_ : type_value -> O.michelson result =
  function
  | T_base b -> base_type b
  | T_pair (t, t') -> (
      type_ t >>? fun t ->
      type_ t' >>? fun t' ->
      ok @@ O.prim ~children:[t;t'] O.T_pair
    )
  | T_or (t, t') -> (
      type_ t >>? fun t ->
      type_ t' >>? fun t' ->
      ok @@ O.prim ~children:[t;t'] O.T_or
    )
  | T_map kv ->
      let%bind (k', v') = bind_map_pair type_ kv in
      ok @@ O.prim ~children:[k';v'] O.T_map
  | T_list t ->
      let%bind t' = type_ t in
      ok @@ O.prim ~children:[t'] O.T_list
  | T_set t ->
      let%bind t' = type_ t in
      ok @@ O.prim ~children:[t'] O.T_set
  | T_option o ->
      let%bind o' = type_ o in
      ok @@ O.prim ~children:[o'] O.T_option
  | T_contract o ->
      let%bind o' = type_ o in
      ok @@ O.prim ~children:[o'] O.T_contract
  | T_function (arg, ret) ->
      let%bind arg = type_ arg in
      let%bind ret = type_ ret in
      ok @@ O.prim ~children:[arg;ret] T_lambda
  | T_deep_closure (c , arg , ret) ->
      let%bind capture = environment_closure c in
      let%bind lambda = lambda_closure (c , arg , ret) in
      ok @@ O.t_pair lambda capture

and environment_element (name, tyv) =
  let%bind michelson_type = type_ tyv in
  ok @@ O.annotate ("@" ^ name) michelson_type

and environment = fun env ->
  bind_map_list type_
  @@ List.map snd env

and lambda_closure = fun (c , arg , ret) ->
  let%bind capture = environment_closure c in
  let%bind arg = type_ arg in
  let%bind ret = type_ ret in
  ok @@ O.t_lambda (O.t_pair arg capture) ret

and environment_closure =
  function
  | [] -> simple_fail "Type of empty env"
  | [a] -> type_ @@ snd a
  | a :: b ->
      let%bind a = type_ @@ snd a in
      let%bind b = environment_closure b in
      ok @@ O.t_pair a b
