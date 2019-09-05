open Trace
open Mini_c.Types

open Proto_alpha_utils.Memory_proto_alpha
open Protocol
open Script_ir_translator

module O = Tezos_utils.Michelson

module Ty = struct

  let not_comparable name () = error (thunk "not a comparable type") (fun () -> name) ()
  let not_compilable_type name () = error (thunk "not a compilable type") (fun () -> name) ()

  open Script_typed_ir


  let nat_k = Nat_key None
  let tez_k = Mutez_key None
  let int_k = Int_key None
  let string_k = String_key None
  let address_k = Address_key None
  let timestamp_k = Timestamp_key None
  let bytes_k = Bytes_key None
  (* let timestamp_k = Timestamp_key None *)

  let unit = Unit_t None
  let bytes = Bytes_t None
  let nat = Nat_t None
  let tez = Mutez_t None
  let int = Int_t None
  let big_map k v = Big_map_t (k, v, None)
  let signature = Signature_t None
  let operation = Operation_t None
  let bool = Bool_t None
  let mutez = Mutez_t None
  let string = String_t None
  let key = Key_t None
  let list a = List_t (a, None)
  let set a = Set_t (a, None)
  let address = Address_t None
  let option a = Option_t ((a, None), None, None)
  let contract a = Contract_t (a, None)
  let lambda a b = Lambda_t (a, b, None)
  let timestamp = Timestamp_t None
  let map a b = Map_t (a, b, None)
  let pair a b = Pair_t ((a, None, None), (b, None, None), None)
  let union a b = Union_t ((a, None), (b, None), None)

  let comparable_type_base : type_base -> ex_comparable_ty result = fun tb ->
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
        ok @@ Ex_ty (pair t t')
      )
    | T_or (t, t') -> (
        type_ t >>? fun (Ex_ty t) ->
        type_ t' >>? fun (Ex_ty t') ->
        ok @@ Ex_ty (union t t')
      )
    | T_function (arg, ret) ->
        let%bind (Ex_ty arg) = type_ arg in
        let%bind (Ex_ty ret) = type_ ret in
        ok @@ Ex_ty (lambda arg ret)
    | T_deep_closure (c, arg, ret) ->
        let%bind (Ex_ty capture) = environment_representation c in
        let%bind (Ex_ty arg) = type_ arg in
        let%bind (Ex_ty ret) = type_ ret in
        ok @@ Ex_ty (pair (lambda (pair arg capture) ret) capture)
    | T_map (k, v) ->
        let%bind (Ex_comparable_ty k') = comparable_type k in
        let%bind (Ex_ty v') = type_ v in
        ok @@ Ex_ty (map k' v')
    | T_list t ->
        let%bind (Ex_ty t') = type_ t in
        ok @@ Ex_ty (list t')
    | T_set t -> (
        let%bind (Ex_comparable_ty t') = comparable_type t in
        ok @@ Ex_ty (set t')
      )
    | T_option t ->
        let%bind (Ex_ty t') = type_ t in
        ok @@ Ex_ty (option t')
    | T_contract t ->
        let%bind (Ex_ty t') = type_ t in
        ok @@ Ex_ty (contract t')

  and environment_representation = function
    | [] -> ok @@ Ex_ty unit
    | [a] -> type_ @@ snd a
    | a::b ->
        let%bind (Ex_ty a) = type_ @@ snd a in
        let%bind (Ex_ty b) = environment_representation b in
        ok @@ Ex_ty (pair a b)

  and environment : environment -> ex_stack_ty result = fun env ->
    let%bind lst =
      bind_map_list type_
      @@ List.map snd env in
    let aux (Ex_stack_ty st) (Ex_ty cur) =
      Ex_stack_ty (Item_t (cur,  st, None))
    in
    ok @@ List.fold_right' aux (Ex_stack_ty Empty_t) lst

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
  | T_deep_closure (c, arg, ret) ->
      let%bind capture = environment_closure c in
      let%bind arg = type_ arg in
      let%bind ret = type_ ret in
      ok @@ O.t_pair (O.t_lambda (O.t_pair arg capture) ret) capture

and environment_element (name, tyv) =
  let%bind michelson_type = type_ tyv in
  ok @@ O.annotate ("@" ^ name) michelson_type

and environment = fun env ->
  bind_map_list type_
  @@ List.map snd env

and environment_closure =
  function
  | [] -> simple_fail "Type of empty env"
  | [a] -> type_ @@ snd a
  | a :: b ->
      let%bind a = type_ @@ snd a in
      let%bind b = environment_closure b in
      ok @@ O.t_pair a b
