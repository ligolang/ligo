open Trace
open Mini_c.Types

open Proto_alpha_utils.Memory_proto_alpha
open Protocol
open Script_ir_translator

module O = Tezos_utils.Michelson

module Ty = struct

  open Script_typed_ir

  let bool_k = Bool_key None
  let nat_k = Nat_key None
  let tez_k = Mutez_key None
  let int_k = Int_key None
  let string_k = String_key None
  let _key_hash_k = Key_hash_key None
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
  let key_hash = Key_hash_t None
  let chain_id = Chain_id_t None
  let list a = List_t (a, None , has_big_map a)
  let set a = Set_t (a, None)
  let address = Address_t None
  let option a = Option_t (a, None , has_big_map a)
  let contract a = Contract_t (a, None)
  let lambda a b = Lambda_t (a, b, None)
  let timestamp = Timestamp_t None
  let map a b = Map_t (a, b, None , has_big_map b)
  let pair a b = Pair_t ((a, None, None), (b, None, None), None , has_big_map a || has_big_map b)
  let union a b = Union_t ((a, None), (b, None), None , has_big_map a || has_big_map b)

  let field_annot = Option.map (fun ann -> `Field_annot ann)

  let union_ann (anna, a) (annb, b) =
    Union_t ((a, field_annot anna), (b, field_annot annb), None , has_big_map a || has_big_map b)

  let pair_ann (anna, a) (annb, b) =
    Pair_t ((a, field_annot anna, None), (b, field_annot annb, None), None , has_big_map a || has_big_map b)

  let not_comparable name () = error (thunk "not a comparable type") (fun () -> name) ()
  let not_compilable_type name () = error (thunk "not a compilable type") (fun () -> name) ()

  let comparable_type_base : type_constant -> ex_comparable_ty result = fun tb ->
    let return x = ok @@ Ex_comparable_ty x in
    match tb with
    | TC_unit -> fail (not_comparable "unit")
    | TC_void -> fail (not_comparable "void")
    | TC_bool -> return bool_k
    | TC_nat -> return nat_k
    | TC_mutez -> return tez_k
    | TC_int -> return int_k
    | TC_string -> return string_k
    | TC_address -> return address_k
    | TC_timestamp -> return timestamp_k
    | TC_bytes -> return bytes_k
    | TC_operation -> fail (not_comparable "operation")
    | TC_signature -> fail (not_comparable "signature")
    | TC_key -> fail (not_comparable "key")
    | TC_key_hash -> fail (not_comparable "key_hash")
    | TC_chain_id -> fail (not_comparable "chain_id")

  let comparable_type : type_value -> ex_comparable_ty result = fun tv ->
    match tv with
    | T_base b -> comparable_type_base b
    | T_function _ -> fail (not_comparable "function")
    | T_or _ -> fail (not_comparable "or")
    | T_pair _ -> fail (not_comparable "pair")
    | T_map _ -> fail (not_comparable "map")
    | T_big_map _ -> fail (not_comparable "big_map")
    | T_list _ -> fail (not_comparable "list")
    | T_set _ -> fail (not_comparable "set")
    | T_option _ -> fail (not_comparable "option")
    | T_contract _ -> fail (not_comparable "contract")

  let base_type : type_constant -> ex_ty result = fun b ->
    let return x = ok @@ Ex_ty x in
   match b with
    | TC_unit -> return unit
    | TC_void -> fail (not_compilable_type "void")
    | TC_bool -> return bool
    | TC_int -> return int
    | TC_nat -> return nat
    | TC_mutez -> return tez
    | TC_string -> return string
    | TC_address -> return address
    | TC_timestamp -> return timestamp
    | TC_bytes -> return bytes
    | TC_operation -> return operation
    | TC_signature -> return signature
    | TC_key -> return key
    | TC_key_hash -> return key_hash
    | TC_chain_id -> return chain_id

  let rec type_ : type_value -> ex_ty result =
    function
    | T_base b -> base_type b
    | T_pair (t, t') -> (
        annotated t >>? fun (ann, Ex_ty t) ->
        annotated t' >>? fun (ann', Ex_ty t') ->
        ok @@ Ex_ty (pair_ann (ann, t) (ann', t'))
      )
    | T_or (t, t') -> (
        annotated t >>? fun (ann, Ex_ty t) ->
        annotated t' >>? fun (ann', Ex_ty t') ->
        ok @@ Ex_ty (union_ann (ann, t) (ann', t'))
      )
    | T_function (arg, ret) ->
        let%bind (Ex_ty arg) = type_ arg in
        let%bind (Ex_ty ret) = type_ ret in
        ok @@ Ex_ty (lambda arg ret)
    | T_map (k, v) ->
        let%bind (Ex_comparable_ty k') = comparable_type k in
        let%bind (Ex_ty v') = type_ v in
        ok @@ Ex_ty (map k' v')
    | T_big_map (k, v) ->
        let%bind (Ex_comparable_ty k') = comparable_type k in
        let%bind (Ex_ty v') = type_ v in
        ok @@ Ex_ty (big_map k' v')
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

  and annotated : type_value annotated -> ex_ty annotated result =
    fun (ann, a) -> let%bind a = type_ a in
                    ok @@ (ann, a)

  and environment_representation = fun e ->
    match List.rev_uncons_opt e with
    | None -> ok @@ Ex_ty unit
    | Some (hds , tl) -> (
        let%bind tl_ty = type_ @@ snd tl in
        let aux (Ex_ty prec_ty) cur =
          let%bind (Ex_ty cur_ty) = type_ @@ snd cur in
          ok @@ Ex_ty (pair prec_ty cur_ty)
        in
        bind_fold_right_list aux tl_ty hds
      )

  and environment : environment -> ex_stack_ty result = fun env ->
    let%bind lst =
      bind_map_list type_
      @@ List.map snd env in
    let aux (Ex_stack_ty st) (Ex_ty cur) =
      Ex_stack_ty (Item_t(cur, st, None))
    in
    ok @@ List.fold_right' aux (Ex_stack_ty Empty_t) lst

end


let base_type : type_constant -> O.michelson result =
  function
  | TC_unit -> ok @@ O.prim T_unit
  | TC_void -> fail (Ty.not_compilable_type "void")
  | TC_bool -> ok @@ O.prim T_bool
  | TC_int -> ok @@ O.prim T_int
  | TC_nat -> ok @@ O.prim T_nat
  | TC_mutez -> ok @@ O.prim T_mutez
  | TC_string -> ok @@ O.prim T_string
  | TC_address -> ok @@ O.prim T_address
  | TC_timestamp -> ok @@ O.prim T_timestamp
  | TC_bytes -> ok @@ O.prim T_bytes
  | TC_operation -> ok @@ O.prim T_operation
  | TC_signature -> ok @@ O.prim T_signature
  | TC_key -> ok @@ O.prim T_key
  | TC_key_hash -> ok @@ O.prim T_key_hash
  | TC_chain_id -> ok @@ O.prim T_chain_id

let rec type_ : type_value -> O.michelson result =
  function
  | T_base b -> base_type b
  | T_pair (t, t') -> (
      annotated t >>? fun t ->
      annotated t' >>? fun t' ->
      ok @@ O.prim ~children:[t;t'] O.T_pair
    )
  | T_or (t, t') -> (
      annotated t >>? fun t ->
      annotated t' >>? fun t' ->
      ok @@ O.prim ~children:[t;t'] O.T_or
    )
  | T_map kv ->
      let%bind (k', v') = bind_map_pair type_ kv in
      ok @@ O.prim ~children:[k';v'] O.T_map
  | T_big_map kv ->
      let%bind (k', v') = bind_map_pair type_ kv in
      ok @@ O.prim ~children:[k';v'] O.T_big_map
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

and annotated : type_value annotated -> O.michelson result =
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
  let%bind arg = type_ arg in
  let%bind ret = type_ ret in
  match c with
  | [] -> ok @@ O.t_lambda arg ret
  | _ :: _ ->
    let%bind capture = environment_closure c in
    ok @@ O.t_lambda (O.t_pair capture arg) ret

and environment_closure =
  function
  | [] -> simple_fail "Type of empty env"
  | [a] -> type_ @@ snd a
  | a :: b ->
      let%bind a = type_ @@ snd a in
      let%bind b = environment_closure b in
      ok @@ O.t_pair a b
