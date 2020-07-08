open Errors
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
  let key_hash_k = Key_hash_key None
  let address_k = Address_key None
  let timestamp_k = Timestamp_key None
  let bytes_k = Bytes_key None
  let pair_k a b = Pair_key ((a,None),(b,None),None)
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

  let comparable_type_base : type_base -> (ex_comparable_ty , stacking_error) result = fun tb ->
    let return x = ok @@ Ex_comparable_ty x in
    match tb with
    | TB_unit -> fail (Errors.not_comparable_base tb)
    | TB_bool -> return bool_k
    | TB_nat -> return nat_k
    | TB_mutez -> return tez_k
    | TB_int -> return int_k
    | TB_string -> return string_k
    | TB_address -> return address_k
    | TB_timestamp -> return timestamp_k
    | TB_bytes -> return bytes_k
    | TB_operation -> fail (Errors.not_comparable_base tb)
    | TB_signature -> fail (Errors.not_comparable_base tb)
    | TB_key -> fail (Errors.not_comparable_base tb) 
    | TB_key_hash -> return key_hash_k
    | TB_chain_id -> fail (Errors.not_comparable_base tb)

  let comparable_leaf : type a. (a, _) comparable_struct -> ((a , leaf) comparable_struct , stacking_error) result =
      fun a ->
    match a with
    | Pair_key _ -> fail Errors.not_comparable_pair_struct
    | Int_key annot -> ok (Int_key annot)
    | Nat_key annot -> ok (Nat_key annot)
    | String_key annot -> ok (String_key annot)
    | Bytes_key annot -> ok (Bytes_key annot)
    | Mutez_key annot -> ok (Mutez_key annot)
    | Bool_key annot -> ok (Bool_key annot)
    | Key_hash_key annot -> ok (Key_hash_key annot)
    | Timestamp_key annot -> ok (Timestamp_key annot)
    | Address_key annot -> ok (Address_key annot)

  let rec comparable_type : type_expression -> (ex_comparable_ty , stacking_error) result = fun tv ->
    match tv.type_content with
    | T_base b -> comparable_type_base b
    | T_function _ -> fail (Errors.not_comparable tv)
    | T_or _ -> fail (Errors.not_comparable tv)
    | T_pair ((_,a),(_,b)) ->
      let%bind (Ex_comparable_ty a') = comparable_type a in
      let%bind (Ex_comparable_ty b') = comparable_type b in
      let%bind a'' = comparable_leaf a' in
      ok @@ Ex_comparable_ty (pair_k a'' b')
    | T_map _ -> fail (Errors.not_comparable tv)
    | T_big_map _ -> fail (Errors.not_comparable tv)
    | T_list _ -> fail (Errors.not_comparable tv)
    | T_set _ -> fail (Errors.not_comparable tv)
    | T_option _ -> fail (Errors.not_comparable tv)
    | T_contract _ -> fail (Errors.not_comparable tv)

  let base_type : type_base -> (ex_ty , stacking_error) result = fun b ->
    let return x = ok @@ Ex_ty x in
   match b with
    | TB_unit -> return unit
    | TB_bool -> return bool
    | TB_int -> return int
    | TB_nat -> return nat
    | TB_mutez -> return tez
    | TB_string -> return string
    | TB_address -> return address
    | TB_timestamp -> return timestamp
    | TB_bytes -> return bytes
    | TB_operation -> return operation
    | TB_signature -> return signature
    | TB_key -> return key
    | TB_key_hash -> return key_hash
    | TB_chain_id -> return chain_id

  let rec type_ : type_expression -> (ex_ty , stacking_error) result =
    fun te -> match te.type_content with
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

  and annotated : type_expression annotated -> (ex_ty annotated , stacking_error) result =
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

  and environment : environment -> (ex_stack_ty , stacking_error) result = fun env ->
    let%bind lst =
      bind_map_list type_
      @@ List.map snd env in
    let aux (Ex_stack_ty st) (Ex_ty cur) =
      Ex_stack_ty (Item_t(cur, st, None))
    in
    ok @@ List.fold_right' aux (Ex_stack_ty Empty_t) lst

end

let base_type : type_base -> (O.michelson , stacking_error) result =
  function
  | TB_unit -> ok @@ O.prim T_unit
  | TB_bool -> ok @@ O.prim T_bool
  | TB_int -> ok @@ O.prim T_int
  | TB_nat -> ok @@ O.prim T_nat
  | TB_mutez -> ok @@ O.prim T_mutez
  | TB_string -> ok @@ O.prim T_string
  | TB_address -> ok @@ O.prim T_address
  | TB_timestamp -> ok @@ O.prim T_timestamp
  | TB_bytes -> ok @@ O.prim T_bytes
  | TB_operation -> ok @@ O.prim T_operation
  | TB_signature -> ok @@ O.prim T_signature
  | TB_key -> ok @@ O.prim T_key
  | TB_key_hash -> ok @@ O.prim T_key_hash
  | TB_chain_id -> ok @@ O.prim T_chain_id

let rec type_ : type_expression -> (O.michelson , stacking_error) result =
  fun te -> match te.type_content with
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

and annotated : type_expression annotated -> (O.michelson , stacking_error) result =
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

and environment_closure =
  function
  | [] -> fail @@ corner_case ~loc:"TODO" "Type of empty env"
  | [a] -> type_ @@ snd a
  | a :: b ->
      let%bind a = type_ @@ snd a in
      let%bind b = environment_closure b in
      ok @@ O.t_pair a b
