module AST = Ast_expanded
module Append_tree = Simple_utils.Tree.Append
module Ligo_string = Simple_utils.Ligo_string
module Errors = Errors
open Errors
open Mini_c
open Simple_utils.Trace
open Ligo_prim.Literal_types

let rec decompile ~raise (v : value) (t : AST.type_expression) : AST.expression =
  let open! AST in
  let loc = Location.dummy in
  let self = decompile ~raise in
  let return e = make_e ~loc e t in
  match t.type_content with
  | tc when AST.compare_type_content tc (t_bool ~loc ()).type_content = 0 ->
    let b = trace_option ~raise (wrong_mini_c_value t v) @@ get_bool v in
    return (e_bool ~loc b)
  | T_constant { language; injection; parameters } ->
    let () =
      Assert.assert_true
        ~raise
        (corner_case ~loc:__LOC__ ("unsupported language " ^ language))
        (String.equal language Backend.Michelson.name)
    in
    (match injection, parameters with
    | Unit, [] ->
      let () = trace_option ~raise (wrong_mini_c_value t v) @@ get_unit v in
      return (E_literal Literal_unit)
    | Int, [] ->
      let n = trace_option ~raise (wrong_mini_c_value t v) @@ get_int v in
      return (E_literal (Literal_int n))
    | Nat, [] ->
      let n = trace_option ~raise (wrong_mini_c_value t v) @@ get_nat v in
      return (E_literal (Literal_nat n))
    | Timestamp, [] ->
      let n = trace_option ~raise (wrong_mini_c_value t v) @@ get_timestamp v in
      return (E_literal (Literal_timestamp n))
    | Tez, [] ->
      let n = trace_option ~raise (wrong_mini_c_value t v) @@ get_mutez v in
      return (E_literal (Literal_mutez n))
    | String, [] ->
      let n = trace_option ~raise (wrong_mini_c_value t v) @@ get_string v in
      let n = Ligo_string.Standard n in
      return (E_literal (Literal_string n))
    | Bytes, [] ->
      let n = trace_option ~raise (wrong_mini_c_value t v) @@ get_bytes v in
      return (E_literal (Literal_bytes n))
    | Bls12_381_g1, [] ->
      let n = trace_option ~raise (wrong_mini_c_value t v) @@ get_bytes v in
      return (E_literal (Literal_bls12_381_g1 n))
    | Bls12_381_g2, [] ->
      let n = trace_option ~raise (wrong_mini_c_value t v) @@ get_bytes v in
      return (E_literal (Literal_bls12_381_g2 n))
    | Bls12_381_fr, [] ->
      let n = trace_option ~raise (wrong_mini_c_value t v) @@ get_bytes v in
      return (E_literal (Literal_bls12_381_fr n))
    | Chest, [] ->
      let n = trace_option ~raise (wrong_mini_c_value t v) @@ get_bytes v in
      return (E_literal (Literal_chest n))
    | Chest_key, [] ->
      let n = trace_option ~raise (wrong_mini_c_value t v) @@ get_bytes v in
      return (E_literal (Literal_chest_key n))
    | Address, [] ->
      let n = trace_option ~raise (wrong_mini_c_value t v) @@ get_string v in
      return (E_literal (Literal_address n))
    | Operation, [] ->
      let op = trace_option ~raise (wrong_mini_c_value t v) @@ get_operation v in
      return (E_literal (Literal_operation op))
    | Key, [] ->
      let n = trace_option ~raise (wrong_mini_c_value t v) @@ get_string v in
      return (E_literal (Literal_key n))
    | Key_hash, [] ->
      let n = trace_option ~raise (wrong_mini_c_value t v) @@ get_string v in
      return (E_literal (Literal_key_hash n))
    | Chain_id, [] ->
      let n = trace_option ~raise (wrong_mini_c_value t v) @@ get_string v in
      return (E_literal (Literal_chain_id n))
    | Signature, [] ->
      let n = trace_option ~raise (wrong_mini_c_value t v) @@ get_string v in
      return (E_literal (Literal_signature n))
    | Map, [ k_ty; v_ty ] ->
      let map = trace_option ~raise (wrong_mini_c_value t v) @@ get_map v in
      let map' =
        let aux (k, v) =
          let key = self k k_ty in
          let value = self v v_ty in
          key, value
        in
        List.map ~f:aux map
      in
      let map' = List.dedup_and_sort ~compare:Caml.compare map' in
      (* AST.Compare.map_kbv is broken because of expression and litteral being broken *)
      let aux (key, value) prev =
        return @@ E_constant { cons_name = C_MAP_ADD; arguments = [ key; value; prev ] }
      in
      let init = return @@ E_constant { cons_name = C_MAP_EMPTY; arguments = [] } in
      List.fold_right ~f:aux ~init map'
    | Big_map, [ k_ty; v_ty ] ->
      let big_map = trace_option ~raise (wrong_mini_c_value t v) @@ get_big_map v in
      let big_map' =
        let aux (k, v) =
          let key = self k k_ty in
          let value = self v v_ty in
          key, value
        in
        List.map ~f:aux big_map
      in
      let big_map' = List.dedup_and_sort ~compare:Caml.compare big_map' in
      let aux (key, value) prev =
        return @@ E_constant { cons_name = C_MAP_ADD; arguments = [ key; value; prev ] }
      in
      let init = return @@ E_constant { cons_name = C_BIG_MAP_EMPTY; arguments = [] } in
      List.fold_right ~f:aux ~init big_map'
    | List, [ ty ] ->
      let lst = trace_option ~raise (wrong_mini_c_value t v) @@ get_list v in
      let lst' =
        let aux e = self e ty in
        List.map ~f:aux lst
      in
      let aux cur prev =
        return @@ E_constant { cons_name = C_CONS; arguments = [ cur; prev ] }
      in
      let init = return @@ E_constant { cons_name = C_LIST_EMPTY; arguments = [] } in
      List.fold_right ~f:aux ~init lst'
    | Set, [ ty ] ->
      let lst = trace_option ~raise (wrong_mini_c_value t v) @@ get_set v in
      let lst' =
        let aux e = self e ty in
        List.map ~f:aux lst
      in
      let lst' = List.dedup_and_sort ~compare:Caml.compare lst' in
      let aux prev cur =
        return @@ E_constant { cons_name = C_SET_ADD; arguments = [ cur; prev ] }
      in
      let init = return @@ E_constant { cons_name = C_SET_EMPTY; arguments = [] } in
      List.fold ~f:aux ~init lst'
    | Ticket, [ _ty ] ->
      (* TODO: should we use a Michelson insertion? *)
      raise.error @@ bad_decompile v
    | Contract, _ -> raise.error @@ bad_decompile v
    | (Michelson_pair | Michelson_or), _ ->
      raise.error
      @@ corner_case ~loc:"unspiller" "Michelson_combs t should not be present in mini-c"
    | ( ( Unit
        | Nat
        | Tez
        | Bytes
        | Bls12_381_g1
        | Bls12_381_g2
        | Bls12_381_fr
        | Address
        | Key
        | Chain_id
        | Signature
        | Map
        | Big_map
        | Set
        | Int64
        | Baker_hash
        | Pvss_key
        | Sapling_state
        | Sapling_transaction
        | Baker_operation
        | Never
        | Michelson_program
        | Michelson_contract
        | Gen
        | String
        | Typed_address
        | Mutation
        | List
        | Int
        | Key_hash
        | Ticket
        | Timestamp
        | Operation
        | Tx_rollup_l2_address
        | External _
        | Views
        | Dynamic_entrypoint
        | Chest
        | Chest_key )
      , _ ) ->
      let () = Format.printf "%a" AST.PP.type_content t.type_content in
      raise.error
      @@ corner_case
           ~loc:"unspiller"
           "Wrong number of args or wrong kinds for the type constant")
  | T_sum _ when Option.is_some (Ast_aggregated.get_t_option t) ->
    (match v with
    | D_some v ->
      let tv =
        trace_option ~raise (corner_case ~loc:"unspiller" "impossible")
        @@ Ast_aggregated.get_t_option t
      in
      let sub = self v tv in
      return
        (E_constructor { constructor = Ligo_prim.Label.create "Some"; element = sub })
    | D_none ->
      return
        (E_constructor
           { constructor = Ligo_prim.Label.create "None"
           ; element = make_e ~loc (e_unit ()) (t_unit ~loc ())
           })
    | _ -> raise.error @@ corner_case ~loc:"unspiller" "impossible")
  | T_sum sum ->
    let constructor, v, tv = Row.extract_constructor sum v get_left get_right in
    let sub = self v tv in
    return (E_constructor { constructor; element = sub })
  | T_record reco ->
    let lst = Row.extract_record reco v get_pair in
    let lst = List.map ~f:(fun (l, v, ty) -> l, (v, ty)) lst in
    let lst = List.Assoc.map ~f:(fun (y, z) -> self y z) lst in
    let m' = Ligo_prim.Record.of_list lst in
    return (E_record m')
  | T_arrow _ ->
    let n = trace_option ~raise (wrong_mini_c_value t v) @@ get_string v in
    let n = Ligo_string.Standard n in
    return (E_literal (Literal_string n))
  | T_variable _ ->
    raise.error @@ corner_case ~loc:__LOC__ "trying to decompile at variable type"
  | T_singleton _ ->
    raise.error @@ corner_case ~loc:__LOC__ "no value is of type singleton"
  | T_for_all _ ->
    raise.error
    @@ corner_case ~loc:__LOC__ "trying to decompile a quantified type (no such thing ?)"
