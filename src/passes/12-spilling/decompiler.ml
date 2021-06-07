module AST = Ast_typed
module Append_tree = Tree.Append
module Errors = Errors
open Errors
open Mini_c
open Trace
open Stage_common.Constant

let rec decompile (v : value) (t : AST.type_expression) : (AST.expression , spilling_error) result =
  let open! AST in
  let return e = ok (make_e e t) in
  match t.type_content with
  | tc when (AST.Compare.type_content tc (t_bool ()).type_content) = 0-> (
        let* b =
          trace_option (wrong_mini_c_value t v) @@
          get_bool v in
        return (e_bool b)
      )
  | T_constant { language; injection; parameters } -> (
    let* () = Assert.assert_true
      (corner_case ~loc:__LOC__ ("unsupported language "^language))
      (String.equal language Stage_common.Backends.michelson)
    in
    match (Ligo_string.extract injection,parameters) with
    | (i, []) when String.equal i unit_name -> (
        let* () =
          trace_option (wrong_mini_c_value t v) @@
          get_unit v in
        return (E_literal Literal_unit)
      )
    | (i, []) when String.equal i int_name -> (
        let* n =
          trace_option (wrong_mini_c_value t v) @@
          get_int v in
        return (E_literal (Literal_int n))
      )
    | (i, []) when String.equal i nat_name -> (
        let* n =
          trace_option (wrong_mini_c_value t v) @@
          get_nat v in
        return (E_literal (Literal_nat n))
      )
    | (i, []) when String.equal i timestamp_name -> (
        let* n =
          trace_option (wrong_mini_c_value t v) @@
          get_timestamp v in
        return (E_literal (Literal_timestamp n))
      )
    | (i, []) when String.equal i tez_name -> (
        let* n =
          trace_option (wrong_mini_c_value t v) @@
          get_mutez v in
        return (E_literal (Literal_mutez n))
      )
    | (i, []) when String.equal i string_name -> (
        let* n =
          trace_option (wrong_mini_c_value t v) @@
          get_string v in
        let n = Ligo_string.Standard n in
        return (E_literal (Literal_string n))
      )
    | (i, []) when String.equal i bytes_name -> (
        let* n =
          trace_option (wrong_mini_c_value t v) @@
          get_bytes v in
        return (E_literal (Literal_bytes n))
      )
    | (i, []) when String.equal i address_name -> (
        let* n =
          trace_option (wrong_mini_c_value t v) @@
          get_string v in
        return (E_literal (Literal_address n))
      )
    | (i, []) when String.equal i operation_name -> (
        let* op =
          trace_option (wrong_mini_c_value t v) @@
          get_operation v in
        return (E_literal (Literal_operation op))
      )
    |  (i, []) when String.equal i key_name -> (
        let* n =
          trace_option (wrong_mini_c_value t v) @@
          get_string v in
        return (E_literal (Literal_key n))
      )
    |  (i, []) when String.equal i key_hash_name -> (
        let* n =
          trace_option (wrong_mini_c_value t v) @@
          get_string v in
        return (E_literal (Literal_key_hash n))
      )
    | (i, []) when String.equal i chain_id_name -> (
      let* n =
        trace_option (wrong_mini_c_value t v) @@
        get_string v in
      return (E_literal (Literal_chain_id n))
    )
    |  (i, []) when String.equal i signature_name -> (
      let* n =
        trace_option (wrong_mini_c_value t v) @@
        get_string v in
      return (E_literal (Literal_signature n))
    )
    | (i, [o]) when String.equal i option_name -> (
        let* opt =
          trace_option (wrong_mini_c_value t v) @@
          get_option v in
        match opt with
        | None -> ok (e_a_none o)
        | Some s ->
            let* s' = decompile s o in
            ok (e_a_some s')
      )
    | (i, [k_ty;v_ty]) when String.equal i map_name -> (
        let* map =
          trace_option (wrong_mini_c_value t v) @@
          get_map v in
        let* map' =
          let aux = fun (k, v) ->
            let* key   = decompile k k_ty in
            let* value = decompile v v_ty in
            ok ({key; value} : AST.map_kv) in
          bind_map_list aux map in
        let map' = List.dedup_and_sort ~compare map' in
        let aux = fun prev ({ key ; value } : AST.map_kv) ->
          return @@ E_constant {cons_name=C_MAP_ADD;arguments=[key ; value ; prev]}
        in
        let* init = return @@ E_constant {cons_name=C_MAP_EMPTY;arguments=[]} in
        bind_fold_right_list aux init map'
      )
    | (i, [k_ty; v_ty]) when String.equal i big_map_name -> (
        let* big_map =
          trace_option (wrong_mini_c_value t v) @@
          get_big_map v in
        let* big_map' =
          let aux = fun (k, v) ->
            let* key   = decompile k k_ty in
            let* value = decompile v v_ty in
            ok ({key; value} : AST.map_kv) in
          bind_map_list aux big_map in
        let big_map' = List.dedup_and_sort ~compare big_map' in
        let aux = fun prev ({ key ; value } : AST.map_kv) ->
          return @@ E_constant {cons_name=C_MAP_ADD;arguments=[key ; value ; prev]}
        in
        let* init = return @@ E_constant {cons_name=C_BIG_MAP_EMPTY;arguments=[]} in
        bind_fold_right_list aux init big_map'
      )
    | (i, _) when String.equal i map_or_big_map_name -> fail @@ corner_case ~loc:"unspiller" "TC_map_or_big_map t should not be present in mini-c"
    | (i, [ty]) when String.equal i list_name -> (
        let* lst =
          trace_option (wrong_mini_c_value t v) @@
          get_list v in
        let* lst' =
          let aux = fun e -> decompile e ty in
          bind_map_list aux lst in
        let aux = fun prev cur ->
          return @@ E_constant {cons_name=C_CONS;arguments=[cur ; prev]} in
        let* init  = return @@ E_constant {cons_name=C_LIST_EMPTY;arguments=[]} in
        bind_fold_right_list aux init lst'
      )
    | (i, [ty]) when String.equal i set_name -> (
        let* lst =
          trace_option (wrong_mini_c_value t v) @@
          get_set v in
        let* lst' =
          let aux = fun e -> decompile e ty in
          bind_map_list aux lst in
        let lst' = List.dedup_and_sort ~compare lst' in
        let aux = fun prev cur ->
          return @@ E_constant {cons_name=C_SET_ADD;arguments=[cur ; prev]} in
        let* init = return @@ E_constant {cons_name=C_SET_EMPTY;arguments=[]} in
        bind_fold_list aux init lst'
      )
    | (i, [ty]) when String.equal i ticket_name -> (
      let* (v,amt) =
        trace_option (wrong_mini_c_value t v) @@
        get_ticket v
      in
      let* v = decompile v ty in
      let* amt = decompile amt (Ast_typed.t_nat ()) in
      return (E_constant {cons_name=C_TICKET;arguments=[v;amt]})
    )
    | (i, _) when String.equal i contract_name ->
      fail @@ bad_decompile v
    | (i,_) when List.exists ~f:(fun el ->String.equal i el) [michelson_pair_name ; michelson_or_name; michelson_pair_left_comb_name ; michelson_pair_right_comb_name ; michelson_or_left_comb_name ; michelson_or_right_comb_name ] ->
      fail @@ corner_case ~loc:"unspiller" "Michelson_combs t should not be present in mini-c"
    | _ ->
      (* let () = Format.printf "%a" Mini_c.PP.value v in *)
      let () = Format.printf "%a" Ast_typed.PP.type_content t.type_content in
      fail @@ corner_case ~loc:"unspiller" "Wrong number of args or wrong kinds for the type constant"
  )
  | T_sum {layout ; content} ->
      let lst = List.map ~f:(fun (k,({associated_type;_} : _ row_element_mini_c)) -> (k,associated_type)) @@ Ast_typed.Helpers.kv_list_of_t_sum ~layout content in
      let* (constructor, v, tv) = Layout.extract_constructor ~layout v lst in
      let* sub = decompile v tv in
      return (E_constructor {constructor;element=sub})
  | T_record {layout ; content } ->
      let lst = List.map ~f:(fun (k,({associated_type;_} : _ row_element_mini_c)) -> (k,associated_type)) @@ Ast_typed.Helpers.kv_list_of_t_record_or_tuple ~layout content in
      let* lst = Layout.extract_record ~layout v lst in
      let* lst = bind_list
        @@ List.map ~f:(fun (x, (y, z)) -> let* yz = decompile y z in ok (x, yz)) lst in
      let m' = AST.LMap.of_list lst in
      return (E_record m')
  | T_arrow _ ->
      let* n =
        trace_option (wrong_mini_c_value t v) @@
        get_string v in
      let n = Ligo_string.Standard n in
      return (E_literal (Literal_string n))
  | T_variable _ ->
    fail @@ corner_case ~loc:__LOC__ "trying to decompile at variable type"
  | T_module_accessor _ ->
    fail @@ corner_case ~loc:__LOC__ "trying to decompile at module access type"
  | T_singleton _ ->
    fail @@ corner_case ~loc:__LOC__ "no value is of type singleton"
