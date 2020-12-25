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
  | tc when (compare tc (t_bool ()).type_content) = 0-> (
        let%bind b =
          trace_option (wrong_mini_c_value t v) @@
          get_bool v in
        return (e_bool b)
      )
  | T_constant { language; injection; parameters } -> (
    let%bind () = Assert.assert_true
      (corner_case ~loc:__LOC__ ("unsupported language "^language))
      (String.equal language Stage_common.Backends.michelson)
    in
    match (Ligo_string.extract injection,parameters) with
    | (i, []) when String.equal i unit_name -> (
        let%bind () =
          trace_option (wrong_mini_c_value t v) @@
          get_unit v in
        return (E_literal Literal_unit)
      )
    | (i, []) when String.equal i int_name -> (
        let%bind n =
          trace_option (wrong_mini_c_value t v) @@
          get_int v in
        return (E_literal (Literal_int n))
      )
    | (i, []) when String.equal i nat_name -> (
        let%bind n =
          trace_option (wrong_mini_c_value t v) @@
          get_nat v in
        return (E_literal (Literal_nat n))
      )
    | (i, []) when String.equal i timestamp_name -> (
        let%bind n =
          trace_option (wrong_mini_c_value t v) @@
          get_timestamp v in
        return (E_literal (Literal_timestamp n))
      )
    | (i, []) when String.equal i tez_name -> (
        let%bind n =
          trace_option (wrong_mini_c_value t v) @@
          get_mutez v in
        return (E_literal (Literal_mutez n))
      )
    | (i, []) when String.equal i string_name -> (
        let%bind n =
          trace_option (wrong_mini_c_value t v) @@
          get_string v in
        let n = Ligo_string.Standard n in
        return (E_literal (Literal_string n))
      )
    | (i, []) when String.equal i bytes_name -> (
        let%bind n =
          trace_option (wrong_mini_c_value t v) @@
          get_bytes v in
        return (E_literal (Literal_bytes n))
      )
    | (i, []) when String.equal i address_name -> (
        let%bind n =
          trace_option (wrong_mini_c_value t v) @@
          get_string v in
        return (E_literal (Literal_address n))
      )
    | (i, []) when String.equal i operation_name -> (
        let%bind op =
          trace_option (wrong_mini_c_value t v) @@
          get_operation v in
        return (E_literal (Literal_operation op))
      )
    |  (i, []) when String.equal i key_name -> (
        let%bind n =
          trace_option (wrong_mini_c_value t v) @@
          get_string v in
        return (E_literal (Literal_key n))
      )
    |  (i, []) when String.equal i key_hash_name -> (
        let%bind n =
          trace_option (wrong_mini_c_value t v) @@
          get_string v in
        return (E_literal (Literal_key_hash n))
      )
    | (i, []) when String.equal i chain_id_name -> (
      let%bind n =
        trace_option (wrong_mini_c_value t v) @@
        get_string v in
      return (E_literal (Literal_chain_id n))
    )
    |  (i, []) when String.equal i signature_name -> (
      let%bind n =
        trace_option (wrong_mini_c_value t v) @@
        get_string v in
      return (E_literal (Literal_signature n))
    )
    | (i, [o]) when String.equal i option_name -> (
        let%bind opt =
          trace_option (wrong_mini_c_value t v) @@
          get_option v in
        match opt with
        | None -> ok (e_a_none o)
        | Some s ->
            let%bind s' = decompile s o in
            ok (e_a_some s')
      )
    | (i, [k_ty;v_ty]) when String.equal i map_name -> (
        let%bind map =
          trace_option (wrong_mini_c_value t v) @@
          get_map v in
        let%bind map' =
          let aux = fun (k, v) ->
            let%bind key   = decompile k k_ty in
            let%bind value = decompile v v_ty in
            ok ({key; value} : AST.map_kv) in
          bind_map_list aux map in
        let map' = List.sort_uniq compare map' in
        let aux = fun prev ({ key ; value } : AST.map_kv) ->
          return @@ E_constant {cons_name=C_MAP_ADD;arguments=[key ; value ; prev]}
        in
        let%bind init = return @@ E_constant {cons_name=C_MAP_EMPTY;arguments=[]} in
        bind_fold_right_list aux init map'
      )
    | (i, [k_ty; v_ty]) when String.equal i big_map_name -> (
        let%bind big_map =
          trace_option (wrong_mini_c_value t v) @@
          get_big_map v in
        let%bind big_map' =
          let aux = fun (k, v) ->
            let%bind key   = decompile k k_ty in
            let%bind value = decompile v v_ty in
            ok ({key; value} : AST.map_kv) in
          bind_map_list aux big_map in
        let big_map' = List.sort_uniq compare big_map' in
        let aux = fun prev ({ key ; value } : AST.map_kv) ->
          return @@ E_constant {cons_name=C_MAP_ADD;arguments=[key ; value ; prev]}
        in
        let%bind init = return @@ E_constant {cons_name=C_BIG_MAP_EMPTY;arguments=[]} in
        bind_fold_right_list aux init big_map'
      )
    | (i, _) when String.equal i map_or_big_map_name -> fail @@ corner_case ~loc:"unspiller" "TC_map_or_big_map t should not be present in mini-c"
    | (i, [ty]) when String.equal i list_name -> (
        let%bind lst =
          trace_option (wrong_mini_c_value t v) @@
          get_list v in
        let%bind lst' =
          let aux = fun e -> decompile e ty in
          bind_map_list aux lst in
        let aux = fun prev cur ->
          return @@ E_constant {cons_name=C_CONS;arguments=[cur ; prev]} in
        let%bind init  = return @@ E_constant {cons_name=C_LIST_EMPTY;arguments=[]} in
        bind_fold_right_list aux init lst'
      )
    | (i, [ty]) when String.equal i set_name -> (
        let%bind lst =
          trace_option (wrong_mini_c_value t v) @@
          get_set v in
        let%bind lst' =
          let aux = fun e -> decompile e ty in
          bind_map_list aux lst in
        let lst' = List.sort_uniq compare lst' in
        let aux = fun prev cur ->
          return @@ E_constant {cons_name=C_SET_ADD;arguments=[cur ; prev]} in
        let%bind init = return @@ E_constant {cons_name=C_SET_EMPTY;arguments=[]} in
        bind_fold_list aux init lst'
      )
    | (i, _) when String.equal i contract_name ->
      fail @@ bad_decompile v
    | (i,_) when List.exists (fun el ->String.equal i el) [michelson_pair_name ; michelson_or_name; michelson_pair_left_comb_name ; michelson_pair_right_comb_name ; michelson_or_left_comb_name ; michelson_or_right_comb_name ] ->
      fail @@ corner_case ~loc:"unspiller" "Michelson_combs t should not be present in mini-c"
    | _ ->
      fail @@ corner_case ~loc:"unspiller" "Wrong number of args or wrong kinds for the type constant"
  )
  | T_sum {layout ; content} ->
      let lst = List.map (fun (k,({associated_type;_} : _ row_element_mini_c)) -> (k,associated_type)) @@ Ast_typed.Helpers.kv_list_of_t_sum ~layout content in
      let%bind (constructor, v, tv) = Layout.extract_constructor ~layout v lst in
      let%bind sub = decompile v tv in
      return (E_constructor {constructor;element=sub})
  | T_record {layout ; content } ->
      let lst = List.map (fun (k,({associated_type;_} : _ row_element_mini_c)) -> (k,associated_type)) @@ Ast_typed.Helpers.kv_list_of_t_record_or_tuple ~layout content in
      let%bind lst = Layout.extract_record ~layout v lst in
      let%bind lst = bind_list
        @@ List.map (fun (x, (y, z)) -> let%bind yz = decompile y z in ok (x, yz)) lst in
      let m' = AST.LMap.of_list lst in
      return (E_record m')
  | T_arrow _ ->
      let%bind n =
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
