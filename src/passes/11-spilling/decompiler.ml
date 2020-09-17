open Helpers

module AST = Ast_typed
module Append_tree = Tree.Append
module Errors = Errors
open Errors
open Mini_c
open Trace

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
  | T_constant {type_constant;arguments} -> (
    match type_constant,arguments with
    | TC_unit, [] -> (
        let%bind () =
          trace_option (wrong_mini_c_value t v) @@
          get_unit v in
        return (E_literal Literal_unit)
      )
    | TC_int, [] -> (
        let%bind n =
          trace_option (wrong_mini_c_value t v) @@
          get_int v in
        return (E_literal (Literal_int n))
      )
    | TC_nat, [] -> (
        let%bind n =
          trace_option (wrong_mini_c_value t v) @@
          get_nat v in
        return (E_literal (Literal_nat n))
      )
    | TC_timestamp, [] -> (
        let%bind n =
          trace_option (wrong_mini_c_value t v) @@
          get_timestamp v in
        return (E_literal (Literal_timestamp n))
      )
    | TC_mutez, [] -> (
        let%bind n =
          trace_option (wrong_mini_c_value t v) @@
          get_mutez v in
        return (E_literal (Literal_mutez n))
      )
    | TC_string, [] -> (
        let%bind n =
          trace_option (wrong_mini_c_value t v) @@
          get_string v in
        let n = Ligo_string.Standard n in
        return (E_literal (Literal_string n))
      )
    | TC_bytes, [] -> (
        let%bind n =
          trace_option (wrong_mini_c_value t v) @@
          get_bytes v in
        return (E_literal (Literal_bytes n))
      )
    | TC_address, [] -> (
        let%bind n =
          trace_option (wrong_mini_c_value t v) @@
          get_string v in
        return (E_literal (Literal_address n))
      )
    | TC_operation, [] -> (
        let%bind op =
          trace_option (wrong_mini_c_value t v) @@
          get_operation v in
        return (E_literal (Literal_operation op))
      )
    |  TC_key, [] -> (
        let%bind n =
          trace_option (wrong_mini_c_value t v) @@
          get_string v in
        return (E_literal (Literal_key n))
      )
    |  TC_key_hash, [] -> (
        let%bind n =
          trace_option (wrong_mini_c_value t v) @@
          get_string v in
        return (E_literal (Literal_key_hash n))
      )
    | TC_chain_id, [] -> (
      let%bind n =
        trace_option (wrong_mini_c_value t v) @@
        get_string v in
      return (E_literal (Literal_chain_id n))
    )
    |  TC_signature, [] -> (
      let%bind n =
        trace_option (wrong_mini_c_value t v) @@
        get_string v in
      return (E_literal (Literal_signature n))
    )
    | TC_option, [o] -> (
        let%bind opt =
          trace_option (wrong_mini_c_value t v) @@
          get_option v in
        match opt with
        | None -> ok (e_a_none o)
        | Some s ->
            let%bind s' = decompile s o in
            ok (e_a_some s')
      )
    | TC_map, [k_ty;v_ty] -> (
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
    | TC_big_map, [k_ty; v_ty] -> (
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
    | TC_map_or_big_map, _ -> fail @@ corner_case ~loc:"unspiller" "TC_map_or_big_map t should not be present in mini-c"
    | TC_list, [ty] -> (
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
    | TC_set, [ty] -> (
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
    | TC_contract, _ ->
      fail @@ bad_decompile v
    | (TC_michelson_pair|TC_michelson_or|TC_michelson_pair_right_comb|TC_michelson_pair_left_comb|TC_michelson_or_right_comb| TC_michelson_or_left_comb), _ -> 
      fail @@ corner_case ~loc:"unspiller" "Michelson_combs t should not be present in mini-c"
    | _ -> 
      fail @@ corner_case ~loc:"unspiller" "Wrong number of args or wrong kinds for the type constant"
  )
  | T_sum m ->
      let lst = List.map (fun (k,{associated_type;_}) -> (k,associated_type)) @@ kv_list_of_lmap m in
      let%bind node = match Append_tree.of_list lst with
        | Empty -> fail @@ corner_case ~loc:__LOC__ "empty sum type"
        | Full t -> ok t
      in
      let%bind (name, v, tv) =
        trace_strong (corner_case ~loc:__LOC__ "sum extract constructor") @@
        extract_constructor v node in
      let%bind sub = decompile v tv in
      return (E_constructor {constructor=Label name;element=sub})
  | T_record m ->
      let lst = List.map (fun (k,{associated_type;_}) -> (k,associated_type)) @@ Ast_typed.Helpers.kv_list_of_record_or_tuple m in
      let%bind node = match Append_tree.of_list lst with
        | Empty -> fail @@ corner_case ~loc:__LOC__ "empty record"
        | Full t -> ok t in
      let%bind lst =
        trace_strong (corner_case ~loc:__LOC__ "record extract") @@
        extract_record v node in
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
  | T_wildcard ->
    fail @@ corner_case ~loc:__LOC__ "trying to decompile a wildcard type"
