open Helpers

module AST = Ast_typed
module Append_tree = Tree.Append
open Mini_c
open Trace

module Errors = struct

  let corner_case ~loc message =
    let title () = "corner case" in
    let content () = "we don't have a good error message for this case. we are
striving find ways to better report them and find the use-cases that generate
them. please report this to the developers." in
    let data = [
      ("location" , fun () -> loc) ;
      ("message" , fun () -> message) ;
    ] in
    error ~data title content

  let wrong_mini_c_value expected_type actual =
    let title () = "illed typed intermediary value" in
    let content () = "type of intermediary value doesn't match what was expected" in
    let data = [
      ("expected_type" , fun () -> expected_type) ;
      ("actual" , fun () -> Format.asprintf "%a" Mini_c.PP.value actual ) ;
    ] in
    error ~data title content

  let bad_untranspile bad_type value =
    let title () = "untranspiling bad value" in
    let content () = Format.asprintf "can not untranspile %s" bad_type in
    let data = [
      ("bad_type" , fun () -> bad_type) ;
      ("value" , fun () -> Format.asprintf "%a" Mini_c.PP.value value) ;
    ] in
    error ~data title content

end

open Errors

let rec untranspile (v : value) (t : AST.type_expression) : AST.expression result =
  let open! AST in
  let return e = ok (make_a_e_empty e t) in
  match t.type_content with
  | T_constant type_constant -> (
    match type_constant with
    | TC_unit -> (
        let%bind () =
          trace_strong (wrong_mini_c_value "unit" v) @@
          get_unit v in
        return (E_literal Literal_unit)
      )
    | TC_bool -> (
        let%bind b =
          trace_strong (wrong_mini_c_value "bool" v) @@
          get_bool v in
        return (E_literal (Literal_bool b))
      )
    | TC_int -> (
        let%bind n =
          trace_strong (wrong_mini_c_value "int" v) @@
          get_int v in
        return (E_literal (Literal_int n))
      )
    | TC_nat -> (
        let%bind n =
          trace_strong (wrong_mini_c_value "nat" v) @@
          get_nat v in
        return (E_literal (Literal_nat n))
      )
    | TC_timestamp -> (
        let%bind n =
          trace_strong (wrong_mini_c_value "timestamp" v) @@
          get_timestamp v in
        return (E_literal (Literal_timestamp n))
      )
    | TC_mutez -> (
        let%bind n =
          trace_strong (wrong_mini_c_value "tez" v) @@
          get_mutez v in
        return (E_literal (Literal_mutez n))
      )
    | TC_string -> (
        let%bind n =
          trace_strong (wrong_mini_c_value "string" v) @@
          get_string v in
        return (E_literal (Literal_string n))
      )
    | TC_bytes -> (
        let%bind n =
          trace_strong (wrong_mini_c_value "bytes" v) @@
          get_bytes v in
        return (E_literal (Literal_bytes n))
      )
    | TC_address -> (

        let%bind n =
          trace_strong (wrong_mini_c_value "address" v) @@
          get_string v in
        return (E_literal (Literal_address n))
      )
    | TC_operation -> (
        let%bind op =
          trace_strong (wrong_mini_c_value "operation" v) @@
          get_operation v in
        return (E_literal (Literal_operation op))
      )
    |  TC_key -> (
        let%bind n =
          trace_strong (wrong_mini_c_value "key" v) @@
          get_string v in
        return (E_literal (Literal_key n))
      )
    |  TC_key_hash -> (
        let%bind n =
          trace_strong (wrong_mini_c_value "key_hash" v) @@
          get_string v in
        return (E_literal (Literal_key_hash n))
      )
    | TC_chain_id -> (
      let%bind n =
        trace_strong (wrong_mini_c_value "chain_id" v) @@
        get_string v in
      return (E_literal (Literal_chain_id n))
    )
    | TC_void -> (
      let%bind () =
        trace_strong (wrong_mini_c_value "void" v) @@
        get_unit v in
      return (E_literal (Literal_void))
    )
    |  TC_signature -> (
      let%bind n =
        trace_strong (wrong_mini_c_value "signature" v) @@
        get_string v in
      return (E_literal (Literal_signature n))
    )
  )
  | T_operator type_operator -> (
    match type_operator with
    | TC_option o -> (
        let%bind opt =
          trace_strong (wrong_mini_c_value "option" v) @@
          get_option v in
        match opt with
        | None -> ok (e_a_empty_none o)
        | Some s ->
            let%bind s' = untranspile s o in
            ok (e_a_empty_some s')
      )
    | TC_map {k=k_ty;v=v_ty}-> (
        let%bind map =
          trace_strong (wrong_mini_c_value "map" v) @@
          get_map v in
        let%bind map' =
          let aux = fun (k, v) ->
            let%bind k = untranspile k k_ty in
            let%bind v = untranspile v v_ty in
            ok ({k; v} : AST.map_kv) in
          bind_map_list aux map in
        let map' = List.sort_uniq compare map' in
        let aux = fun prev ({ k ; v } : AST.map_kv) ->
          return @@ E_constant {cons_name=C_MAP_ADD;arguments=[k ; v ; prev]}
        in
        let%bind init = return @@ E_constant {cons_name=C_MAP_EMPTY;arguments=[]} in
        bind_fold_right_list aux init map'
      )
    | TC_big_map {k=k_ty; v=v_ty} -> (
        let%bind big_map =
          trace_strong (wrong_mini_c_value "big_map" v) @@
          get_big_map v in
        let%bind big_map' =
          let aux = fun (k, v) ->
            let%bind k = untranspile k k_ty in
            let%bind v = untranspile v v_ty in
            ok ({k; v} : AST.map_kv) in
          bind_map_list aux big_map in
        let big_map' = List.sort_uniq compare big_map' in
        let aux = fun prev ({ k ; v } : AST.map_kv) ->
          return @@ E_constant {cons_name=C_MAP_ADD;arguments=[k ; v ; prev]}
        in
        let%bind init = return @@ E_constant {cons_name=C_BIG_MAP_EMPTY;arguments=[]} in
        bind_fold_right_list aux init big_map'
      )
    | TC_map_or_big_map _ -> fail @@ corner_case ~loc:"untranspiler" "TC_map_or_big_map t should not be present in mini-c"
    | TC_list ty -> (
        let%bind lst =
          trace_strong (wrong_mini_c_value "list" v) @@
          get_list v in
        let%bind lst' =
          let aux = fun e -> untranspile e ty in
          bind_map_list aux lst in
        let aux = fun prev cur ->
          return @@ E_constant {cons_name=C_CONS;arguments=[cur ; prev]} in
        let%bind init  = return @@ E_constant {cons_name=C_LIST_EMPTY;arguments=[]} in
        bind_fold_right_list aux init lst'
      )
    | TC_arrow _ -> (
        let%bind n =
          trace_strong (wrong_mini_c_value "lambda as string" v) @@
          get_string v in
        return (E_literal (Literal_string n))
      )
    | TC_set ty -> (
        let%bind lst =
          trace_strong (wrong_mini_c_value "set" v) @@
          get_set v in
        let%bind lst' =
          let aux = fun e -> untranspile e ty in
          bind_map_list aux lst in
        let lst' = List.sort_uniq compare lst' in
        let aux = fun prev cur ->
          return @@ E_constant {cons_name=C_SET_ADD;arguments=[cur ; prev]} in
        let%bind init = return @@ E_constant {cons_name=C_SET_EMPTY;arguments=[]} in
        bind_fold_list aux init lst'
      )
    | TC_contract _ ->
      fail @@ bad_untranspile "contract" v
  )
  | T_sum m ->
      let lst = List.map (fun (k,{ctor_type;_}) -> (k,ctor_type)) @@ kv_list_of_cmap m in
      let%bind node = match Append_tree.of_list lst with
        | Empty -> fail @@ corner_case ~loc:__LOC__ "empty sum type"
        | Full t -> ok t
      in
      let%bind (name, v, tv) =
        trace_strong (corner_case ~loc:__LOC__ "sum extract constructor") @@
        extract_constructor v node in
      let%bind sub = untranspile v tv in
      return (E_constructor {constructor=Constructor name;element=sub})
  | T_record m ->
      let lst = List.map (fun (k,{field_type;_}) -> (k,field_type)) @@ Ast_typed.Helpers.kv_list_of_record_or_tuple m in
      let%bind node = match Append_tree.of_list lst with
        | Empty -> fail @@ corner_case ~loc:__LOC__ "empty record"
        | Full t -> ok t in
      let%bind lst =
        trace_strong (corner_case ~loc:__LOC__ "record extract") @@
        extract_record v node in
      let%bind lst = bind_list
        @@ List.map (fun (x, (y, z)) -> let%bind yz = untranspile y z in ok (x, yz)) lst in
      let m' = AST.LMap.of_list lst in
      return (E_record m')
  | T_arrow _ ->
      let%bind n =
        trace_strong (wrong_mini_c_value "lambda as string" v) @@
        get_string v in
      return (E_literal (Literal_string n))
  | T_variable _ ->
    fail @@ corner_case ~loc:__LOC__ "trying to untranspile at variable type"
