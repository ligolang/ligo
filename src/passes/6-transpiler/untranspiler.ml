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

  let unknown_untranspile unknown_type value =
    let title () = "untranspiling unknown value" in
    let content () = Format.asprintf "can not untranspile %s" unknown_type in
    let data = [
      ("unknown_type" , fun () -> unknown_type) ;
      ("value" , fun () -> Format.asprintf "%a" Mini_c.PP.value value) ;
    ] in
    error ~data title content

end

open Errors

let rec untranspile (v : value) (t : AST.type_value) : AST.annotated_expression result =
  let open! AST in
  let return e = ok (make_a_e_empty e t) in
  match t.type_value' with
  | T_constant ("unit", []) -> (
      let%bind () =
        trace_strong (wrong_mini_c_value "unit" v) @@
        get_unit v in
      return (E_literal Literal_unit)
    )
  | T_constant ("bool", []) -> (
      let%bind b =
        trace_strong (wrong_mini_c_value "bool" v) @@
        get_bool v in
      return (E_literal (Literal_bool b))
    )
  | T_constant ("int", []) -> (
      let%bind n =
        trace_strong (wrong_mini_c_value "int" v) @@
        get_int v in
      return (E_literal (Literal_int n))
    )
  | T_constant ("nat", []) -> (
      let%bind n =
        trace_strong (wrong_mini_c_value "nat" v) @@
        get_nat v in
      return (E_literal (Literal_nat n))
    )
  | T_constant ("timestamp", []) -> (
      let%bind n =
        trace_strong (wrong_mini_c_value "timestamp" v) @@
        get_timestamp v in
      return (E_literal (Literal_timestamp n))
    )
  | T_constant ("tez", []) -> (
      let%bind n =
        trace_strong (wrong_mini_c_value "tez" v) @@
        get_nat v in
      return (E_literal (Literal_tez n))
    )
  | T_constant ("string", []) -> (
      let%bind n =
        trace_strong (wrong_mini_c_value "string" v) @@
        get_string v in
      return (E_literal (Literal_string n))
    )
  | T_constant ("bytes", []) -> (
      let%bind n =
        trace_strong (wrong_mini_c_value "bytes" v) @@
        get_bytes v in
      return (E_literal (Literal_bytes n))
    )
  | T_constant ("address", []) -> (
      let%bind n =
        trace_strong (wrong_mini_c_value "address" v) @@
        get_string v in
      return (E_literal (Literal_address n))
    )
  | T_constant ("option", [o]) -> (
      let%bind opt =
        trace_strong (wrong_mini_c_value "option" v) @@
        get_option v in
      match opt with
      | None -> ok (e_a_empty_none o)
      | Some s ->
          let%bind s' = untranspile s o in
          ok (e_a_empty_some s')
    )
  | T_constant ("map", [k_ty;v_ty]) -> (
      let%bind lst =
        trace_strong (wrong_mini_c_value "map" v) @@
        get_map v in
      let%bind lst' =
        let aux = fun (k, v) ->
          let%bind k' = untranspile k k_ty in
          let%bind v' = untranspile v v_ty in
          ok (k', v') in
        bind_map_list aux lst in
      return (E_map lst')
    )
  | T_constant ("big_map", [k_ty;v_ty]) -> (
      let%bind lst =
        trace_strong (wrong_mini_c_value "big_map" v) @@
        get_big_map v in
      let%bind lst' =
        let aux = fun (k, v) ->
          let%bind k' = untranspile k k_ty in
          let%bind v' = untranspile v v_ty in
          ok (k', v') in
        bind_map_list aux lst in
      return (E_big_map lst')
    )
  | T_constant ("list", [ty]) -> (
      let%bind lst =
        trace_strong (wrong_mini_c_value "list" v) @@
        get_list v in
      let%bind lst' =
        let aux = fun e -> untranspile e ty in
        bind_map_list aux lst in
      return (E_list lst')
    )
  | T_constant ("set", [ty]) -> (
      let%bind lst =
        trace_strong (wrong_mini_c_value "set" v) @@
        get_set v in
      let%bind lst' =
        let aux = fun e -> untranspile e ty in
        bind_map_list aux lst in
      return (E_set lst')
    )
  | T_constant ("contract" , [_ty]) ->
    fail @@ bad_untranspile "contract" v
  | T_constant ("operation" , []) -> (
      let%bind op =
        trace_strong (wrong_mini_c_value "operation" v) @@
        get_operation v in
      return (E_literal (Literal_operation op))
    )
  | T_constant (name , _lst) ->
    fail @@ unknown_untranspile name v
  | T_sum m ->
      let lst = kv_list_of_map m in
      let%bind node = match Append_tree.of_list lst with
        | Empty -> fail @@ corner_case ~loc:__LOC__ "empty sum type"
        | Full t -> ok t
      in
      let%bind (name, v, tv) =
        trace_strong (corner_case ~loc:__LOC__ "sum extract constructor") @@
        extract_constructor v node in
      let%bind sub = untranspile v tv in
      return (E_constructor (name, sub))
  | T_tuple lst ->
      let%bind node = match Append_tree.of_list lst with
        | Empty -> fail @@ corner_case ~loc:__LOC__ "empty tuple"
        | Full t -> ok t in
      let%bind tpl =
        trace_strong (corner_case ~loc:__LOC__ "tuple extract") @@
        extract_tuple v node in
      let%bind tpl' = bind_list
        @@ List.map (fun (x, y) -> untranspile x y) tpl in
      return (E_tuple tpl')
  | T_record m ->
      let lst = kv_list_of_map m in
      let%bind node = match Append_tree.of_list lst with
        | Empty -> fail @@ corner_case ~loc:__LOC__ "empty record"
        | Full t -> ok t in
      let%bind lst =
        trace_strong (corner_case ~loc:__LOC__ "record extract") @@
        extract_record v node in
      let%bind lst = bind_list
        @@ List.map (fun (x, (y, z)) -> let%bind yz = untranspile y z in ok (x, yz)) lst in
      let m' = map_of_kv_list lst in
      return (E_record m')
  | T_function _ -> fail @@ bad_untranspile "function" v
