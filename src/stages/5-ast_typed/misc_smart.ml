open Trace
open Types
open Misc
(* open Stage_common.Types *)

module Captured_variables = struct

  type bindings = expression_variable list
  let mem : expression_variable -> bindings -> bool = List.mem
  let singleton : expression_variable -> bindings = fun s -> [ s ]
  let union : bindings -> bindings -> bindings = (@)
  let unions : bindings list -> bindings = List.concat
  let empty : bindings = []
  let of_list : expression_variable list -> bindings = fun x -> x

  let rec expression : bindings -> expression -> (bindings,_) result = fun b e ->
    expression_content b e.expression_content
  and expression_content : bindings -> expression_content -> (bindings,_) result = fun b ec ->
    let self = expression b in
    match ec with
    | E_lambda l -> ok @@ Free_variables.lambda empty l
    | E_literal _ -> ok empty
    | E_constant {arguments;_} ->
      let%bind lst' = bind_map_list self arguments in
      ok @@ unions lst'
    | E_variable name -> (
      if mem name b then ok empty else ok (singleton name)
      )
    | E_application {lamb;args} ->
      let%bind lst' = bind_map_list self [ lamb ; args ] in
      ok @@ unions lst'
    | E_constructor {element;_} -> self element
    | E_record m ->
      let%bind lst' = bind_map_list self @@ LMap.to_list m in
      ok @@ unions lst'
    | E_record_accessor {record;_} -> self record
    | E_record_update {record;update;_} -> 
      let%bind r = self record in
      let%bind e = self update in
      ok @@ union r e
    | E_matching {matchee;cases;_} ->
      let%bind a' = self matchee in
      let%bind cs' = matching_expression b cases in
      ok @@ union a' cs'
    | E_let_in li ->
      let b' = union (singleton li.let_binder) b in
      expression b' li.let_result
    | E_raw_code _ -> ok empty
    | E_recursive r -> 
      let b' = union (singleton r.fun_name) b in
      expression_content b' @@ E_lambda r.lambda

  and matching_variant_case : (bindings -> expression -> (bindings,_) result) -> bindings -> matching_content_case -> (bindings,_) result  = fun f b { constructor=_ ; pattern ; body } ->
    f (union (singleton pattern) b) body

  and matching : (bindings -> expression -> (bindings,_) result) -> bindings -> matching_expr -> (bindings,_) result = fun f b m ->
    match m with
    | Match_list { match_nil = n ; match_cons = {hd; tl; body; tv=_} } ->
      let%bind n' = f b n in
      let%bind c' = f (union (of_list [hd ; tl]) b) body in
      ok @@ union n' c'
    | Match_option { match_none = n ; match_some = {opt; body; tv=_} } ->
      let%bind n' = f b n in
      let%bind s' = f (union (singleton opt) b) body in
      ok @@ union n' s'
    | Match_variant { cases ; tv=_ } ->
      let%bind lst' = bind_map_list (matching_variant_case f b) cases in
      ok @@ unions lst'

  and matching_expression = fun x -> matching expression x

end
