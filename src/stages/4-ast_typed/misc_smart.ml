open Trace
open Types
open Combinators
open Misc
(* open Stage_common.Types *)

let program_to_main : program -> string -> lambda result = fun p s ->
  let%bind (main , input_type , _) =
    let pred = fun d ->
      match d with
      | Declaration_constant { binder; expr; inline=_ } when binder = Var.of_name s -> Some expr
      | Declaration_constant _ -> None
      | Declaration_type _ -> None
    in
    let%bind main =
      trace_option (simple_error "no main with given name") @@
      List.find_map (Function.compose pred Location.unwrap) p in
    let%bind (input_ty , output_ty) =
      match (get_type' @@ get_type_expression main) with
      | T_arrow {type1;type2} -> ok (type1 , type2)
      | _ -> simple_fail "program main isn't a function" in
    ok (main , input_ty , output_ty)
  in
  let binder = Var.of_name "@contract_input" in
  let result =
    let input_expr = e_a_variable binder input_type in
    let main_expr = e_a_variable (Var.of_name s) (get_type_expression main) in
    e_a_application main_expr input_expr in
  ok {
    binder ;
    result ;
  }

module Captured_variables = struct

  type bindings = expression_variable list
  let mem : expression_variable -> bindings -> bool = List.mem
  let singleton : expression_variable -> bindings = fun s -> [ s ]
  let union : bindings -> bindings -> bindings = (@)
  let unions : bindings list -> bindings = List.concat
  let empty : bindings = []
  let of_list : expression_variable list -> bindings = fun x -> x

  let rec expression : bindings -> expression -> bindings result = fun b e ->
    expression_content b e.expression_content
  and expression_content : bindings -> expression_content -> bindings result = fun b ec ->
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
    | E_recursive r -> 
      let b' = union (singleton r.fun_name) b in
      expression_content b' @@ E_lambda r.lambda

  and matching_variant_case : (bindings -> expression -> bindings result) -> bindings -> matching_content_case -> bindings result  = fun f b { constructor=_ ; pattern ; body } ->
    f (union (singleton pattern) b) body

  and matching : (bindings -> expression -> bindings result) -> bindings -> matching_expr -> bindings result = fun f b m ->
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
