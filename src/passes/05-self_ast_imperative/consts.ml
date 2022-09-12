open Helpers
open Errors
open Ast_imperative
open Simple_utils.Trace
open Ligo_prim

let add_binder b vars =
  let var  = Binder.get_var b in
  let vars = remove_from var vars in
  if not @@ Binder.is_mutable b then var :: vars else vars

let rec assign_expression ~raise : ?vars:Value_var.t list -> expression -> expression  = fun ?(vars = []) e ->
  let self = assign_expression ~raise in
  let _ = fold_map_expression
                (fun (vars : Value_var.t list) expr ->
                  match expr.expression_content with
                  | E_assign {binder;expression=_} -> (
                    let var = Binder.get_var binder in
                    match List.find ~f:(Value_var.equal var) vars with
                    | Some (v:Value_var.t) ->
                      raise.error @@ const_assigned (Value_var.get_location v) var
                    | None -> (true, vars, expr)
                  )
                  | E_lambda {binder;output_type=_;result=_} ->
                    let vars = add_binder binder vars in
                    (true, vars, expr)
                  | E_let_in {let_binder;rhs;let_result;attributes=_} ->
                    let _ = self ~vars rhs in
                    let vars = add_binder let_binder vars in
                    let _ = self ~vars let_result in
                    (false, vars, expr)
                  | E_matching {matchee;cases} ->
                    let f Match_expr.{pattern;body} =
                      let all_pattern_vars = get_pattern pattern in
                      let vars = List.fold_right ~f:remove_from all_pattern_vars ~init:vars in
                      let const_pattern_vars = get_pattern ~pred:not pattern in
                      let vars =
                          List.fold_right ~f:(fun var vars -> var :: remove_from var vars) const_pattern_vars ~init:vars in
                      self ~vars body in
                    let _ = self ~vars matchee in
                    let _ = List.map ~f:f cases in
                    (false, vars, expr)
                  | E_recursive {lambda={binder;output_type=_;result=_};fun_name=_;fun_type=_} ->
                    let vars = add_binder binder vars in
                    (true, vars, expr)
                  | _  ->
                    (true, vars, expr)
                  )
                  vars e
  in
  e

let assign_expression ~raise : expression -> expression  =
  assign_expression ~raise
