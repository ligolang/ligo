open Helpers
open Errors
open Ast_imperative
open Simple_utils.Trace

let is_const = fun x -> match x with
                      | { const_or_var = Some `Const } -> true
                      | _ -> false

let add_binder b var vars =
  let vars = remove_from var vars in
  if b then var :: vars else vars

let rec assign_expression ~raise : ?vars:expression_variable list -> expression -> expression  = fun ?(vars = []) e ->
  let self = assign_expression ~raise in
  let _ = fold_map_expression
                (fun (vars : expression_variable list) expr ->
                  match expr.expression_content with
                  | E_assign {binder={var;_};expression=_;access_path=_} -> (
                    match List.find ~f:(fun v -> ValueVar.equal var v) vars with
                    | Some (v:expression_variable) ->
                      raise.error @@ const_assigned (ValueVar.get_location v) var
                    | None -> (true, vars, expr)
                  )
                  | E_lambda {binder={var;ascr=_;attributes};output_type=_;result=_} ->
                    let vars = add_binder (is_const attributes) var vars in
                    (true, vars, expr)
                  | E_let_in {let_binder={var;ascr=_;attributes};rhs;let_result;attributes=_} ->
                    let _ = self ~vars rhs in
                    let vars = add_binder (is_const attributes) var vars in
                    let _ = self ~vars let_result in
                    (false, vars, expr)
                  | E_matching {matchee;cases} ->
                    let f {pattern;body} =
                      let all_pattern_vars = get_pattern pattern in
                      let vars = List.fold_right ~f:remove_from all_pattern_vars ~init:vars in
                      let const_pattern_vars = get_pattern ~pred:is_const pattern in
                      let vars =
                        List.fold_right ~f:(add_binder true) const_pattern_vars ~init:vars in
                      self ~vars body in
                    let _ = self ~vars matchee in
                    let _ = List.map ~f:f cases in
                    (false, vars, expr)
                  | E_recursive {lambda={binder={var;ascr=_;attributes};output_type=_;result=_};fun_name=_;fun_type=_} ->
                    let vars = add_binder (is_const attributes) var vars in
                    (true, vars, expr)
                  | _  ->
                    (true, vars, expr)
                  )
                  vars e
  in
  e

let assign_expression ~raise : expression -> expression  =
  assign_expression ~raise
