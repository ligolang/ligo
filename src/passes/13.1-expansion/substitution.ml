open Ligo_prim
module O = Ast_expanded

(**
  `substitute_var_in_body to_subst new_var body` replaces variables equal to `to_subst` with variable `new_var` in expression `body`.
  note that `new_var` here is never a user variable (always previously generated by the compiler)
**)
let rec substitute_var_in_body
    : to_subst:Value_var.t -> new_var:Value_var.t -> O.expression -> O.expression
  =
 fun ~to_subst ~new_var body ->
  let aux : unit -> O.expression -> bool * unit * O.expression =
   fun () exp ->
    let ret continue exp = continue, (), exp in
    match exp.expression_content with
    | O.E_deref var when Value_var.equal var to_subst ->
      ret true { exp with expression_content = E_deref new_var }
    | O.E_variable var when Value_var.equal var to_subst ->
      ret true { exp with expression_content = E_variable new_var }
    | O.E_assign { binder; expression }
      when Value_var.equal (Binder.get_var binder) to_subst ->
      let binder = Binder.set_var binder new_var in
      let expression = substitute_var_in_body ~to_subst ~new_var expression in
      ret true { exp with expression_content = E_assign { binder; expression } }
    | O.E_let_in letin when Binder.apply (Value_var.equal to_subst) letin.let_binder ->
      let rhs = substitute_var_in_body ~to_subst ~new_var letin.rhs in
      let letin = { letin with rhs } in
      ret false { exp with expression_content = E_let_in letin }
    | O.E_let_mut_in letin when Binder.apply (Value_var.equal to_subst) letin.let_binder
      ->
      let rhs = substitute_var_in_body ~to_subst ~new_var letin.rhs in
      let letin = { letin with rhs } in
      ret false { exp with expression_content = E_let_mut_in letin }
    | O.E_lambda lamb when Value_var.equal to_subst (Param.get_var lamb.binder) ->
      ret false exp
    | O.E_recursive r when Value_var.equal r.fun_name to_subst -> ret false exp
    | O.E_matching m ->
      let matchee = substitute_var_in_body ~to_subst ~new_var m.matchee in
      let cases =
        match m.cases with
        | Match_record { fields; body; tv } ->
          if Record.exists
               ~f:(fun (b : O.type_expression Binder.t) ->
                 Binder.apply (Value_var.equal to_subst) b)
               fields
          then m.cases
          else (
            let body = substitute_var_in_body ~to_subst ~new_var body in
            Match_record { fields; body; tv })
        | Match_variant { cases; tv } ->
          let cases =
            List.fold_right cases ~init:[] ~f:(fun case cases ->
                if Value_var.equal case.pattern to_subst
                then case :: cases
                else (
                  let body = substitute_var_in_body ~to_subst ~new_var case.body in
                  { case with body } :: cases))
          in
          Match_variant { cases; tv }
      in
      ret false { exp with expression_content = O.E_matching { matchee; cases } }
    | E_literal _
    | E_constant _
    | E_variable _
    | E_application _
    | E_lambda _
    | E_assign _
    | E_let_mut_in _
    | E_while _
    | E_for _
    | E_for_each _
    | E_deref _
    | E_type_abstraction _
    | E_recursive _
    | E_let_in _
    | E_raw_code _
    | E_constructor _
    | E_record _
    | E_accessor _
    | E_update _
    | E_type_inst _ -> ret true exp
  in
  let (), res = O.Helpers.fold_map_expression aux () body in
  res
