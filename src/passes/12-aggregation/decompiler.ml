open Ligo_prim
module I = Ast_aggregated
module O = Ast_typed

let rec decompile : I.expression -> O.expression =
 fun exp ->
  let decompile_value_attr : I.ValueAttr.t -> O.ValueAttr.t = fun x -> x in
  let return expression_content : O.expression =
    { expression_content
    ; location = exp.location
    ; type_expression = decompile_type exp.type_expression
    }
  in
  match exp.expression_content with
  | E_literal l -> return (O.E_literal l)
  | E_constant { cons_name; arguments } ->
    let arguments = List.map ~f:decompile arguments in
    return (O.E_constant { cons_name; arguments })
  | E_variable v -> return (O.E_variable v)
  | E_application { lamb; args } ->
    let args = decompile args in
    let lamb = decompile lamb in
    return (O.E_application { lamb; args })
  | E_lambda { binder; output_type; result } ->
    let binder = Param.map decompile_type binder in
    let output_type = decompile_type output_type in
    let result = decompile result in
    return (O.E_lambda { binder; output_type; result })
  | E_type_abstraction { type_binder; result } ->
    let result = decompile result in
    return (O.E_type_abstraction { type_binder; result })
  | E_recursive
      { fun_name; fun_type; lambda = { binder; output_type; result }; force_lambdarec } ->
    let fun_type = decompile_type fun_type in
    let result = decompile result in
    let output_type = decompile_type output_type in
    let binder = Param.map decompile_type binder in
    return
      (O.E_recursive
         { fun_name; fun_type; lambda = { binder; output_type; result }; force_lambdarec })
  | E_let_in { let_binder; rhs; let_result; attributes } ->
    let rhs = decompile rhs in
    let let_result = decompile let_result in
    let let_binder = I.Pattern.map decompile_type let_binder in
    let attributes = decompile_value_attr attributes in
    return (O.E_let_in { let_binder; rhs; let_result; attributes })
  | E_raw_code { language; code } ->
    let code = decompile code in
    return (O.E_raw_code { language; code })
  | E_type_inst { forall; type_ } ->
    let type_ = decompile_type type_ in
    let forall = decompile forall in
    return (O.E_type_inst { forall; type_ })
  (* Variant *)
  | E_constructor { constructor; element } ->
    let element = decompile element in
    return (O.E_constructor { constructor; element })
  | E_matching m ->
    let O.Match_expr.{ matchee; disc_label; cases } = decompile_match_expr m in
    return (O.E_matching { matchee; disc_label; cases })
  (* Record *)
  | E_record map ->
    let map = Record.map ~f:decompile map in
    return (O.E_record map)
  | E_accessor { struct_; path } ->
    let struct_ = decompile struct_ in
    return (O.E_accessor { struct_; path })
  | E_update { struct_; path; update } ->
    let struct_ = decompile struct_ in
    let update = decompile update in
    return (O.E_update { struct_; path; update })
  (* Imperative *)
  | I.E_let_mut_in { let_binder; rhs; let_result; attributes } ->
    let rhs = decompile rhs in
    let let_result = decompile let_result in
    let let_binder = I.Pattern.map decompile_type let_binder in
    let attributes = decompile_value_attr attributes in
    return (O.E_let_mut_in { let_binder; rhs; let_result; attributes })
  | I.E_deref var -> return (O.E_deref var)
  | I.E_assign { binder; expression } ->
    let binder = Binder.map decompile_type binder in
    let expression = decompile expression in
    return @@ O.E_assign { binder; expression }
  | I.E_coerce { anno_expr; type_annotation } ->
    let type_annotation = decompile_type type_annotation in
    let anno_expr = decompile anno_expr in
    return @@ O.E_coerce { anno_expr; type_annotation }
  | I.E_for for_loop ->
    let for_loop = For_loop.map decompile for_loop in
    return @@ O.E_for for_loop
  | I.E_for_each for_each_loop ->
    let for_each_loop = For_each_loop.map decompile for_each_loop in
    return @@ O.E_for_each for_each_loop
  | I.E_while while_loop ->
    let while_loop = While_loop.map decompile while_loop in
    return @@ O.E_while while_loop


and decompile_type : I.type_expression -> O.type_expression =
 fun ty ->
  let return type_content : O.type_expression =
    { type_content
    ; location = ty.location
    ; orig_var = Option.map ty.orig_var ~f:(fun v -> [], v)
    }
  in
  match ty.type_content with
  | T_variable v -> return (O.T_variable v)
  | T_constant { language; injection; parameters } ->
    let parameters = List.map ~f:decompile_type parameters in
    return (O.T_constant { language; injection; parameters })
  | T_sum { fields; layout } ->
    let fields = Record.map ~f:decompile_type fields in
    return (O.T_sum ({ fields; layout }, None))
  | T_record { fields; layout } ->
    let fields = Record.map ~f:decompile_type fields in
    return (O.T_record { fields; layout })
  | T_arrow { type1; type2; param_names } ->
    let type1 = decompile_type type1 in
    let type2 = decompile_type type2 in
    return (O.T_arrow { type1; type2; param_names })
  | T_singleton l -> return (O.T_singleton l)
  | T_for_all { ty_binder; kind; type_ } ->
    let type_ = decompile_type type_ in
    return (O.T_for_all { ty_binder; kind; type_ })


and decompile_match_expr
    :  (I.expression, I.type_expression) I.Match_expr.t
    -> (O.expression, O.type_expression) O.Match_expr.t
  =
 fun { matchee; disc_label; cases } ->
  let matchee = decompile matchee in
  let cases =
    List.map cases ~f:(fun { pattern; body } ->
        let pattern = I.Pattern.map decompile_type pattern in
        let body = decompile body in
        O.Match_expr.{ pattern; body })
  in
  O.Match_expr.{ matchee; disc_label; cases }
