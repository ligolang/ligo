open Simple_utils.Function

let specialise_and_print_cameligo =
  Parsing.Cameligo.pretty_print Parsing.Cameligo.Pretty.default_state
  <@ Unification.Cameligo.decompile_program


let specialise_and_print_expression_cameligo =
  Parsing.Cameligo.pretty_print_expression Parsing.Cameligo.Pretty.default_state
  <@ Unification.Cameligo.decompile_expression


let specialise_and_print_ty_cameligo =
  Parsing.Cameligo.pretty_print_type_expr Parsing.Cameligo.Pretty.default_state
  <@ Unification.Cameligo.decompile_ty_expr


let specialise_and_print_jsligo =
  Parsing.Jsligo.pretty_print Parsing.Jsligo.Pretty.default_state
  <@ Unification.Jsligo.decompile_program


let specialise_and_print_expression_jsligo =
  Parsing.Jsligo.pretty_print_expression Parsing.Jsligo.Pretty.default_state
  <@ Unification.Jsligo.decompile_expression


let specialise_and_print_ty_jsligo =
  Parsing.Jsligo.pretty_print_type_expr Parsing.Jsligo.Pretty.default_state
  <@ Unification.Jsligo.decompile_ty_expr


let specialise_and_print (syntax : Syntax_types.t) source : Buffer.t =
  let specialise_and_print =
    match syntax with
    | CameLIGO -> specialise_and_print_cameligo
    | JsLIGO -> specialise_and_print_jsligo
  in
  specialise_and_print source


let specialise_and_print_expression (syntax : Syntax_types.t) source =
  let specialise_and_print =
    match syntax with
    | CameLIGO -> specialise_and_print_expression_cameligo
    | JsLIGO -> specialise_and_print_expression_jsligo
  in
  specialise_and_print source


let specialise_and_print_ty (syntax : Syntax_types.t) source =
  let specialise_and_print =
    match syntax with
    | CameLIGO -> specialise_and_print_ty_cameligo
    | JsLIGO -> specialise_and_print_ty_jsligo
  in
  specialise_and_print source
