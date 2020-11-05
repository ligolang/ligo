open Trace

let to_c_unit ~options f stx =
  let {libs ; _} : Compiler_options.t = options in
  let%bind c_unit  = Of_source.compile ~libs f (Syntax_name stx) in
  ok @@ c_unit

let to_imperative ~options c_unit f stx =
  let {libs ; _} : Compiler_options.t = options in
  let%bind imperative  = Of_c_unit.compile ~libs c_unit f (Syntax_name stx) in
  ok @@ imperative

let to_sugar ~options c_unit f stx =
  let%bind imperative = to_imperative ~options c_unit f stx in
  let%bind sugar      = Of_imperative.compile imperative in
  ok @@ sugar

let to_core ~options c_unit f stx =
  let%bind sugar  = to_sugar ~options c_unit f stx in
  let%bind core   = Of_sugar.compile sugar in
  ok @@ core

let type_file ~options f stx form : (Ast_typed.program_fully_typed * Ast_typed.environment * _ Typer.Solver.typer_state, _) result =
  let {init_env ; typer_switch ; _} : Compiler_options.t = options in
  let%bind c_unit,_      = to_c_unit ~options f stx in
  let%bind core          = to_core ~options c_unit f stx in
  let%bind typed,e,state = Of_core.compile ~typer_switch ~init_env form core in
  ok @@ (typed,e,state)

let to_mini_c ~options f stx env =
  let%bind typed,_,_  = type_file ~options f stx env in
  let%bind mini_c     = Of_typed.compile typed in
  ok @@ mini_c

let compile_file ~options f stx ep : (Michelson.michelson, _) result =
  let%bind typed,_,_  = type_file ~options f stx @@ Contract ep in
  let%bind mini_c     = Of_typed.compile typed in
  let%bind michelson  = Of_mini_c.aggregate_and_compile_contract mini_c ep in
  let%bind contract   = Of_michelson.build_contract michelson in
  ok @@ contract

let type_expression ~options source_file syntax expression env state =
  let {libs ; typer_switch ; _} : Compiler_options.t = options in
  let%bind v_syntax          = Helpers.syntax_to_variant (Syntax_name syntax) source_file in
  let%bind c_unit_exp, _     = Of_source.compile_string ~libs v_syntax expression in
  let%bind imperative_exp    = Of_c_unit.compile_expression ~libs v_syntax c_unit_exp in
  let%bind sugar_exp         = Of_imperative.compile_expression imperative_exp in
  let%bind core_exp          = Of_sugar.compile_expression sugar_exp in
  let%bind (typed_exp,state) = Of_core.compile_expression ~typer_switch ~env ~state core_exp in
  ok @@ (typed_exp,state)

let expression_to_mini_c ~options source_file syntax expression env state =
  let%bind (typed_exp,_)  = type_expression ~options source_file syntax expression env state in
  let%bind mini_c_exp     = Of_typed.compile_expression typed_exp in
  ok @@ mini_c_exp

let compile_expression ~options source_file syntax expression env state =
  let%bind mini_c_exp = expression_to_mini_c ~options source_file syntax expression env state in
  let%bind compiled   = Of_mini_c.compile_expression mini_c_exp in
  ok @@ compiled

let compile_and_aggregate_expression ~options source_file syntax expression env state mini_c_prg =
  let%bind mini_c_exp = expression_to_mini_c ~options source_file syntax expression env state in
  let%bind compiled   = Of_mini_c.aggregate_and_compile_expression mini_c_prg mini_c_exp in
  ok @@ compiled

let compile_storage ~options storage input source_file syntax env state mini_c_prg =
  let {libs ; typer_switch ; _} : Compiler_options.t = options in
  let%bind v_syntax   = Helpers.syntax_to_variant (Syntax_name syntax) (Some source_file) in
  let%bind (storage,_),(input,_) = Of_source.compile_contract_input ~libs storage input v_syntax in
  let%bind imperative = Of_c_unit.compile_contract_input ~libs storage input v_syntax in
  let%bind sugar      = Of_imperative.compile_expression imperative in
  let%bind core       = Of_sugar.compile_expression sugar in
  let%bind typed,_    = Of_core.compile_expression ~typer_switch ~env ~state core in
  let%bind mini_c     = Of_typed.compile_expression typed in
  let%bind compiled   = Of_mini_c.aggregate_and_compile_expression mini_c_prg mini_c in
  ok @@ compiled

let pretty_print ~options f stx =
  let%bind c_unit,_ = to_c_unit ~options f stx in
  Of_c_unit.pretty_print f c_unit (Syntax_name stx)

let pretty_print_cst ~options f stx =
  let%bind c_unit,_ = to_c_unit ~options f stx in
  Of_c_unit.pretty_print_cst f c_unit (Syntax_name stx)
