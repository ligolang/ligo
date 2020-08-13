open Trace 

let to_imperatve f stx =
  let%bind imperative  = Of_source.compile f (Syntax_name stx) in
  ok @@ imperative

let to_sugar f stx = 
  let%bind imperative = to_imperatve f stx in 
  let%bind sugar      = Of_imperative.compile imperative in
  ok @@ sugar

let to_core f stx = 
  let%bind sugar  = to_sugar f stx in
  let%bind core   = Of_sugar.compile sugar in
  ok @@ core

let type_file f stx form : (Ast_typed.program * _ Typesystem.Solver_types.typer_state, _) result =
  let%bind core        = to_core f stx in
  let%bind typed,state = Of_core.compile form core in
  ok @@ (typed,state)

let to_mini_c f stx env =
  let%bind typed, _ = type_file f stx env in
  let%bind mini_c     = Of_typed.compile typed in
  ok @@ mini_c

let compile_file f stx ep : (Michelson.michelson, _) result =
  let%bind typed, _ = type_file f stx @@ Contract ep in
  let%bind mini_c     = Of_typed.compile typed in
  let%bind michelson  = Of_mini_c.aggregate_and_compile_contract mini_c ep in
  let%bind contract   = Of_michelson.build_contract michelson in
  ok @@ contract

let type_expression source_file syntax expression env state =
  let%bind v_syntax         = Helpers.syntax_to_variant (Syntax_name syntax) source_file in
  let%bind imperative_exp    = Of_source.compile_expression v_syntax expression in
  let%bind sugar_exp         = Of_imperative.compile_expression imperative_exp in
  let%bind core_exp          = Of_sugar.compile_expression sugar_exp in
  let%bind (typed_exp,state) = Of_core.compile_expression ~env ~state core_exp in
  ok @@ (typed_exp,state)

let expression_to_mini_c source_file syntax expression env state =
  let%bind (typed_exp,_)  = type_expression source_file syntax expression env state in
  let%bind mini_c_exp     = Of_typed.compile_expression typed_exp in
  ok @@ mini_c_exp

let compile_expression source_file syntax expression env state =
  let%bind mini_c_exp = expression_to_mini_c source_file syntax expression env state in
  let%bind compiled   = Of_mini_c.compile_expression mini_c_exp in
  ok @@ compiled

let compile_and_aggregate_expression source_file syntax expression env state mini_c_prg =
  let%bind mini_c_exp = expression_to_mini_c source_file syntax expression env state in
  let%bind compiled   = Of_mini_c.aggregate_and_compile_expression mini_c_prg mini_c_exp in
  ok @@ compiled

let compile_storage storage input source_file syntax env state mini_c_prg =
  let%bind v_syntax   = Helpers.syntax_to_variant (Syntax_name syntax) (Some source_file) in
  let%bind imperative = Of_source.compile_contract_input storage input v_syntax in
  let%bind sugar      = Of_imperative.compile_expression imperative in
  let%bind core       = Of_sugar.compile_expression sugar in
  let%bind typed,_    = Of_core.compile_expression ~env ~state core in
  let%bind mini_c     = Of_typed.compile_expression typed in
  let%bind compiled   = Of_mini_c.aggregate_and_compile_expression mini_c_prg mini_c in
  ok @@ compiled
