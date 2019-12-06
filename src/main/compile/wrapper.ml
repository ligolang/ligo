open Trace

let source_to_typed syntax source_file =
  let%bind simplified  = Of_source.compile source_file syntax in
  let%bind typed,state = Of_simplified.compile simplified in
  let env = Ast_typed.program_environment typed in
  ok (typed,state,env)

let typed_to_michelson_fun
    (typed: Ast_typed.program) (entry_point:string) : Compiler.compiled_expression result =
  let%bind mini_c = Of_typed.compile typed in
  Of_mini_c.aggregate_and_compile_function mini_c entry_point

(* fetches entry_point and transform it into a let .. in let .. in expression *)
let typed_to_michelson_expression
    (typed: Ast_typed.program) (entry_point:string) : Compiler.compiled_expression result =
  let%bind mini_c = Of_typed.compile typed in
  Of_mini_c.aggregate_and_compile_expression mini_c entry_point

let source_expression_to_michelson ~env ~state parameter syntax =
  let%bind simplified = Of_source.compile_expression syntax parameter in
  let%bind (typed,_) = Of_simplified.compile_expression ~env ~state simplified in
  let%bind mini_c = Of_typed.compile_expression typed in
  Of_mini_c.compile_expression mini_c

let source_contract_param_to_michelson ~env ~state (storage,parameter) syntax =
  let%bind simplified = Of_source.compile_contract_input storage parameter syntax in
  let%bind typed,_    = Of_simplified.compile_expression ~env ~state simplified in
  let%bind mini_c = Of_typed.compile_expression typed in
  Of_mini_c.compile_expression mini_c

(*  produce a michelson contract e.g. the following sequence K_param ; K_storage ; K_code.
    and fails if the produced contract isn't valid                                         *)
let source_to_michelson_contract syntax source_file entry_point =
  let%bind (typed,state,env) = source_to_typed syntax source_file in
  let%bind michelson = typed_to_michelson_fun typed entry_point in
  let%bind contract = Of_mini_c.build_contract michelson in
  ok (contract, (typed,state,env))
