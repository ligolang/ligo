open Trace

let source_to_typed syntax source_file =
  let%bind simplified  = Of_source.compile source_file syntax in
  let%bind typed,state = Of_simplified.compile simplified in
  let env = Ast_typed.program_environment typed in
  ok (typed,state,env)

let source_to_typed_expression ~env ~state parameter syntax =
  let%bind simplified = Of_source.compile_expression syntax parameter in
  let%bind (typed,_) = Of_simplified.compile_expression ~env ~state simplified in
  ok typed

let typed_to_michelson_program
    (typed: Ast_typed.program) (entry_point:string) : Compiler.compiled_program result =
  let%bind mini_c = Of_typed.compile typed in
  Of_mini_c.compile_function_entry mini_c entry_point

let typed_to_michelson_value_as_function
    (typed: Ast_typed.program) (entry_point:string) : Compiler.compiled_program result =
  let%bind mini_c = Of_typed.compile typed in
  Of_mini_c.compile_expression_as_function_entry mini_c entry_point

let typed_expression_to_michelson_value_as_function
    (typed: Ast_typed.annotated_expression) : Compiler.compiled_program result =
  let%bind mini_c = Of_typed.compile_expression typed in
  Of_mini_c.compile_expression_as_function mini_c

let simplified_to_compiled_program
    ~env ~state (exp: Ast_simplified.expression) : Compiler.compiled_program result =
  let%bind (typed,_) = Of_simplified.compile_expression ~env ~state exp in
  typed_expression_to_michelson_value_as_function typed

let source_expression_to_michelson_value_as_function ~env ~state parameter syntax =
  let%bind typed  = source_to_typed_expression ~env ~state parameter syntax in
  let%bind mini_c = Of_typed.compile_expression typed in
  Of_mini_c.compile_expression_as_function mini_c

let source_contract_input_to_michelson_value_as_function ~env ~state (storage,parameter) syntax =
  let%bind simplified = Of_source.compile_contract_input storage parameter syntax in
  let%bind typed,_    = Of_simplified.compile_expression ~env ~state simplified in
  typed_expression_to_michelson_value_as_function typed

(* new *)
let typed_to_michelson_contract_as_exp
    (typed: Ast_typed.program) (entry_point:string) : Compiler.compiled_expression result =
  let%bind mini_c = Of_typed.compile typed in
  Of_mini_c.compile_contract_as_exp mini_c entry_point

(* produce a michelson contract e.g. the following sequence K_param ; K_storage ; K_code *)
let source_to_michelson_contract syntax source_file entry_point =
  let%bind (typed,state,env) = source_to_typed syntax source_file in
  let%bind michelson = typed_to_michelson_contract_as_exp typed entry_point in
  let%bind contract = Of_mini_c.build_contract michelson in
  ok (contract, (typed,state,env))
