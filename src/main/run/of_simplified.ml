open Trace
open Ast_simplified

let get_final_environment program =
  let last_declaration = Location.unwrap List.(hd @@ rev program) in
  let (Ast_typed.Declaration_constant (_ , (_ , post_env))) = last_declaration in
  post_env

let run_typed_program
    ?options ?input_to_value
    (program : Ast_typed.program) (entry : string)
    (input : expression) : expression result =
  let%bind code = Compile.Of_typed.compile_function_entry program entry in
  let%bind input =
    let env = get_final_environment program in
    Compile.Of_simplified.compile_expression ~env ?value:input_to_value input
  in
  let%bind ex_ty_value = Of_michelson.run ?options code input in
  Compile.Of_simplified.uncompile_typed_program_entry_function_result program entry ex_ty_value

let evaluate_typed_program_entry
    ?options
    (program : Ast_typed.program) (entry : string)
  : Ast_simplified.expression result =
  let%bind code = Compile.Of_typed.compile_expression_as_function_entry program entry in
  let%bind ex_ty_value = Of_michelson.evaluate ?options code in
  Compile.Of_simplified.uncompile_typed_program_entry_expression_result program entry ex_ty_value
