open Trace
open Ast_typed

let run_function ?options f input =
  let%bind code = Compile.Of_typed.compile_function f in
  let%bind input = Compile.Of_typed.compile_expression input in
  let%bind ex_ty_value = Of_michelson.run ?options code input in
  let%bind ty =
    let%bind (_ , output_ty) = get_t_function f.type_annotation in
    ok output_ty
  in
  Compile.Of_typed.uncompile_value ex_ty_value ty

let run_entry
    ?options (entry : string)
    (program : Ast_typed.program) (input : Ast_typed.annotated_expression) : Ast_typed.annotated_expression result =
  let%bind code = Compile.Of_typed.compile_function_entry program entry in
  let%bind input = Compile.Of_typed.compile_expression input in
  let%bind ex_ty_value = Of_michelson.run ?options code input in
  Compile.Of_typed.uncompile_entry_function_result program entry ex_ty_value

let evaluate ?options (e : annotated_expression) : annotated_expression result =
  let%bind code = Compile.Of_typed.compile_expression_as_function e in
  let%bind ex_ty_value = Of_michelson.evaluate ?options code in
  Compile.Of_typed.uncompile_value ex_ty_value e.type_annotation

let evaluate_entry ?options program entry =
  let%bind code = Compile.Of_typed.compile_expression_as_function_entry program entry in
  let%bind ex_ty_value = Of_michelson.evaluate ?options code in
  Compile.Of_typed.uncompile_entry_expression_result program entry ex_ty_value
