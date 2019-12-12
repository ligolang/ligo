open Trace

type ret_type = Function | Expression
let uncompile_value func_or_expr program entry ex_ty_value =
  let%bind entry_expression = Ast_typed.get_entry program entry in
  let%bind output_type = match func_or_expr with
    | Expression -> ok entry_expression.type_annotation
    | Function ->
      let%bind (_,output_type) =  Ast_typed.get_t_function entry_expression.type_annotation in
      ok output_type in
  let%bind mini_c = Compiler.Uncompiler.translate_value ex_ty_value in
  let%bind typed = Transpiler.untranspile mini_c output_type in
  Typer.untype_expression typed

let uncompile_typed_program_entry_expression_result program entry ex_ty_value =
  uncompile_value Expression program entry ex_ty_value

let uncompile_typed_program_entry_function_result program entry ex_ty_value =
  uncompile_value Function program entry ex_ty_value

let uncompile_expression type_value ex_ty_value =
  let%bind mini_c = Compiler.Uncompiler.translate_value ex_ty_value in
  let%bind typed = Transpiler.untranspile mini_c type_value in
  Typer.untype_expression typed