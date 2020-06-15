module Formatter = Formatter

open Main_errors
open Trace
open Simple_utils.Runned_result

type ret_type = Function | Expression
let uncompile_value func_or_expr program entry ex_ty_value =
  let%bind output_type =
    let%bind entry_expression = trace_option entrypoint_not_found @@ Ast_typed.get_entry program entry in
    match func_or_expr with
    | Expression ->
      ok entry_expression.type_expression
    | Function ->
      let%bind (_,output_type) = trace_option entrypoint_not_a_function @@ Ast_typed.get_t_function entry_expression.type_expression in
      ok output_type in
  let%bind mini_c = trace uncompile_michelson @@ Compiler.Uncompiler.translate_value ex_ty_value in
  let%bind typed =  trace uncompile_mini_c    @@ Transpiler.untranspile mini_c output_type in
  let%bind core  =  trace uncompile_typed     @@ Typer.untype_expression typed in
  ok @@ core

let uncompile_typed_program_entry_expression_result program entry runned_result =
  match runned_result with
  | Fail s -> ok (Fail s)
  | Success ex_ty_value ->
    let%bind uncompiled_value = uncompile_value Expression program entry ex_ty_value in
    ok (Success uncompiled_value)

let uncompile_typed_program_entry_function_result program entry runned_result =
  match runned_result with
  | Fail s -> ok (Fail s)
  | Success ex_ty_value ->
    let%bind uncompiled_value = uncompile_value Function program entry ex_ty_value in
    ok (Success uncompiled_value)

let uncompile_expression type_value runned_result =
  match runned_result with
  | Fail s -> ok (Fail s)
  | Success ex_ty_value ->
    let%bind mini_c = trace uncompile_michelson @@ Compiler.Uncompiler.translate_value ex_ty_value in
    let%bind typed = trace uncompile_mini_c @@ Transpiler.untranspile mini_c type_value in
    let%bind uncompiled_value = trace uncompile_typed @@ Typer.untype_expression typed in
    ok (Success uncompiled_value)

