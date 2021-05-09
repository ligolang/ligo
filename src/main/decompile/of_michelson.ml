module Formatter = Formatter

open Main_errors
open Trace
open Simple_utils.Runned_result

type ret_type = Function | Expression
let decompile_value func_or_expr program entry (ty, value) =
  let* output_type =
    let* entry_expression = trace_option entrypoint_not_found @@ Ast_typed.get_entry program entry in
    match func_or_expr with
    | Expression ->
      ok entry_expression.type_expression
    | Function ->
      let* (_,output_type) = trace_option entrypoint_not_a_function @@ Ast_typed.get_t_function entry_expression.type_expression in
      ok output_type in
  let* mini_c = trace decompile_michelson @@ Stacking.Decompiler.decompile_value ty value in
  let* typed =  trace decompile_mini_c    @@ Spilling.decompile mini_c output_type in
  let* inferred  =  trace decompile_typed @@ Checking.untype_expression typed in
  let* core      = trace inference_tracer @@ Inference.Untyper.untype_expression inferred in
  ok @@ core

let decompile_typed_program_entry_expression_result program entry runned_result =
  match runned_result with
  | Fail s -> ok (Fail s)
  | Success ex_ty_value ->
    let* decompiled_value = decompile_value Expression program entry ex_ty_value in
    ok (Success decompiled_value)

let decompile_typed_program_entry_function_result program entry runned_result =
  match runned_result with
  | Fail s -> ok (Fail s)
  | Success ex_ty_value ->
    let* decompiled_value = decompile_value Function program entry ex_ty_value in
    ok (Success decompiled_value)

let decompile_expression type_value runned_result =
  match runned_result with
  | Fail s -> ok (Fail s)
  | Success (ty, value) ->
    let* mini_c = trace decompile_michelson @@ Stacking.Decompiler.decompile_value ty value in
    let* typed = trace decompile_mini_c @@ Spilling.decompile mini_c type_value in
    let* decompiled_value = trace decompile_typed @@ Checking.untype_expression typed in
    ok (Success decompiled_value)
