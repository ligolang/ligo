open Ast_simplified
open Trace
open Tezos_utils

let compile_contract_entry (program : program) entry_point =
  let%bind prog_typed = Typer.type_program program in
  Of_typed.compile_contract_entry prog_typed entry_point

let compile_function_entry (program : program) entry_point : _ result =
  let%bind prog_typed = Typer.type_program program in
  Of_typed.compile_function_entry prog_typed entry_point

let compile_expression_as_function_entry (program : program) entry_point : _ result =
  let%bind typed_program = Typer.type_program program in
  Of_typed.compile_expression_as_function_entry typed_program entry_point

let compile_expression_as_value ?(env = Ast_typed.Environment.full_empty) ae : Michelson.t result =
  let%bind typed = Typer.type_expression env ae in
  Of_typed.compile_expression_as_value typed

let compile_expression_as_function ?(env = Ast_typed.Environment.full_empty) ae : _ result =
  let%bind typed = Typer.type_expression env ae in
  Of_typed.compile_expression_as_function typed

let uncompile_typed_program_entry_expression_result program entry ex_ty_value =
  let%bind output_type =
    let%bind entry_expression = Ast_typed.get_entry program entry in
    ok entry_expression.type_annotation
  in
  let%bind typed = Of_typed.uncompile_value ex_ty_value output_type in
  Typer.untype_expression typed

let uncompile_typed_program_entry_function_result program entry ex_ty_value =
  let%bind output_type =
    let%bind entry_expression = Ast_typed.get_entry program entry in
    let%bind (_ , output_type) = Ast_typed.get_t_function entry_expression.type_annotation in
    ok output_type
  in
  let%bind typed = Of_typed.uncompile_value ex_ty_value output_type in
  Typer.untype_expression typed
