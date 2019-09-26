open Trace
open Ast_typed
open Tezos_utils


let compile_expression_as_value : annotated_expression -> Michelson.t result = fun e ->
  let%bind mini_c_expression = Transpiler.transpile_annotated_expression e in
  let%bind expr = Of_mini_c.compile_expression_as_value mini_c_expression in
  ok expr

let compile_expression_as_function : annotated_expression -> _ result = fun e ->
  let%bind mini_c_expression = Transpiler.transpile_annotated_expression e in
  let%bind expr = Of_mini_c.compile_expression_as_function mini_c_expression in
  ok expr

let compile_function : annotated_expression -> _ result = fun e ->
  let%bind mini_c_expression = Transpiler.transpile_annotated_expression e in
  let%bind expr = Of_mini_c.compile_function mini_c_expression in
  ok expr

(*
   val compile_value : annotated_expression -> Michelson.t result
   This requires writing a function
   `transpile_expression_as_value : annotated_expression -> Mini_c.value result`
 *)

let compile_function_entry : program -> string -> _ = fun p entry ->
  let%bind prog_mini_c = Transpiler.transpile_program p in
  Of_mini_c.compile_function_entry prog_mini_c entry

let compile_contract_entry : program -> string -> _ = fun p entry ->
  let%bind prog_mini_c = Transpiler.transpile_program p in
  Of_mini_c.compile_contract_entry prog_mini_c entry

let compile_expression_as_function_entry : program -> string -> _ = fun p entry ->
  let%bind prog_mini_c = Transpiler.transpile_program p in
  Of_mini_c.compile_expression_as_function_entry prog_mini_c entry

let uncompile_value : _ -> _ -> annotated_expression result = fun x ty ->
  let%bind mini_c = Of_mini_c.uncompile_value x in
  let%bind typed = Transpiler.untranspile mini_c ty in
  ok typed

let uncompile_entry_function_result = fun program entry ex_ty_value ->
  let%bind output_type =
    let%bind entry_expression = get_entry program entry in
    let%bind (_ , output_type) = get_t_function entry_expression.type_annotation in
    ok output_type
  in
  uncompile_value ex_ty_value output_type

let uncompile_entry_expression_result = fun program entry ex_ty_value ->
  let%bind output_type =
    let%bind entry_expression = get_entry program entry in
    ok entry_expression.type_annotation
  in
  uncompile_value ex_ty_value output_type
