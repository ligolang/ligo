open Mini_c
open Proto_alpha_utils
open Trace

let compile_contract : expression -> Compiler.compiled_expression result = fun e ->
  let%bind (input_ty , _) = get_t_function e.type_value in
  let%bind body = get_function e in
  let%bind body = Compiler.Program.translate_function_body body [] input_ty in
  let expr = Self_michelson.optimize body in
  let%bind expr_ty = Compiler.Type.Ty.type_ e.type_value in
  ok ({ expr_ty ; expr } : Compiler.Program.compiled_expression)

let compile_expression : expression -> Compiler.compiled_expression result = fun e ->
  let%bind expr = Compiler.Program.translate_expression e Compiler.Environment.empty in
  let expr = Self_michelson.optimize expr in
  let%bind expr_ty = Compiler.Type.Ty.type_ e.type_value in
  ok ({ expr_ty ; expr } : Compiler.Program.compiled_expression)

let aggregate_and_compile = fun program form ->
  let%bind aggregated = aggregate_entry program form in
  let aggregated' = Self_mini_c.all_expression aggregated in
  match form with
  | ContractForm _ -> compile_contract aggregated'
  | ExpressionForm _ -> compile_expression aggregated'

let aggregate_and_compile_contract = fun (program : Types.program) name ->
  let%bind (exp, idx) = get_entry program name in
  let program' = List.take idx program in
  aggregate_and_compile program' (ContractForm exp)

let aggregate_and_compile_expression = fun program exp ->
  aggregate_and_compile program (ExpressionForm exp)
