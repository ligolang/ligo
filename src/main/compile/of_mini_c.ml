open Main_errors
open Mini_c
open Proto_alpha_utils
open Trace

let compile_contract : expression -> (Compiler.compiled_expression , _) result = fun e ->
  let%bind e = trace self_mini_c_tracer @@ Self_mini_c.contract_check e in
  let%bind (input_ty , _) = trace self_mini_c_tracer @@ Self_mini_c.get_t_function e.type_expression in
  let%bind body = trace self_mini_c_tracer @@ Self_mini_c. get_function e in
  let%bind body = trace compiler_tracer @@ Compiler.Program.translate_function_body body [] input_ty in
  let expr = Self_michelson.optimize body in
  let%bind expr_ty = trace compiler_tracer @@ Compiler.Type.Ty.type_ e.type_expression in
  ok ({ expr_ty ; expr } : Compiler.Program.compiled_expression)

let compile_expression : expression -> (Compiler.compiled_expression, _) result = fun e ->
  trace compiler_tracer @@
  let%bind expr = Compiler.Program.translate_expression e Compiler.Environment.empty in
  let expr = Self_michelson.optimize expr in
  let%bind expr_ty = Compiler.Type.Ty.type_ e.type_expression in
  ok ({ expr_ty ; expr } : Compiler.Program.compiled_expression)

let aggregate_and_compile : program -> form_t -> (Compiler.compiled_expression, _) result = fun program form ->
  let%bind aggregated = trace self_mini_c_tracer @@ Self_mini_c.aggregate_entry program form in
  let aggregated' = Self_mini_c.all_expression aggregated in
  match form with
  | ContractForm _ -> compile_contract aggregated'
  | ExpressionForm _ -> compile_expression aggregated'

let aggregate_and_compile_contract : program -> string -> (Compiler.compiled_expression, _) result = fun program name ->
  let%bind (exp, idx) = trace_option entrypoint_not_found @@ Mini_c.get_entry program name in
  let program' = List.take idx program in
  aggregate_and_compile program' (ContractForm exp)

let aggregate_and_compile_expression = fun program exp ->
  aggregate_and_compile program (ExpressionForm exp)

let pretty_print program = 
  Mini_c.PP.program program


(* TODO refactor? *)

let aggregate = fun program form ->
  trace self_mini_c_tracer @@
  let%bind aggregated = Self_mini_c.aggregate_entry program form in
  ok @@ Self_mini_c.all_expression aggregated

let aggregate_contract = fun (program : Types.program) name ->
  let%bind (exp, idx) = trace_option entrypoint_not_found @@ get_entry program name in
  let program' = List.take idx program in
  aggregate program' (ContractForm exp)
