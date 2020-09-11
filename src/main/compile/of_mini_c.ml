open Main_errors
open Mini_c
open Proto_alpha_utils
open Trace
open Stacking

let compile_contract : expression -> (Stacking.compiled_expression , _) result = fun e ->
  let%bind e = trace self_mini_c_tracer @@ Self_mini_c.contract_check e in
  let%bind (input_ty , _) = trace self_mini_c_tracer @@ Self_mini_c.get_t_function e.type_expression in
  let%bind body = trace self_mini_c_tracer @@ Self_mini_c. get_function e in
  let body = Scoping.translate_closed_function body input_ty in
  let%bind body = trace stacking_tracer @@ Stacking.Program.translate_function_body body [] [] in
  let expr = Self_michelson.optimize body in
  let%bind expr_ty = trace stacking_tracer @@ Stacking.Type.type_ e.type_expression in
  ok ({ expr_ty ; expr } : Stacking.Program.compiled_expression)

let compile_expression : expression -> (compiled_expression, _) result = fun e ->
  trace stacking_tracer @@
  let (expr, _) = Scoping.translate_expression e [] in
  let%bind expr = Stacking.Program.translate_expression expr [] [] in
  let expr = Self_michelson.optimize expr in
  let%bind expr_ty = Stacking.Type.type_ e.type_expression in
  ok ({ expr_ty ; expr } : Program.compiled_expression)

let aggregate_and_compile : program -> form_t -> (Stacking.compiled_expression, _) result = fun program form ->
  let%bind aggregated = trace self_mini_c_tracer @@ Self_mini_c.aggregate_entry program form in
  let aggregated' = Self_mini_c.all_expression aggregated in
  match form with
  | ContractForm _ -> compile_contract aggregated'
  | ExpressionForm _ -> compile_expression aggregated'

let aggregate_and_compile_contract : program -> string -> (Stacking.compiled_expression, _) result = fun program name ->
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
