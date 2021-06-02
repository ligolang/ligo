open Main_errors
open Mini_c
open Proto_alpha_utils
open Trace
open! Stacking
open Tezos_micheline

let dummy_locations : 'l 'p. ('l, 'p) Micheline.node -> (Location.t, 'p) Micheline.node =
  fun e ->
  Micheline.(inject_locations (fun _ -> Location.dummy) (strip_locations e))

let compile_contract : options:Compiler_options.t -> expression -> (Stacking.compiled_expression , _) result = fun ~options e ->
  let* e = trace self_mini_c_tracer @@ Self_mini_c.contract_check e in
  let* (input_ty , _) = trace self_mini_c_tracer @@ Self_mini_c.get_t_function e.type_expression in
  let* body = trace self_mini_c_tracer @@ Self_mini_c. get_function e in
  let body = Scoping.translate_closed_function body input_ty in
  let body = Stacking.Program.compile_function_body options.protocol_version body in
  let expr = Self_michelson.optimize options.protocol_version body in
  let expr_ty = Scoping.translate_type e.type_expression in
  let expr_ty = dummy_locations expr_ty in
  ok ({ expr_ty ; expr } : Stacking.Program.compiled_expression)

let compile_expression : options:Compiler_options.t -> expression -> (compiled_expression, _) result = fun ~options e ->
  trace stacking_tracer @@
  let (expr, _) = Scoping.translate_expression e [] in
  let expr = Stacking.Program.compile_expr options.protocol_version [] [] expr in
  let expr = Self_michelson.optimize options.protocol_version expr in
  let expr_ty = Scoping.translate_type e.type_expression in
  let expr_ty = dummy_locations expr_ty in
  ok ({ expr_ty ; expr } : Program.compiled_expression)

let aggregate_and_compile : options:Compiler_options.t -> program -> form_t -> (Stacking.compiled_expression, _) result =
    fun ~options program form ->
  let* aggregated = trace self_mini_c_tracer @@ Self_mini_c.aggregate_entry program form in
  let aggregated' = Self_mini_c.all_expression aggregated in
  match form with
  | ContractForm _ -> compile_contract ~options aggregated'
  | ExpressionForm _ -> compile_expression ~options aggregated'

let aggregate_and_compile_contract : options:Compiler_options.t ->  program -> string -> (Stacking.compiled_expression, _) result =
    fun ~options program name ->
  let* (exp, idx) = trace_option entrypoint_not_found @@ Mini_c.get_entry program name in
  let program' = List.take program idx in
  aggregate_and_compile ~options program' (ContractForm exp)

let aggregate_and_compile_expression = fun ~options program exp ->
  aggregate_and_compile ~options program (ExpressionForm exp)

let pretty_print program = 
  Mini_c.PP.program program


(* TODO refactor? *)

let aggregate = fun program form ->
  trace self_mini_c_tracer @@
  let* aggregated = Self_mini_c.aggregate_entry program form in
  ok @@ Self_mini_c.all_expression aggregated

let aggregate_contract = fun (program : Types.program) name ->
  let* (exp, idx) = trace_option entrypoint_not_found @@ get_entry program name in
  let program' = List.take program idx in
  aggregate program' (ContractForm exp)
