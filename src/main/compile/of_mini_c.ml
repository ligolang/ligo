open Mini_c
open Tezos_utils
open Proto_alpha_utils
open Trace

let compile_expression_as_function : expression -> Compiler.compiled_program result = fun e ->
  let (input , output) = t_unit , e.type_value in
  let%bind body = Compiler.Program.translate_expression e Compiler.Environment.empty in
  let body = Self_michelson.optimize body in
  let body = Michelson.(seq [ i_drop ; body ]) in
  let%bind (input , output) = bind_map_pair Compiler.Type.Ty.type_ (input , output) in
  let open! Compiler.Program in
  ok { input ; output ; body }

let compile_function : expression -> Compiler.compiled_program result = fun e ->
  let%bind (input , output) = get_t_function e.type_value in
  let%bind body = get_function e in
  let%bind body = Compiler.Program.translate_function_body body [] input in
  let body = Self_michelson.optimize body in
  let%bind (input , output) = bind_map_pair Compiler.Type.Ty.type_ (input , output) in
  let open! Compiler.Program in
  ok { input ; output ; body }

let compile_expression_as_function_entry = fun program name ->
  let%bind aggregated = aggregate_entry program name true in
  let aggregated = Self_mini_c.all_expression aggregated in
  compile_function aggregated

let compile_function_entry = fun program name ->
  let%bind aggregated = aggregate_entry program name false in
  let aggregated = Self_mini_c.all_expression aggregated in
  compile_function aggregated

(* new *)

let compile_contract : expression -> Compiler.compiled_expression result = fun e ->
  let%bind (input , _) = get_t_function e.type_value in
  let%bind body = get_function e in
  let%bind body = Compiler.Program.translate_function_body body [] input in
  let expr = Self_michelson.optimize body in
  let%bind expr_ty = Compiler.Type.Ty.type_ e.type_value in
  let open! Compiler.Program in
  ok { expr_ty ; expr }

let compile_contract_as_exp = fun program name ->
  let%bind aggregated = aggregate_entry program name false in
  let aggregated = Self_mini_c.all_expression aggregated in
  compile_contract aggregated

let build_contract : Compiler.compiled_expression -> Michelson.michelson result =
  fun compiled ->
  let%bind ((Ex_ty _param_ty),(Ex_ty _storage_ty)) = Self_michelson.fetch_lambda_parameters compiled.expr_ty in
  (*TODO : bind pair trace_tzresult_lwt ? *)
  let%bind (param_michelson : Tezos_raw_protocol_005_PsBabyM1.Alpha_context.Script.node) = 
    Trace.trace_tzresult_lwt (simple_error "TODO") @@
    Proto_alpha_utils.Memory_proto_alpha.unparse_ty_michelson _param_ty in
  let%bind (storage_michelson : Tezos_raw_protocol_005_PsBabyM1.Alpha_context.Script.node) = 
    Trace.trace_tzresult_lwt (simple_error "TODO") @@
    Proto_alpha_utils.Memory_proto_alpha.unparse_ty_michelson _storage_ty in
  ok @@ Michelson.contract param_michelson storage_michelson compiled.expr
