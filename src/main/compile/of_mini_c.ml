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

(*TODO rename to compile_function ; see if can be merge with compile expression ? do the same match as in get_t_function and done. ? *)
let compile_function_expression : expression -> Compiler.compiled_expression result = fun e ->
  let%bind (input_ty , _) = get_t_function e.type_value in
  let%bind body = get_function e in
  let%bind body = Compiler.Program.translate_function_body body [] input_ty in
  let expr = Self_michelson.optimize body in
  let%bind expr_ty = Compiler.Type.Ty.type_ e.type_value in
  let open! Compiler.Program in
  ok { expr_ty ; expr }

let compile_expression : expression -> Compiler.compiled_expression result = fun e ->
  let%bind expr = Compiler.Program.translate_expression e Compiler.Environment.empty in
  let expr = Self_michelson.optimize expr in
  let%bind expr_ty = Compiler.Type.Ty.type_ e.type_value in
  let open! Compiler.Program in
  ok { expr_ty ; expr }

let aggregate_and_compile_function = fun program name ->
  let%bind aggregated = aggregate_entry program name false in
  let aggregated = Self_mini_c.all_expression aggregated in
  compile_function_expression aggregated

let aggregate_and_compile_expression = fun program name ->
  let%bind aggregated = aggregate_entry program name true in
  let aggregated = Self_mini_c.all_expression aggregated in
  compile_expression aggregated

let build_contract : Compiler.compiled_expression -> Michelson.michelson result =
  fun compiled ->
  let%bind ((Ex_ty _param_ty),(Ex_ty _storage_ty)) = Self_michelson.fetch_lambda_parameters compiled.expr_ty in
  let%bind param_michelson =
    Trace.trace_tzresult_lwt (simple_error "Could not unparse contract lambda's parameter") @@
    Proto_alpha_utils.Memory_proto_alpha.unparse_ty_michelson _param_ty in
  let%bind storage_michelson =
    Trace.trace_tzresult_lwt (simple_error "Could not unparse contract lambda's storage") @@
    Proto_alpha_utils.Memory_proto_alpha.unparse_ty_michelson _storage_ty in
  let contract = Michelson.contract param_michelson storage_michelson compiled.expr in
  let%bind () = 
    Trace.trace_tzresult_lwt (simple_error "Invalid contract") @@
    Proto_alpha_utils.Memory_proto_alpha.typecheck_contract contract in
  ok contract
