open Mini_c
open Tezos_utils
open Proto_alpha_utils
open Trace

let compile_contract : expression -> Compiler.compiled_expression result = fun e ->
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

let aggregate_and_compile = fun program form ->
  let%bind aggregated = aggregate_entry program form in
  let aggregated' = Self_mini_c.all_expression aggregated in
  match form with
  | ContractForm _ -> compile_contract aggregated'
  | ExpressionForm _ -> compile_expression aggregated'

let aggregate_and_compile_contract = fun program name ->
  let%bind (exp, _) = get_entry program name in
  aggregate_and_compile program (ContractForm exp)

let aggregate_and_compile_expression = fun program exp ->
  aggregate_and_compile program (ExpressionForm exp)

let build_contract : Compiler.compiled_expression -> Michelson.michelson result =
  fun compiled ->
  let%bind ((Ex_ty _param_ty),(Ex_ty _storage_ty)) = Self_michelson.fetch_contract_inputs compiled.expr_ty in
  let%bind param_michelson =
    Trace.trace_tzresult_lwt (simple_error "Invalid contract: Could not unparse parameter") @@
    Proto_alpha_utils.Memory_proto_alpha.unparse_ty_michelson _param_ty in
  let%bind storage_michelson =
    Trace.trace_tzresult_lwt (simple_error "Invalid contract: Could not unparse storage") @@
    Proto_alpha_utils.Memory_proto_alpha.unparse_ty_michelson _storage_ty in
  let contract = Michelson.contract param_michelson storage_michelson compiled.expr in
  let%bind () = 
    Trace.trace_tzresult_lwt (simple_error "Invalid contract: Contract did not typecheck") @@
    Proto_alpha_utils.Memory_proto_alpha.typecheck_contract contract in
  ok contract

type check_type = Check_parameter | Check_storage
let assert_equal_michelson_type : check_type -> Compiler.compiled_expression -> Compiler.compiled_expression -> unit result =
  fun c compiled_prg compiled_param ->
    let%bind (Ex_ty expected_ty) =
      let%bind (c_param_ty,c_storage_ty) = Self_michelson.fetch_contract_inputs compiled_prg.expr_ty in
      match c with
      | Check_parameter -> ok c_param_ty
      | Check_storage -> ok c_storage_ty in
    let (Ex_ty actual_ty) = compiled_param.expr_ty in
    let%bind _ = 
      Trace.trace_tzresult (simple_error "Passed parameter does not match the contract type") @@
      Proto_alpha_utils.Memory_proto_alpha.assert_equal_michelson_type expected_ty actual_ty in
    ok ()