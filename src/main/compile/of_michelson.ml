open Main_errors
open Tezos_utils
open Proto_alpha_utils
open Trace

let build_contract : ?disable_typecheck:bool -> Compiler.compiled_expression -> (Michelson.michelson , _) result =
  fun ?(disable_typecheck= false) compiled ->
  let%bind ((Ex_ty _param_ty),(Ex_ty _storage_ty)) = trace_option (entrypoint_not_a_function) @@
    Self_michelson.fetch_contract_inputs compiled.expr_ty in
  let%bind param_michelson =
    Trace.trace_tzresult_lwt unparse_tracer @@
    Proto_alpha_utils.Memory_proto_alpha.unparse_ty_michelson _param_ty in
  let%bind storage_michelson =
    Trace.trace_tzresult_lwt unparse_tracer @@
    Proto_alpha_utils.Memory_proto_alpha.unparse_ty_michelson _storage_ty in
  let contract = Michelson.contract param_michelson storage_michelson compiled.expr in
  if disable_typecheck then
    ok contract
  else
    let%bind res =
      Trace.trace_tzresult_lwt (typecheck_contract_tracer contract) @@
      Proto_alpha_utils.Memory_proto_alpha.typecheck_contract contract in
    match res with
    | Type_checked  -> ok contract
    | Err_parameter -> fail @@ bad_parameter contract
    | Err_storage   -> fail @@ bad_storage contract
    | Err_contract  -> fail @@ bad_contract contract
    | Err_gas       -> fail @@ gas_exhaustion
    | Err_unknown   -> fail @@ unknown

let assert_equal_contract_type : Simple_utils.Runned_result.check_type -> Compiler.compiled_expression -> Compiler.compiled_expression -> (unit , _) result =
  fun c compiled_prg compiled_param ->
    let%bind (Ex_ty expected_ty) =
      let%bind (c_param_ty,c_storage_ty) = trace_option (entrypoint_not_a_function) @@
        Self_michelson.fetch_contract_inputs compiled_prg.expr_ty in
      match c with
      | Check_parameter -> ok c_param_ty
      | Check_storage -> ok c_storage_ty in
    let (Ex_ty actual_ty) = compiled_param.expr_ty in
    let%bind _ = 
      Trace.trace_tzresult typecheck_parameters_tracer @@
      Proto_alpha_utils.Memory_proto_alpha.assert_equal_michelson_type expected_ty actual_ty in
    ok ()
