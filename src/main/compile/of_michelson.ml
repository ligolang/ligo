open Tezos_utils
open Proto_alpha_utils
open Trace

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
let assert_equal_contract_type : check_type -> Compiler.compiled_expression -> Compiler.compiled_expression -> unit result =
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