open Proto_alpha_utils
open Trace
open Memory_proto_alpha.Protocol.Script_ir_translator
open Memory_proto_alpha.X
open Simple_utils.Runned_result

module Errors = Main_errors

type options = Memory_proto_alpha.options

type dry_run_options =
  { amount : string ;
    balance : string ;
    now : string option ;
    sender : string option ;
    source : string option }

let make_dry_run_options (opts : dry_run_options) : (options , _) result =
  let open Proto_alpha_utils.Trace in
  let open Proto_alpha_utils.Memory_proto_alpha in
  let open Protocol.Alpha_context in
  let%bind balance = match Tez.of_string opts.balance with
    | None -> fail @@ Errors.invalid_balance opts.balance
    | Some balance -> ok balance in
  let%bind amount = match Tez.of_string opts.amount with
    | None -> fail @@ Errors.invalid_amount opts.amount
    | Some amount -> ok amount in
  let%bind sender =
    match opts.sender with
    | None -> ok None
    | Some sender ->
      let%bind sender =
        trace_alpha_tzresult
          (fun _ -> Errors.invalid_sender sender)
          (Contract.of_b58check sender) in
      ok (Some sender) in
  let%bind source =
    match opts.source with
    | None -> ok None
    | Some source ->
      let%bind source =
        trace_alpha_tzresult
          (fun _ -> Errors.invalid_source source)
          (Contract.of_b58check source) in
      ok (Some source) in
  let%bind now =
    match opts.now with
    | None -> ok None
    | Some st ->
      match Memory_proto_alpha.Protocol.Alpha_context.Script_timestamp.of_string st with
        | Some t -> ok (Some t)
        | None -> fail @@ Errors.invalid_timestamp st in
  ok @@ make_options ?now:now ~amount ~balance ?sender ?source ()

let ex_value_ty_to_michelson (v : ex_typed_value) : (Michelson.t , _) result =
  let (Ex_typed_value (value , ty)) = v in
  Trace.trace_tzresult_lwt Errors.unparsing_michelson_tracer @@
  Memory_proto_alpha.unparse_michelson_data value ty

let pack_payload (payload:Michelson.t) ty =
  let%bind payload =
    Trace.trace_tzresult_lwt Errors.parsing_payload_tracer @@
    Memory_proto_alpha.parse_michelson_data payload ty in
  let%bind data =
    Trace.trace_tzresult_lwt Errors.packing_payload_tracer @@
    Memory_proto_alpha.pack ty payload in
  ok @@ data

let fetch_lambda_types (contract_ty:ex_ty) =
  match contract_ty with
  | Ex_ty (Lambda_t (in_ty, out_ty, _)) -> ok (Ex_ty in_ty, Ex_ty out_ty)
  | _ -> fail Errors.unknown (*TODO*)

let run_contract ?options (exp:Michelson.t) (exp_type:ex_ty) (input_michelson:Michelson.t) : (ex_typed_value runned_result, _) result =
  let open! Tezos_raw_protocol_ligo006_PsCARTHA in
  let%bind (Ex_ty input_ty, Ex_ty output_ty) = fetch_lambda_types exp_type in
  let%bind input =
    Trace.trace_tzresult_lwt Errors.parsing_input_tracer @@
    Memory_proto_alpha.parse_michelson_data input_michelson input_ty
  in
  let top_level = Script_ir_translator.Toplevel
    { storage_type = output_ty ; param_type = input_ty ;
      root_name = None ; legacy_create_contract_literal = false } in
  let ty_stack_before = Script_typed_ir.Item_t (input_ty, Empty_t, None) in
  let ty_stack_after = Script_typed_ir.Item_t (output_ty, Empty_t, None) in
  let%bind descr =
    Trace.trace_tzresult_lwt Errors.parsing_code_tracer @@
    Memory_proto_alpha.parse_michelson_fail ~top_level exp ty_stack_before ty_stack_after in
  let open! Memory_proto_alpha.Protocol.Script_interpreter in
  let%bind res =
    Trace.trace_tzresult_lwt Errors.error_of_execution_tracer @@
    Memory_proto_alpha.failure_interpret ?options descr (Item(input, Empty)) in
  match res with
  | Memory_proto_alpha.Succeed stack ->
    let (Item(output, Empty)) = stack in
    ok @@ Success (Ex_typed_value (output_ty, output))
  | Memory_proto_alpha.Fail expr -> ( match Tezos_micheline.Micheline.root @@ Memory_proto_alpha.strings_of_prims expr with
    | Int (_ , i)    -> ok @@ Fail (Failwith_int (Z.to_int i))
    | String (_ , s) -> ok @@ Fail (Failwith_string s)
    | Bytes (_, s)   -> ok @@ Fail (Failwith_bytes s)
    | _              -> fail @@ Errors.unknown_failwith_type )

let run_expression ?options (exp:Michelson.t) (exp_type:ex_ty) : (ex_typed_value runned_result, _) result =
  let open! Tezos_raw_protocol_ligo006_PsCARTHA in
  let (Ex_ty exp_type') = exp_type in
  let top_level = Script_ir_translator.Lambda
  and ty_stack_before = Script_typed_ir.Empty_t
  and ty_stack_after = Script_typed_ir.Item_t (exp_type', Empty_t, None) in
  let%bind descr =
    Trace.trace_tzresult_lwt Errors.parsing_code_tracer @@
    Memory_proto_alpha.parse_michelson_fail ~top_level exp ty_stack_before ty_stack_after in
  let open! Memory_proto_alpha.Protocol.Script_interpreter in
  let%bind res =
    Trace.trace_tzresult_lwt Errors.error_of_execution_tracer @@
    Memory_proto_alpha.failure_interpret ?options descr Empty in
  match res with
  | Memory_proto_alpha.Succeed stack ->
    let (Item(output, Empty)) = stack in
    ok @@ Success (Ex_typed_value (exp_type', output))
  | Memory_proto_alpha.Fail expr -> ( match Tezos_micheline.Micheline.root @@ Memory_proto_alpha.strings_of_prims expr with
    | Int (_ , i)    -> ok @@ Fail (Failwith_int (Z.to_int i))
    | String (_ , s) -> ok @@ Fail (Failwith_string s)
    | Bytes (_, s)   -> ok @@ Fail (Failwith_bytes s)
    | _              -> fail @@ Errors.unknown_failwith_type )

let run_failwith ?options (exp:Michelson.t) (exp_type:ex_ty) : (failwith , _) result =
  let%bind expr = run_expression ?options exp exp_type in
  match expr with
  | Success _  -> fail Errors.unknown (* TODO : simple_fail "an error of execution was expected" *)
  | Fail res -> ok res

let run_no_failwith ?options (exp:Michelson.t) (exp_type:ex_ty) : (ex_typed_value , _) result =
  let%bind expr = run_expression ?options exp exp_type in
  match expr with
  | Success tval  -> ok tval
  | Fail _ -> fail Errors.unknown (* TODO : simple_fail "unexpected error of execution" *)

let evaluate_expression ?options exp exp_type =
  let%bind etv = run_expression ?options exp exp_type in
  match etv with
    | Success etv' -> ex_value_ty_to_michelson etv'
    | Fail res -> fail @@ Errors.failwith res
