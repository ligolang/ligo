open Proto_alpha_utils
open Trace
open Memory_proto_alpha.Protocol.Script_ir_translator
open Memory_proto_alpha.X

type options = Memory_proto_alpha.options

type run_res =
  | Success of ex_typed_value
  | Fail of Memory_proto_alpha.Protocol.Script_repr.expr

type run_failwith_res =
  | Failwith_int of int
  | Failwith_string of string
  | Failwith_bytes of bytes

type dry_run_options =
  { amount : string ;
    sender : string option ;
    source : string option }

let make_dry_run_options (opts : dry_run_options) : options result =
  let open Proto_alpha_utils.Trace in
  let open Proto_alpha_utils.Memory_proto_alpha in
  let open Protocol.Alpha_context in
  let%bind amount = match Tez.of_string opts.amount with
    | None -> simple_fail "invalid amount"
    | Some amount -> ok amount in
  let%bind sender =
    match opts.sender with
    | None -> ok None
    | Some sender ->
      let%bind sender =
        trace_alpha_tzresult
          (simple_error "invalid address")
          (Contract.of_b58check sender) in
      ok (Some sender) in
  let%bind source =
    match opts.source with
    | None -> ok None
    | Some source ->
      let%bind source =
        trace_alpha_tzresult
          (simple_error "invalid source address")
          (Contract.of_b58check source) in
      ok (Some source) in
  ok @@ make_options ~amount ?source:sender ?payer:source ()

let ex_value_ty_to_michelson (v : ex_typed_value) : Michelson.t result =
  let (Ex_typed_value (value , ty)) = v in
  Trace.trace_tzresult_lwt (simple_error "error unparsing michelson result") @@
  Memory_proto_alpha.unparse_michelson_data value ty

let pack_payload (payload:Michelson.t) ty =
  let%bind payload =
    Trace.trace_tzresult_lwt (simple_error "error parsing message") @@
    Memory_proto_alpha.parse_michelson_data payload ty in
  let%bind data =
    Trace.trace_tzresult_lwt (simple_error "error packing message") @@
    Memory_proto_alpha.pack ty payload in
  ok @@ data

let fetch_lambda_types (contract_ty:ex_ty) =
  match contract_ty with
  | Ex_ty (Lambda_t (in_ty, out_ty, _)) -> ok (Ex_ty in_ty, Ex_ty out_ty)
  | _ -> simple_fail "failed to fetch lambda types"

let run_contract ?options (exp:Michelson.t) (exp_type:ex_ty) (input_michelson:Michelson.t) : ex_typed_value result =
  let open! Tezos_raw_protocol_005_PsBabyM1 in
  let%bind (Ex_ty input_ty, Ex_ty output_ty) = fetch_lambda_types exp_type in
  let%bind input =
    Trace.trace_tzresult_lwt (simple_error "error parsing input") @@
    Memory_proto_alpha.parse_michelson_data input_michelson input_ty
  in
  let top_level = Script_ir_translator.Toplevel
    { storage_type = output_ty ; param_type = input_ty ;
      root_name = None ; legacy_create_contract_literal = false } in
  let ty_stack_before = Script_typed_ir.Item_t (input_ty, Empty_t, None) in
  let ty_stack_after = Script_typed_ir.Item_t (output_ty, Empty_t, None) in
  let exp = Michelson.strip_annots exp in
  let%bind descr =
    Trace.trace_tzresult_lwt (simple_error "error parsing program code") @@
    Memory_proto_alpha.parse_michelson ~top_level exp ty_stack_before ty_stack_after in
  let open! Memory_proto_alpha.Protocol.Script_interpreter in
  let%bind (Item(output, Empty)) =
    Trace.trace_tzresult_lwt (simple_error "error of execution") @@
    Memory_proto_alpha.interpret ?options descr
      (Item(input, Empty)) in
  ok (Ex_typed_value (output_ty, output))

let run_expression ?options (exp:Michelson.t) (exp_type:ex_ty) : run_res result =
  let open! Tezos_raw_protocol_005_PsBabyM1 in
  let (Ex_ty exp_type') = exp_type in
  let exp = Michelson.strip_annots exp in
  let top_level = Script_ir_translator.Lambda
  and ty_stack_before = Script_typed_ir.Empty_t
  and ty_stack_after = Script_typed_ir.Item_t (exp_type', Empty_t, None) in
  let%bind descr =
    Trace.trace_tzresult_lwt (simple_error "error parsing program code") @@
    Memory_proto_alpha.parse_michelson ~top_level exp ty_stack_before ty_stack_after in
  let open! Memory_proto_alpha.Protocol.Script_interpreter in
  let%bind res =
    Trace.trace_tzresult_lwt (simple_error "error of execution") @@
    Memory_proto_alpha.failure_interpret ?options descr Empty in
  match res with
  | Memory_proto_alpha.Succeed stack ->
    let (Item(output, Empty)) = stack in
    ok @@ Success (Ex_typed_value (exp_type', output))
  | Memory_proto_alpha.Fail expr ->
    ok (Fail expr)

let run ?options (exp:Michelson.t) (exp_type:ex_ty) : ex_typed_value result =
  let%bind expr = run_expression ?options exp exp_type in
  match expr with
  | Success res -> ok res
  | _  -> simple_fail "Execution terminated with failwith"

let run_failwith ?options (exp:Michelson.t) (exp_type:ex_ty) : run_failwith_res result =
  let%bind expr = run_expression ?options exp exp_type in
  match expr with
  | Fail res -> ( match Tezos_micheline.Micheline.root @@ Memory_proto_alpha.strings_of_prims res with
    | Int (_ , i)    -> ok (Failwith_int (Z.to_int i))
    | String (_ , s) -> ok (Failwith_string s)
    | Bytes (_,b)    -> ok (Failwith_bytes b)
    | _              -> simple_fail "Unknown failwith type" )
  | _  -> simple_fail "An error of execution was expected"

let evaluate_expression ?options exp exp_type =
  let%bind etv = run ?options exp exp_type in
  ex_value_ty_to_michelson etv