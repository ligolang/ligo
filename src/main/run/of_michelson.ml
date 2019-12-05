open Proto_alpha_utils
open Trace
open Compiler.Program
open Memory_proto_alpha.Protocol.Script_ir_translator
open Memory_proto_alpha.X

type options = Memory_proto_alpha.options

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

let run ?options (* ?(is_input_value = false) *) (program:compiled_program) (input_michelson:Michelson.t) : ex_typed_value result =
  let Compiler.Program.{input;output;body} : compiled_program = program in
  let (Ex_ty input_ty) = input in
  let (Ex_ty output_ty) = output in
  (* let%bind input_ty_mich =
    Trace.trace_tzresult_lwt (simple_error "error unparsing input ty") @@
    Memory_proto_alpha.unparse_michelson_ty input_ty in
  let%bind output_ty_mich =
    Trace.trace_tzresult_lwt (simple_error "error unparsing output ty") @@
    Memory_proto_alpha.unparse_michelson_ty output_ty in
  Format.printf "code: %a\n" Michelson.pp program.body ;
  Format.printf "input_ty: %a\n" Michelson.pp input_ty_mich ;
  Format.printf "output_ty: %a\n" Michelson.pp output_ty_mich ;
  Format.printf "input: %a\n" Michelson.pp input_michelson ; *)
  let%bind input =
    Trace.trace_tzresult_lwt (simple_error "error parsing input") @@
    Memory_proto_alpha.parse_michelson_data input_michelson input_ty
  in
  let body = Michelson.strip_annots body in
  let open! Memory_proto_alpha.Protocol.Script_ir_translator in 
  let top_level = Toplevel { storage_type = output_ty ; param_type = input_ty ;
                             root_name = None ; legacy_create_contract_literal = false } in
  let%bind descr =
    Trace.trace_tzresult_lwt (simple_error "error parsing program code") @@
    Memory_proto_alpha.parse_michelson ~top_level body
      (Item_t (input_ty, Empty_t, None)) (Item_t (output_ty, Empty_t, None)) in
  let open! Memory_proto_alpha.Protocol.Script_interpreter in
  let%bind (Item(output, Empty)) =
    Trace.trace_tzresult_lwt (simple_error "error of execution") @@
    Memory_proto_alpha.interpret ?options descr (Item(input, Empty)) in
  ok (Ex_typed_value (output_ty, output))

type failwith_res =
  | Failwith_int of int
  | Failwith_string of string
  | Failwith_bytes of bytes

let get_exec_error_aux ?options (program:compiled_program) (input_michelson:Michelson.t) : Memory_proto_alpha.Protocol.Script_repr.expr result =
  let Compiler.Program.{input;output;body} : compiled_program = program in
  let (Ex_ty input_ty) = input in
  let (Ex_ty output_ty) = output in
  let%bind input =
    Trace.trace_tzresult_lwt (simple_error "error parsing input") @@
    Memory_proto_alpha.parse_michelson_data input_michelson input_ty
  in
  let body = Michelson.strip_annots body in
  let%bind descr =
    Trace.trace_tzresult_lwt (simple_error "error parsing program code") @@
    Memory_proto_alpha.parse_michelson body
      (Item_t (input_ty, Empty_t, None)) (Item_t (output_ty, Empty_t, None)) in
  let%bind err =
    Trace.trace_tzresult_lwt (simple_error "unexpected error of execution") @@
    Memory_proto_alpha.failure_interpret ?options descr (Item(input, Empty)) in
  match err with
  | Memory_proto_alpha.Succeed _ -> simple_fail "an error of execution was expected" 
  | Memory_proto_alpha.Fail expr ->
    ok expr

let get_exec_error ?options (program:compiled_program) (input_michelson:Michelson.t) : failwith_res result =
  let%bind expr = get_exec_error_aux ?options program input_michelson in
  match Tezos_micheline.Micheline.root @@ Memory_proto_alpha.strings_of_prims expr with
  | Int (_ , i)    -> ok (Failwith_int (Z.to_int i))
  | String (_ , s) -> ok (Failwith_string s)
  | Bytes (_,b)    -> ok (Failwith_bytes b)
  | _  -> simple_fail "Unknown failwith"

let evaluate ?options program = run ?options program Michelson.d_unit

let ex_value_ty_to_michelson (v : ex_typed_value) : Michelson.t result =
  let (Ex_typed_value (value , ty)) = v in
  Trace.trace_tzresult_lwt (simple_error "error unparsing michelson result") @@
  Memory_proto_alpha.unparse_michelson_data value ty

let evaluate_michelson ?options program =
  let%bind etv = evaluate ?options program in
  ex_value_ty_to_michelson etv

let pack_payload (payload:Michelson.t) ty =
  let%bind payload =
    Trace.trace_tzresult_lwt (simple_error "error parsing message") @@
    Memory_proto_alpha.parse_michelson_data payload ty in
  let%bind data =
    Trace.trace_tzresult_lwt (simple_error "error packing message") @@
    Memory_proto_alpha.pack ty payload in
  ok @@ data

(* new *)
let fetch_lambda_types (contract_ty:ex_ty) =
  match contract_ty with
  | Ex_ty (Lambda_t (in_ty, out_ty, _)) -> ok (Ex_ty in_ty, Ex_ty out_ty)
  | _ -> simple_fail "failed to fetch lambda types"

(* type run_res = Failwith of failwith_res | Success of ex_typed_value *)
let run_function ?options (exp:Michelson.t) (exp_type:ex_ty) (input_michelson:Michelson.t) (is_contract:bool) : ex_typed_value result =
  let open! Tezos_raw_protocol_005_PsBabyM1 in
  let%bind (Ex_ty input_ty, Ex_ty output_ty) = fetch_lambda_types exp_type in
  let%bind input =
    Trace.trace_tzresult_lwt (simple_error "error parsing input") @@
    Memory_proto_alpha.parse_michelson_data input_michelson input_ty
  in
  let (top_level, ty_stack_before, ty_stack_after) =
    (if is_contract then
      Script_ir_translator.Toplevel { storage_type = output_ty ; param_type = input_ty ;
                                      root_name = None ; legacy_create_contract_literal = false }
    else Script_ir_translator.Lambda) ,
    Script_typed_ir.Item_t (input_ty, Empty_t, None),
    Script_typed_ir.Item_t (output_ty, Empty_t, None) in
  let exp = Michelson.strip_annots exp in
  let%bind descr =
    Trace.trace_tzresult_lwt (simple_error "error parsing program code") @@
    Memory_proto_alpha.parse_michelson ~top_level exp ty_stack_before ty_stack_after in
  let open! Memory_proto_alpha.Protocol.Script_interpreter in
  let%bind (Item(output, Empty)) =
    Trace.trace_tzresult_lwt (simple_error "error of execution") @@
    Memory_proto_alpha.interpret ?options descr
      (Item(input, Empty))
      in
  ok (Ex_typed_value (output_ty, output))

let run_exp ?options (exp:Michelson.t) (exp_type:ex_ty) : ex_typed_value result =
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
  let%bind (Item(output, Empty)) =
    Trace.trace_tzresult_lwt (simple_error "error of execution") @@
    Memory_proto_alpha.interpret ?options descr Empty in
  ok (Ex_typed_value (exp_type', output))

let evaluate_expression ?options exp exp_type =
  let%bind etv = run_exp ?options exp exp_type in
  ex_value_ty_to_michelson etv