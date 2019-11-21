open Proto_alpha_utils
open Trace
open Compiler.Program
open Memory_proto_alpha.Protocol.Script_ir_translator
open Memory_proto_alpha.X

type options = Memory_proto_alpha.options

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
