open Proto_alpha_utils
open Trace
open Compiler.Program
open Memory_proto_alpha.Protocol.Script_ir_translator
open Memory_proto_alpha.X

type options = Memory_proto_alpha.options

let run ?options (program:compiled_program) (input_michelson:Michelson.t) : ex_typed_value result =
  let Compiler.Program.{input;output;body} : compiled_program = program in
  let (Ex_ty input_ty) = input in
  let (Ex_ty output_ty) = output in
  let%bind input =
    Trace.trace_tzresult_lwt (simple_error "error parsing input") @@
    Memory_proto_alpha.parse_michelson_data input_michelson input_ty in
  let body = Michelson.strip_annots body in
  let%bind descr =
    Trace.trace_tzresult_lwt (simple_error "error parsing program code") @@
    Memory_proto_alpha.parse_michelson body
      (Item_t (input_ty, Empty_t, None)) (Item_t (output_ty, Empty_t, None)) in
  let open! Memory_proto_alpha.Protocol.Script_interpreter in
  let%bind (Item(output, Empty)) =
    Trace.trace_tzresult_lwt (simple_error "error of execution") @@
    Memory_proto_alpha.interpret ?options descr (Item(input, Empty)) in
  ok (Ex_typed_value (output_ty, output))
