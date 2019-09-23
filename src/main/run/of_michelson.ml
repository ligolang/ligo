open Proto_alpha_utils
open Trace
open Compiler.Program
open Memory_proto_alpha.Protocol.Script_ir_translator
open Memory_proto_alpha.X

type options = Memory_proto_alpha.options

let run ?options ?(is_input_value = false) (program:compiled_program) (input_michelson:Michelson.t) : ex_typed_value result =
  let Compiler.Program.{input;output;body} : compiled_program = program in
  let (Ex_ty input_ty) = input in
  let (Ex_ty output_ty) = output in
  let%bind input_ty_mich =
    Trace.trace_tzresult_lwt (simple_error "error unparsing input ty") @@
    Memory_proto_alpha.unparse_michelson_ty input_ty in
  let%bind output_ty_mich =
    Trace.trace_tzresult_lwt (simple_error "error unparsing output ty") @@
    Memory_proto_alpha.unparse_michelson_ty output_ty in
  Format.printf "code: %a\n" Michelson.pp program.body ;
  Format.printf "input_ty: %a\n" Michelson.pp input_ty_mich ;
  Format.printf "output_ty: %a\n" Michelson.pp output_ty_mich ;
  Format.printf "input: %a\n" Michelson.pp input_michelson ;
  let%bind input =
    if is_input_value then (
      Trace.trace_tzresult_lwt (simple_error "error parsing input") @@
      Memory_proto_alpha.parse_michelson_data input_michelson input_ty
    ) else (
      let input_michelson = Michelson.(seq [ input_michelson ; dip i_drop ]) in
      let body = Michelson.(strip_nops @@ strip_annots input_michelson) in
      let%bind descr =
        Trace.trace_tzresult_lwt (simple_error "error parsing input code") @@
        Memory_proto_alpha.parse_michelson body
          (Item_t (Memory_proto_alpha.Protocol.Script_typed_ir.Unit_t None, Empty_t, None)) (Item_t (input_ty, Empty_t, None)) in
      let%bind (Item(output, Empty)) =
        Trace.trace_tzresult_lwt (simple_error "input error of execution") @@
        Memory_proto_alpha.interpret ?options descr (Item((), Empty)) in
      ok output
    ) in
  let body = Michelson.(strip_nops @@ strip_annots body) in
  let%bind descr =
    Trace.trace_tzresult_lwt (simple_error "error parsing program code") @@
    Memory_proto_alpha.parse_michelson body
      (Item_t (input_ty, Empty_t, None)) (Item_t (output_ty, Empty_t, None)) in
  let open! Memory_proto_alpha.Protocol.Script_interpreter in
  let%bind (Item(output, Empty)) =
    Trace.trace_tzresult_lwt (simple_error "error of execution") @@
    Memory_proto_alpha.interpret ?options descr (Item(input, Empty)) in
  ok (Ex_typed_value (output_ty, output))

let evaluate ?options program = run ?options ~is_input_value:true program Michelson.d_unit
