open Proto_alpha_utils
open Trace
open Mini_c
open! Compiler.Program
open Memory_proto_alpha.Script_ir_translator

let run_aux ?options (program:compiled_program) (input_michelson:Michelson.t) : ex_typed_value result =
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
      (Stack.(input_ty @: nil)) (Stack.(output_ty @: nil)) in
  let open! Memory_proto_alpha.Script_interpreter in
  let%bind (Item(output, Empty)) =
    Trace.trace_tzresult_lwt (simple_error "error of execution") @@
    Memory_proto_alpha.interpret ?options descr (Item(input, Empty)) in
  ok (Ex_typed_value (output_ty, output))

let run_entry ?(debug_michelson = false) ?options (entry:anon_function) (input:value) : value result =
  let%bind compiled =
    let error =
      let title () = "compile entry" in
      let content () =
        Format.asprintf "%a" PP.function_ entry
      in
      error title content in
    trace error @@
    translate_entry entry in
  let%bind input_michelson = translate_value input in
  if debug_michelson then (
    Format.printf "Program: %a\n" Michelson.pp compiled.body ;
    Format.printf "Expression: %a\n" PP.expression entry.result ;
    Format.printf "Input: %a\n" PP.value input ;
    Format.printf "Input Type: %a\n" PP.type_ entry.input ;
    Format.printf "Compiled Input: %a\n" Michelson.pp input_michelson ;
  ) ;
  let%bind ex_ty_value = run_aux ?options compiled input_michelson in
  if debug_michelson then (
    let (Ex_typed_value (ty , v)) = ex_ty_value in
    ignore @@
    let%bind michelson_value =
      trace_tzresult_lwt (simple_error "debugging run_mini_c") @@
      Proto_alpha_utils.Memory_proto_alpha.unparse_michelson_data ty v in
    Format.printf "Compiled Output: %a\n" Michelson.pp michelson_value ;
    ok ()
  ) ;
  let%bind (result : value) = Compiler.Uncompiler.translate_value ex_ty_value in
  ok result
