open Proto_alpha_utils
open Memory_proto_alpha.X
open Trace
open Mini_c
open Compiler.Program

module Errors = struct

  let entry_error =
    simple_error "error translating entry point"

end

type options = {
  entry_point : anon_function ;
  input_type : type_value ;
  output_type : type_value ;
  input : value ;
  michelson_options : From_michelson.options ;
}

let run_entry ?(debug_michelson = false) ?options (entry : anon_function) ty (input:value) : value result =
  let%bind compiled =
    trace Errors.entry_error @@
    translate_entry entry ty in
  let%bind input_michelson = translate_value input (fst ty) in
  if debug_michelson then (
    Format.printf "Program: %a\n" Michelson.pp compiled.body ;
    Format.printf "Expression: %a\n" PP.expression entry.result ;
    Format.printf "Input: %a\n" PP.value input ;
    Format.printf "Input Type: %a\n" PP.type_ (fst ty) ;
    Format.printf "Compiled Input: %a\n" Michelson.pp input_michelson ;
  ) ;
  let%bind ex_ty_value = From_michelson.run ?options compiled input_michelson in
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
