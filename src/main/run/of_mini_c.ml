open Proto_alpha_utils
open Memory_proto_alpha.X
open Trace
open Mini_c
open! Compiler.Program

module Errors = struct

  let entry_error =
    simple_error "error translating entry point"

end

type options = {
  entry_point : anon_function ;
  input_type : type_value ;
  output_type : type_value ;
  input : value ;
  michelson_options : Of_michelson.options ;
}

let evaluate ?options expression =
  let%bind code = Compile.Of_mini_c.compile_expression_as_function expression in
  let%bind ex_ty_value = Of_michelson.evaluate ?options code in
  Compile.Of_mini_c.uncompile_value ex_ty_value

let evaluate_entry ?options program entry =
  let%bind code = Compile.Of_mini_c.compile_expression_as_function_entry program entry in
  let%bind ex_ty_value = Of_michelson.evaluate ?options code in
  Compile.Of_mini_c.uncompile_value ex_ty_value

let run_function ?options expression input ty =
  let%bind code = Compile.Of_mini_c.compile_function expression in
  let%bind input = Compile.Of_mini_c.compile_value input ty in
  let%bind ex_ty_value = Of_michelson.run ?options code input in
  Compile.Of_mini_c.uncompile_value ex_ty_value

let run_function_entry ?options program entry input =
  let%bind code = Compile.Of_mini_c.compile_function_entry program entry in
  let%bind input_michelson =
    let%bind code = Compile.Of_mini_c.compile_expression_as_function input in
    let%bind (Ex_typed_value (ty , value)) = Of_michelson.evaluate ?options code in
    Trace.trace_tzresult_lwt (simple_error "error unparsing input") @@
    Memory_proto_alpha.unparse_michelson_data ty value
  in
  let%bind ex_ty_value = Of_michelson.run ?options code input_michelson in
  Compile.Of_mini_c.uncompile_value ex_ty_value
