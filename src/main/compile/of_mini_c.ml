open Trace
open Mini_c
open Tezos_utils

let compile_value : value -> type_value -> Michelson.t result =
  Compiler.Program.translate_value

let compile_expression : expression -> _ result = fun e ->
  Compiler.Program.translate_expression e Compiler.Environment.empty

let compile_expression_as_function : expression -> _ result = fun e ->
  let (input , output) = t_unit , e.type_value in
  let%bind body = get_function e in
  let%bind body = compile_value body (t_function input output) in
  let%bind (input , output) = bind_map_pair Compiler.Type.Ty.type_ (input , output) in
  let open! Compiler.Program in
  ok { input ; output ; body }

let compile_function = fun e ->
  let%bind (input , output) = get_t_function e.type_value in
  let%bind body = get_function e in
  let%bind body = compile_value body (t_function input output) in
  let%bind (input , output) = bind_map_pair Compiler.Type.Ty.type_ (input , output) in
  let open! Compiler.Program in
  ok { input ; output ; body }

(* let compile_function : anon_function -> (type_value * type_value) -> Compiler.Program.compiled_program result = fun f io ->
 *   Compiler.Program.translate_entry f io *)

let compile_expression_as_function_entry = fun program name ->
  let%bind aggregated = aggregate_entry program name true in
  compile_function aggregated

let compile_function_entry = fun program name ->
  let%bind aggregated = aggregate_entry program name false in
  compile_function aggregated

let uncompile_value : Proto_alpha_utils.Memory_proto_alpha.X.ex_typed_value -> value result = fun x ->
  Compiler.Uncompiler.translate_value x
