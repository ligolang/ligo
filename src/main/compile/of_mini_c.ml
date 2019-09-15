open Trace
open Mini_c
open Tezos_utils

let compile_value : value -> type_value -> Michelson.t result =
  Compiler.Program.translate_value

let compile_expression : expression -> Michelson.t result = fun e ->
  Compiler.Program.translate_expression e Compiler.Environment.empty

let compile_function : anon_function -> type_value -> type_value -> Compiler.Program.compiled_program result = fun f in_ty out_ty ->
  Compiler.Program.translate_entry f (in_ty , out_ty)

let uncompile_value : Proto_alpha_utils.Memory_proto_alpha.X.ex_typed_value -> value result = fun x ->
  Compiler.Uncompiler.translate_value x
