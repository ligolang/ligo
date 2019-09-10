open Trace
open Ast_typed
open Tezos_utils

let compile_expression : annotated_expression -> Michelson.t result = fun e ->
  let%bind mini_c_expression = Transpiler.translate_annotated_expression e in
  Of_mini_c.compile_expression mini_c_expression

let compile_entry : program -> string -> _ = fun p entry ->
  let%bind (f , (in_ty , out_ty)) = Transpiler.translate_entry p entry in
  Of_mini_c.compile_function f in_ty out_ty
