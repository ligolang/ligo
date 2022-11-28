open Simple_utils.Trace
open Ast_expanded
open Spilling
open Main_errors

module SMap = Map.Make(String)

let compile_expression ~raise : expression -> Mini_c.expression = fun e ->
  trace ~raise spilling_tracer @@ compile_expression e

let compile_type ~raise : type_expression -> Mini_c.type_expression = fun e ->
  trace ~raise spilling_tracer @@ compile_type e
