open Expansion
open Simple_utils.Trace
open Main_errors
module SMap = Map.Make (String)

let compile_expression ~raise : Ast_aggregated.expression -> Ast_expanded.expression =
 fun e -> trace ~raise expansion_tracer @@ compile e


let compile_type_expression ~raise
    : Ast_aggregated.type_expression -> Ast_expanded.type_expression
  =
 fun ty -> trace ~raise expansion_tracer @@ compile_type ty
