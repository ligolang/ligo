open Expansion
module Trace = Simple_utils.Trace

let compile_expression ~raise : Ast_aggregated.expression -> Ast_expanded.expression =
 fun e -> Trace.trace ~raise Main_errors.expansion_tracer @@ compile e


let compile_type_expression ~raise
    : Ast_aggregated.type_expression -> Ast_expanded.type_expression
  =
 fun ty -> Trace.trace ~raise Main_errors.expansion_tracer @@ compile_type ty
