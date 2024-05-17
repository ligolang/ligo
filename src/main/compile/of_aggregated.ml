open Expansion
module SMap = Map.Make (String)

let compile_expression ~raise : Ast_aggregated.expression -> Ast_expanded.expression =
 fun e ->
  ignore raise;
  compile e


let compile_type_expression ~raise
    : Ast_aggregated.type_expression -> Ast_expanded.type_expression
  =
 fun ty ->
  ignore raise;
  compile_type ty
