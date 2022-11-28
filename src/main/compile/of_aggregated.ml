open Expansion

module SMap = Map.Make(String)

let compile_expression ~raise : Ast_aggregated.expression -> Ast_expanded.expression = fun e ->
  ignore raise ;
  compile e
