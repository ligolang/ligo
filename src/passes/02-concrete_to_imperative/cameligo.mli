[@@@warning "-45"]

open Trace
open Ast_imperative

module Raw = Parser.Cameligo.AST
module SMap = Map.String
module Option = Simple_utils.Option

val npseq_to_nelist : 'a * ( 'b * 'c ) list -> 'a * 'c list 

val compile_expression : Raw.expr -> (expr, Errors_cameligo.abs_error) result

val compile_program : Raw.ast -> (program, Errors_cameligo.abs_error) result