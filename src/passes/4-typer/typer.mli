val use_new_typer : bool

open Trace

module I = Ast_simplified
module O = Ast_typed

module SMap = O.SMap
module Environment = O.Environment

module Solver = Typer_new.Solver

type environment = Environment.t

val type_program : I.program -> (O.program * Solver.state) result
val type_expression : environment -> Solver.state -> ?tv_opt:O.type_value -> I.expression -> (O.annotated_expression * Solver.state) result
val untype_expression : O.annotated_expression -> I.expression result

