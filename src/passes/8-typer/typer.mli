val use_new_typer : bool

open Trace

module I = Ast_simplified
module O = Ast_typed

module Environment = O.Environment

module Solver = Typer_new.Solver

type environment = Environment.t

val type_program : I.program -> (O.program * Solver.state) result
val type_expression_subst : environment -> Solver.state -> ?tv_opt:O.type_expression -> I.expression -> (O.expression * Solver.state) result
val untype_expression : O.expression -> I.expression result
