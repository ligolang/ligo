val use_new_typer : bool

open Trace

module I = Ast_core
module O = Ast_typed
module O' = Typesystem.Solver_types

module Environment = O.Environment

module Solver = Typer_new.Solver

type environment = Environment.t

val type_program : I.program -> (O.program * O'.typer_state) result
val type_expression_subst : environment -> O'.typer_state -> ?tv_opt:O.type_expression -> I.expression -> (O.expression * O'.typer_state) result
val untype_expression : O.expression -> I.expression result
