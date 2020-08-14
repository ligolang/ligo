open Trace

open Typer_common.Errors
module Errors = Typer_common.Errors
module I = Ast_core
module O = Ast_typed
module O' = Typesystem.Solver_types

module Environment = O.Environment

module Solver = Solver

type environment = Environment.t

val type_program : I.program -> (O.program * typer_error O'.typer_state, typer_error) result
val type_declaration : environment -> typer_error O'.typer_state -> I.declaration -> (environment * typer_error O'.typer_state * O.declaration, typer_error) result
val evaluate_type : environment -> I.type_expression -> (O.type_expression, typer_error) result
val type_expression : ?tv_opt:O.type_expression -> environment -> typer_error O'.typer_state -> I.expression -> (typer_error O'.typer_state * O.expression, typer_error) result
val type_expression_subst : environment -> typer_error O'.typer_state -> ?tv_opt:O.type_expression -> I.expression -> (O.expression * typer_error O'.typer_state, typer_error) result

val untype_type_expression : O.type_expression -> (I.type_expression, typer_error) result
val untype_expression : O.expression -> (I.expression, typer_error) result
