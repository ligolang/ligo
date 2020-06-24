open Trace

open Typer_common.Errors
module Errors = Typer_common.Errors
module I = Ast_core
module O = Ast_typed
module O' = Typesystem.Solver_types

module Environment = O.Environment

module Solver = Solver

type environment = Environment.t

val type_program : I.program -> (O.program * O'.typer_state, typer_error) result
val type_declaration : environment -> O'.typer_state -> I.declaration -> (environment * O'.typer_state * O.declaration option, typer_error) result
val evaluate_type : environment -> I.type_expression -> (O.type_expression, typer_error) result
val type_expression : environment -> O'.typer_state -> ?tv_opt:O.type_expression -> I.expression -> (O.expression * O'.typer_state, typer_error) result
val type_expression_subst : environment -> O'.typer_state -> ?tv_opt:O.type_expression -> I.expression -> (O.expression * O'.typer_state, typer_error) result
val type_constant : I.constant' -> O.type_expression list -> O.type_expression option -> (O.constant' * O.type_expression, typer_error) result

val untype_type_expression : O.type_expression -> (I.type_expression, typer_error) result
val untype_expression : O.expression -> (I.expression, typer_error) result
