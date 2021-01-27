open Trace

open Typer_common.Errors
module Errors = Typer_common.Errors
module I = Ast_core
module O = Ast_typed
module O' = Solver              (* needs an instance of the solver now *)

module Environment = O.Environment

module Solver = Solver

type environment = Environment.t

val type_module : init_env:environment -> I.module_ -> (environment * O.module_fully_typed * typer_error O'.typer_state, typer_error) result
val type_declaration : environment -> typer_error O'.typer_state -> I.declaration Location.wrap -> (environment * typer_error O'.typer_state * O.declaration Location.wrap, typer_error) result
val evaluate_type : environment -> I.type_expression -> (O.type_expression, typer_error) result
val type_expression : ?tv_opt:O.type_expression -> environment -> typer_error O'.typer_state -> I.expression -> (environment * typer_error O'.typer_state * O.expression, typer_error) result
val type_expression_subst : environment -> typer_error O'.typer_state -> ?tv_opt:O.type_expression -> I.expression -> (environment * O.expression * typer_error O'.typer_state, typer_error) result

val untype_expression : O.expression -> (I.expression, typer_error) result