val use_new_typer : bool

open Trace

module I = Ast_core
module O = Ast_typed
module O' = Typesystem.Solver_types
module Environment = O.Environment
module Errors = Typer_common.Errors

module Solver = Typer_new.Solver

type environment = Environment.t

val type_program : I.program -> (O.program * Errors.typer_error O'.typer_state, Errors.typer_error) result
val type_expression_subst : environment -> Errors.typer_error O'.typer_state -> ?tv_opt:O.type_expression -> I.expression -> (O.expression * Errors.typer_error O'.typer_state , Errors.typer_error) result
val untype_expression : O.expression -> (I.expression , Errors.typer_error) result
val evaluate_type : environment -> O.ast_core_type_expression -> (O.type_expression, Errors.typer_error) result

val assert_type_expression_eq : O.type_expression * O.type_expression -> (unit, Errors.typer_error) result
