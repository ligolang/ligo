open Trace

module I = Ast_core
module O = Ast_typed
module O' = Typer_new.Solver              (* needs an instance of the solver now *)
module Environment = O.Environment
module Errors = Typer_common.Errors

module Solver = Typer_new.Solver

type environment = Environment.t

val force_new_typer : unit -> bool

val type_module : O.typer_switch -> init_env:environment -> I.module_ -> (environment * O.module_fully_typed * Errors.typer_error O'.typer_state, Errors.typer_error) result
val type_expression_subst : O.typer_switch -> environment -> Errors.typer_error O'.typer_state -> ?tv_opt:O.type_expression -> I.expression -> (O.environment * O.expression * Errors.typer_error O'.typer_state , Errors.typer_error) result
val untype_expression : O.typer_switch -> O.expression -> (I.expression , Errors.typer_error) result
val untype_module : O.typer_switch -> O.module_fully_typed -> (I.module_, Errors.typer_error) result
val evaluate_type : O.typer_switch -> environment -> O.ast_core_type_expression -> (O.type_expression, Errors.typer_error) result

val assert_type_expression_eq : Location.t -> O.type_expression * O.type_expression -> (unit, Errors.typer_error) result
