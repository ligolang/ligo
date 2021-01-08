open Typer_common.Errors
open Trace

module I = Ast_core
module O = Ast_typed
module O' = Typer_new.Solver              (* needs an instance of the solver now *)

module Environment = O.Environment

module Solver : module type of Typer_new.Solver

type environment = Environment.t

val type_module     : init_env:environment -> I.module_ -> (environment * O.module_fully_typed * typer_error O'.typer_state, typer_error) result
val type_declaration : environment -> typer_error O'.typer_state -> I.declaration Location.wrap -> (environment * typer_error O'.typer_state * O.declaration Location.wrap , typer_error) result
val evaluate_type : environment -> I.type_expression -> (O.type_expression , typer_error) result
val type_expression : environment -> typer_error O'.typer_state -> ?tv_opt:O.type_expression -> I.expression -> (O.environment * O.expression * typer_error O'.typer_state , typer_error) result
val type_constant : I.constant' -> Location.t -> O.type_expression list -> O.type_expression option -> (O.constant' * O.type_expression , typer_error) result
(*
val untype_type_value : O.type_value -> (I.type_expression) result
val untype_literal : O.literal -> I.literal result
*)
val untype_expression : O.expression -> (I.expression , typer_error) result
(*
val untype_matching : ('o -> 'i result) -> 'o O.matching -> ('i I.matching) result
*)
