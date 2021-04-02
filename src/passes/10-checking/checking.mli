open Errors
open Trace

module I = Ast_core
module O = Ast_typed

module Errors = Errors
module Environment = O.Environment

type environment = Environment.t

val type_module     : init_env:environment -> I.module_ -> (environment * O.module_fully_typed, typer_error) result
val type_declaration : environment -> I.declaration Location.wrap -> (environment * O.declaration Location.wrap , typer_error) result
val evaluate_type : environment -> I.type_expression -> (O.type_expression , typer_error) result
val type_expression : environment -> ?tv_opt:O.type_expression -> I.expression -> (O.environment * O.expression , typer_error) result
val type_constant : I.constant' -> Location.t -> O.type_expression list -> O.type_expression option -> (O.constant' * O.type_expression , typer_error) result
(*
val untype_type_value : O.type_value -> (I.type_expression) result
val untype_literal : O.literal -> I.literal result
*)
val untype_expression : O.expression -> (I.expression , typer_error) result
(*
val untype_matching : ('o -> 'i result) -> 'o O.matching -> ('i I.matching) result
*)

val untype_module_fully_typed : O.module_fully_typed -> (I.module_, typer_error) result

val assert_type_expression_eq : Location.t -> O.type_expression * O.type_expression -> (unit, typer_error) result
val decompile_env : O.Environment.t -> (I.Environment.t, typer_error) result