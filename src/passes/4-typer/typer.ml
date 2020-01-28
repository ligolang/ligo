let use_new_typer = false

module I = Ast_simplified
module O = Ast_typed

module Environment = O.Environment

module Solver = Typer_new.Solver (* Both the old typer and the new typer use the same solver state. *)

type environment = Environment.t

let type_program = if use_new_typer then Typer_new.type_program else Typer_old.type_program
let type_expression_subst = if use_new_typer then Typer_new.type_expression_subst else Typer_old.type_expression (* the old typer does not have unification variables that would need substitution, so no need to "subst" anything. *)
let untype_expression = if use_new_typer then Typer_new.untype_expression else Typer_old.untype_expression
