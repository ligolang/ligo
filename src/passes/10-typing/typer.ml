let force_new_typer () = String.equal (match Sys.getenv_opt "LIGO_FORCE_NEW_TYPER" with Some e -> e | None -> "false") "true"
module I = Ast_core
module O = Ast_typed
module O' = Typer_new.Solver
module Errors = Typer_common.Errors

module Environment = O.Environment

module Solver = Typer_new.Solver (* Both the old typer and the new typer use the same solver state. *)

type environment = Environment.t

let is_new s = if force_new_typer () then true else match s with O.New -> true | O.Old -> false

(* let type_module = if use_new_typer then Typer_new.type_module else Typer_old.type_module *)
let type_module typer_switch = if is_new typer_switch then Typer_new.type_module else Typer_old.type_module
let type_expression_subst typer_switch = if is_new typer_switch then Typer_new.type_expression_subst else Typer_old.type_expression (* the old typer does not have unification variables that would need substitution, so no need to "subst" anything. *)
let untype_expression typer_switch = if is_new typer_switch  then Typer_new.untype_expression else Typer_old.untype_expression
let untype_module typer_switch =
  if is_new typer_switch
  then Typer_common.Untyper.untype_module Typer_new.untype_expression
  else Typer_common.Untyper.untype_module Typer_old.untype_expression

let evaluate_type typer_switch = if is_new typer_switch then Typer_new.evaluate_type else Typer_old.evaluate_type

let assert_type_expression_eq = Typer_common.Helpers.assert_type_expression_eq
