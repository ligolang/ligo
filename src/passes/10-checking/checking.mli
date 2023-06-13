module I = Ast_core
module O = Ast_typed
module Errors = Errors
open Errors
open Simple_utils.Trace

val type_program
  :  raise:(typer_error, Main_warnings.all) raise
  -> options:Compiler_options.middle_end
  -> ?env:O.signature
  -> I.program
  -> O.program

val type_program_with_signature
  :  raise:(typer_error, Main_warnings.all) raise
  -> options:Compiler_options.middle_end
  -> ?env:O.signature
  -> I.program
  -> O.program * O.signature

val type_declaration
  :  raise:(typer_error, Main_warnings.all) raise
  -> options:Compiler_options.middle_end
  -> ?env:O.signature
  -> I.declaration
  -> O.declaration list

val type_expression
  :  raise:(typer_error, Main_warnings.all) raise
  -> options:Compiler_options.middle_end
  -> ?env:O.signature
  -> ?tv_opt:O.type_expression
  -> I.expression
  -> O.expression

val untype_expression : O.expression -> I.expression
val untype_type_expression : ?use_orig_var:bool -> O.type_expression -> I.type_expression

val assert_type_expression_eq
  :  raise:(typer_error, Main_warnings.all) raise
  -> Location.t
  -> O.type_expression * O.type_expression
  -> unit
