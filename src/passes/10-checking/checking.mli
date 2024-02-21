module I = Ast_core
module O = Ast_typed
module Errors = Errors
module Type_var_name_tbl = Type.Type_var_name_tbl
open Errors
open Simple_utils.Trace

val type_program
  :  raise:(typer_error, Main_warnings.all) raise
  -> options:Compiler_options.middle_end
  -> ?env:O.signature
  -> I.program
  -> O.program

val type_declaration
  :  raise:(typer_error, Main_warnings.all) raise
  -> options:Compiler_options.middle_end
  -> path:Ligo_prim.Module_var.t list
  -> ?env:O.signature
  -> I.declaration
  -> O.declaration list

val type_expression
  :  raise:(typer_error, Main_warnings.all) raise
  -> options:Compiler_options.middle_end
  -> path:Ligo_prim.Module_var.t list
  -> ?env:O.signature
  -> ?tv_opt:O.type_expression
  -> I.expression
  -> O.expression

val type_type_expression
  :  raise:(typer_error, Main_warnings.all) raise
  -> options:Compiler_options.middle_end
  -> path:Ligo_prim.Module_var.t list
  -> ?env:O.signature
  -> I.type_expression
  -> O.type_expression

val eval_signature_sort
  :  raise:(typer_error, Main_warnings.all) raise
  -> options:Compiler_options.middle_end
  -> loc:Location.t
  -> path:Ligo_prim.Module_var.t list
  -> ?env:O.signature
  -> O.signature
  -> O.sig_sort

val untype_expression : O.expression -> I.expression

(** Decompile an [O.type_expression] into an [I.type_expression]. [use_orig_var] ([false]
    if not set) tries to preserve the original type name, rather than using its body. This
    means that in case of [type t = u], then [t] should be used rather than [u]. *)
val untype_type_expression : ?use_orig_var:bool -> O.type_expression -> I.type_expression

(** Decompile an [O.signature] into an [I.signature]. [use_orig_var] ([false] if not set)
    tries to preserve the original type names, rather than using their bodies. This means
    that in case of [type t = u], then [t] should be used rather than [u]. *)
val untype_signature : ?use_orig_var:bool -> O.signature -> I.signature

val assert_type_expression_eq
  :  raise:(typer_error, Main_warnings.all) raise
  -> Location.t
  -> O.type_expression * O.type_expression
  -> unit

(* Utils *)
val loc_of_program : 'a Location.wrap list -> Location.t
