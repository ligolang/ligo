module I = Ast_core
module O = Ast_typed
module Errors = Errors
module Type_var_name_tbl = Type.Type_var_name_tbl
module Trace = Simple_utils.Trace
module Location = Simple_utils.Location
module Refs_tbl = Context.Refs_tbl

val type_program_with_refs
  :  raise:(Errors.typer_error, Main_warnings.all) Trace.raise
  -> options:Compiler_options.middle_end
  -> refs_tbl:Refs_tbl.t
  -> ?env:O.signature
  -> I.program
  -> O.program

val type_program
  :  raise:(Errors.typer_error, Main_warnings.all) Trace.raise
  -> options:Compiler_options.middle_end
  -> ?env:O.signature
  -> I.program
  -> O.program

val type_declaration
  :  raise:(Errors.typer_error, Main_warnings.all) Trace.raise
  -> options:Compiler_options.middle_end
  -> path:Ligo_prim.Module_var.t list
  -> ?env:O.signature
  -> I.declaration
  -> O.declaration list

val type_expression
  :  raise:(Errors.typer_error, Main_warnings.all) Trace.raise
  -> options:Compiler_options.middle_end
  -> path:Ligo_prim.Module_var.t list
  -> ?env:O.signature
  -> ?tv_opt:O.type_expression
  -> I.expression
  -> O.expression

val type_type_expression
  :  raise:(Errors.typer_error, Main_warnings.all) Trace.raise
  -> options:Compiler_options.middle_end
  -> path:Ligo_prim.Module_var.t list
  -> ?env:O.signature
  -> I.type_expression
  -> O.type_expression

val eval_signature_sort
  :  raise:(Errors.typer_error, Main_warnings.all) Trace.raise
  -> options:Compiler_options.middle_end
  -> loc:Location.t
  -> path:Ligo_prim.Module_var.t list
  -> ?env:O.signature
  -> O.signature
  -> O.sig_sort

val untype_expression
  :  raise:(Errors.typer_error, Main_warnings.all) Trace.raise
  -> O.expression
  -> I.expression

(** Decompile an [O.type_expression] into an [I.type_expression]. [use_orig_var] ([false]
    if not set) tries to preserve the original type name, rather than using its body. This
    means that in case of [type t = u], then [t] should be used rather than [u]. *)
val untype_type_expression
  :  raise:(Errors.typer_error, Main_warnings.all) Trace.raise
  -> ?use_orig_var:bool
  -> O.type_expression
  -> I.type_expression

(** Decompile an [O.signature] into an [I.signature]. [use_orig_var] ([false] if not set)
    tries to preserve the original type names, rather than using their bodies. This means
    that in case of [type t = u], then [t] should be used rather than [u]. *)
val untype_signature
  :  raise:(Errors.typer_error, Main_warnings.all) Trace.raise
  -> ?use_orig_var:bool
  -> O.signature
  -> I.signature

val assert_type_expression_eq
  :  raise:(Errors.typer_error, Main_warnings.all) Trace.raise
  -> Location.t
  -> O.type_expression * O.type_expression
  -> unit

(* Utils *)
val loc_of_program : 'a Location.wrap list -> Location.t
