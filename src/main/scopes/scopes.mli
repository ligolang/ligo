module Formatter = Formatter
module Api_helper = Api_helper
module Trace = Simple_utils.Trace
module Types = Types
module PP = PP

type def = Types.def
type scopes = Types.scopes

val defs_and_typed_program
  :  raise:(Main_errors.all, Main_warnings.all) Trace.raise
  -> options:Compiler_options.middle_end
  -> stdlib:Ast_typed.program * Ast_core.program
  -> prg:Ast_core.module_
  -> module_deps:string Map.Make(String).t
  -> with_types:bool
  -> def list * (Ast_typed.signature * Ast_typed.declaration list) option

val scopes
  :  options:Compiler_options.middle_end
  -> stdlib:Ast_typed.program * Ast_core.program
  -> prg:Ast_core.module_
  -> module_deps:string Map.Make(String).t
  -> definitions:def list
  -> scopes

val defs_and_typed_program_and_scopes
  :  raise:(Main_errors.all, Main_warnings.all) Trace.raise
  -> options:Compiler_options.middle_end
  -> stdlib:Ast_typed.program * Ast_core.program
  -> prg:Ast_core.module_
  -> module_deps:string Map.Make(String).t
  -> with_types:bool
  -> def list * (Ast_typed.signature * Ast_typed.declaration list) option * scopes
