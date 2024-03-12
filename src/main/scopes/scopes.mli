module Formatter = Formatter
module Api_helper = Api_helper
module Trace = Simple_utils.Trace
module Types = Types
module PP = PP
module Uid = Types.Uid
module Misc = Misc

type def = Types.def
type definitions = Types.definitions
type scopes = Types.scopes
type inlined_scopes = Types.inlined_scopes

val defs_and_typed_program
  :  raise:(Main_errors.all, Main_warnings.all) Trace.raise
  -> options:Compiler_options.middle_end
  -> stdlib:Ast_typed.program * Ast_core.program
  -> prg:Ast_core.module_
  -> module_deps:string Map.Make(String).t
  -> with_types:bool
  -> definitions
     * (Ast_typed.signature * Ast_typed.declaration list) option
     * Ast_typed.ty_expr Types.LMap.t

val inlined_scopes
  :  options:Compiler_options.middle_end
  -> stdlib:Ast_typed.program * Ast_core.program
  -> prg:Ast_core.module_
  -> definitions:definitions
  -> inlined_scopes

val scopes
  :  options:Compiler_options.middle_end
  -> stdlib:Ast_typed.program * Ast_core.program
  -> prg:Ast_core.module_
  -> scopes

val defs_and_typed_program_and_scopes
  :  raise:(Main_errors.all, Main_warnings.all) Trace.raise
  -> options:Compiler_options.middle_end
  -> stdlib:Ast_typed.program * Ast_core.program
  -> prg:Ast_core.module_
  -> module_deps:string Map.Make(String).t
  -> with_types:bool
  -> definitions
     * (Ast_typed.signature * Ast_typed.declaration list) option
     * inlined_scopes
