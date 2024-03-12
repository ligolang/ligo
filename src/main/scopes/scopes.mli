module Formatter = Formatter
module Api_helper = Api_helper
module Trace = Simple_utils.Trace
module Types = Types
module PP = PP
module Uid = Types.Uid
module Misc = Misc
module Subst = Types.Subst

type def = Types.def
type definitions = Types.definitions
type scopes = Types.scopes
type inlined_scopes = Types.inlined_scopes

type t = Types.t =
  { definitions : definitions
  ; program : Ast_typed.program option
  ; subst : Subst.t
  ; inlined_scopes : inlined_scopes lazy_t
  ; lambda_types : Ast_typed.ty_expr Types.LMap.t
  }

val run
  :  raise:(Main_errors.all, Main_warnings.all) Trace.raise
  -> options:Compiler_options.middle_end
  -> stdlib:Ast_typed.program * Ast_core.program
  -> prg:Ast_core.module_
  -> module_deps:string Map.Make(String).t
  -> with_types:bool
  -> t

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
