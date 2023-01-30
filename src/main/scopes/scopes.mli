module AST = Ast_core
module Formatter = Formatter
module Api_helper = Api_helper
module Trace = Simple_utils.Trace

module PP = PP

type def = Types.def
type scopes = Types.scopes

val scopes
  :  raise:(Main_errors.all, Main_warnings.all) Trace.raise
  -> with_types:bool
  -> options:Compiler_options.middle_end
  -> stdlib:Ast_typed.program * Ast_core.program
  -> AST.module_
  -> def list * scopes
