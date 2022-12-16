module AST = Ast_core
module Formatter = Formatter
module Api_helper = Api_helper

type def = Types.def
type scopes = Types.scopes

val scopes
  :  with_types:bool
  -> options:Compiler_options.middle_end
  -> stdlib:Ast_typed.program
  -> AST.module_
  -> def list * scopes
