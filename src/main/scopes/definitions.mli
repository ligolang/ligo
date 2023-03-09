module AST = Ast_core

val definitions : AST.program -> Types.def list -> Types.def list

module Merge_defs_temp : sig
  val merge_defs : Types.def list -> Types.def list -> Types.def list
end
