module AST = Ast_core

val definitions : AST.program -> Types.def list -> Types.def list

module Of_Stdlib : sig
  val definitions : AST.program -> Types.def list
end
