module AST = Ast_core

val definitions
  :  AST.program
  -> string Map.Make(String).t
  -> Types.def list
  -> Types.def list

module Of_Stdlib : sig
  val definitions : AST.program -> string Map.Make(String).t -> Types.def list
end
