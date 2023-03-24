open Simple_utils
module AST = Ast_core
module LSet = Types.LSet
module LMap : Map.S with type key = Location.t

type t = string list LMap.t

val declarations : AST.declaration list -> t
val patch : t -> Types.def list -> Types.def list
