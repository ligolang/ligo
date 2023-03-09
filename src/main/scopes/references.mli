open Simple_utils
module AST = Ast_core
module LSet = Types.LSet
module LMap : Map.S with type key = Location.t

type references = LSet.t LMap.t

val declarations : AST.declaration list -> references