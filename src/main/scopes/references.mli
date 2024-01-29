open Ligo_prim
module AST = Ast_core
module LSet = Types.LSet
module LMap = Types.LMap

module References : sig
  type references = LSet.t LMap.t

  type t =
    { references : references
    ; type_var_loc_to_label_refs : LSet.t Label.Map.t LMap.t
    }

  type label_types = Ast_core.ty_expr LMap.t
end

type references = References.t
type label_types = References.label_types

val declarations : AST.declaration list -> label_types -> references
val patch : references -> Types.def list -> Types.def list
