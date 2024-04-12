module AST = Ast_core

(** The result of running this pass. *)
type t

(** Collects all references from the provided AST. *)
val declarations : AST.declaration list -> Ast_core.ty_expr Types.LMap.t -> t

(** Maps each definition, appropriately updating the [references] field to contain all
    references found in [declarations]. *)
val patch : t -> Types.def list -> Types.def list
