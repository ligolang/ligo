module AST = Ast_core

(** The result of running this pass. This a map from the locations of each module name to
    their resolved module paths, and resolved modules (pointing to a definition). *)
type t = (Types.Uid.t list * Types.Uid.t) Types.LMap.t

(** [declarations] takes each declaration and fills [t] with all resolved module aliases.
    It also fills an [Env.Env_map.t], for use in other passes. *)
val declarations : AST.declaration list -> t * Env.Env_map.t

(** [patch] maps each module alias in the list of definitions. It looks for module aliases
    definitions, replacing them with their resolved modules. *)
val patch : t -> Types.def list -> Types.def list
