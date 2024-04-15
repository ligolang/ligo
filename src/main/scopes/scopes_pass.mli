(** The result of running this pass. Each location in the map represents the range of a
    scope, and the values represent the definitions that are in-scope. *)
type t = Env.def list Types.LMap.t

module Of_Ast : sig
  (** Collects scopes from the provided AST.

      @param env_preload_decls Allows preloading the environment with stdlib declarations. *)
  val declarations
    :  env_preload_decls:Ast_core.declaration list
    -> Ast_core.declaration list
    -> t
end

(** Inlines definitions in the scopes list. Used by the debugger. *)
val inline_scopes : Env.def_map -> t -> Types.inlined_scopes
