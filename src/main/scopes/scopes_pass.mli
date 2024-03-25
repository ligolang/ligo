type t = Env.def list Types.LMap.t

module Of_Ast : sig
  val declarations
    :  env_preload_decls:Ast_core.declaration list
    -> Ast_core.declaration list
    -> t
end

val inline_scopes : Env.Def.def_map -> t -> Types.inlined_scopes
