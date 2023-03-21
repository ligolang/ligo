open Simple_utils
module LMap : Map.S with type key = Location.t

type t = Env.def list LMap.t

module Of_Ast : sig
  val declarations
    :  env_preload_decls:Ast_core.declaration list
    -> Ast_core.declaration list
    -> t
end

val to_old_scopes : Types.def list -> t -> Types.scopes
