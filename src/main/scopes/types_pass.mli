open Simple_utils
module LMap : Map.S with type key = Location.t

type t = Types.type_case LMap.t

val empty : t

module Typing_env : sig
  type nonrec t =
    { (* type_env is the global typing signature required by the typer *)
      type_env : Ast_typed.signature
    ; (* bindings is Map from [Location.t] -> [Types.type_case] *)
      bindings : t
    ; (* Top-level declaration tree used for ast-typed-self-passes *)
      decls : Ast_typed.declaration list
    }
end

val resolve
  :  raise:(Main_errors.all, Main_warnings.all) Trace.raise
  -> options:Compiler_options.middle_end
  -> stdlib_decls:Ast_typed.program
  -> Ast_core.program
  -> Typing_env.t

module Of_Ast_core : sig
  val declarations : t -> Ast_core.declaration list -> t
end

val patch : t -> Types.def list -> Types.def list
