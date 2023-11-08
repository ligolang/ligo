open Simple_utils
module LMap : Map.S with type key = Location.t

type t =
  { type_cases : Types.type_case LMap.t
  ; module_signatures : Types.signature_case LMap.t
  ; module_env : Env.t
  }

val empty : Env.t -> t

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
  -> module_env:Env.t
  -> Ast_core.program
  -> Typing_env.t

module Of_Ast_core : sig
  val declarations : t -> Ast_typed.signature -> Ast_core.declaration list -> t
end

val patch : t -> Types.def list -> Types.def list
