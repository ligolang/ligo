open Simple_utils
module LMap = Types.LMap

type t =
  { type_cases : Types.type_case LMap.t
  ; label_cases : Ast_core.ty_expr LMap.t
  ; lambda_cases : Ast_typed.ty_expr LMap.t
  ; module_signatures : Types.signature_case LMap.t
  ; module_env : Env.Env_map.t
  }

val empty : Env.Env_map.t -> t

val resolve
  :  raise:(Main_errors.all, Main_warnings.all) Trace.raise
  -> options:Compiler_options.middle_end
  -> stdlib_decls:Ast_typed.program
  -> module_env:Env.Env_map.t
  -> Ast_core.program
  -> t * Ast_typed.program

module Of_Ast_core : sig
  val program
    :  raise:(Main_errors.all, Main_warnings.all) Trace.raise
    -> t
    -> Ast_typed.signature
    -> Ast_core.program
    -> t
end

val patch : t -> Types.def list -> Types.def list
