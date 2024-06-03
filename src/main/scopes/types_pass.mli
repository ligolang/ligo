module Trace = Simple_utils.Trace
module Location = Simple_utils.Location

(** The state (and final result) of running [resolve]. *)
type t =
  { type_cases : Types.type_case Location.Map.t
        (** A map relating locations of each definition to their types. *)
  ; label_cases : Ast_core.ty_expr Location.Map.t
        (** A map relating locations of each label definition to their types. *)
  ; lambda_cases : Ast_typed.ty_expr Location.Map.t
        (** A map relating locations of each function definition to their types. *)
  ; module_signatures : Types.signature_case Location.Map.t
        (** A map relating locations of each module definition to their signatures. *)
  ; module_env : Env.Env_map.t (** A module environment used to resolve module paths. *)
  ; refs_tbl : Checking.Refs_tbl.t (** A hashtable with references. *)
  }

(** Creates an empty [t], using the provided module environment. *)
val empty : Env.Env_map.t -> t

(** Runs the typer on the provided [Ast_core.program].

    @param stdlib_decls The typed AST for the stdlib.
    @param module_env A module environment used to resolve module paths, obtained from
    the [Module_aliases_pass].
    @returns The typing result of this pass [t] and the type-checked [Ast_typed.program]. *)
val resolve
  :  raise:(Main_errors.all, Main_warnings.all) Trace.raise
  -> options:Compiler_options.middle_end
  -> stdlib_decls:Ast_typed.program
  -> module_env:Env.Env_map.t
  -> Ast_core.program
  -> t * Ast_typed.program

module Of_Ast_core : sig
  (** Fill the state [t] for the provided program. *)
  val program
    :  raise:(Main_errors.all, Main_warnings.all) Trace.raise
    -> t
    -> Ast_typed.signature
    -> Ast_core.program
    -> t
end

(** Maps each definition, appropriately updating the [references] field to contain all
    references found in [declarations] and replacing each [vdef]'s [type_case] and [mdef]'s
    [signature_case] with the annotated/inferred types/signatures, if available. *)
val patch : t -> Types.def list -> Types.def list
