module Formatter = Formatter
module Api_helper = Api_helper
module Trace = Simple_utils.Trace
module Types = Types
module PP = PP
module Uid = Types.Uid
module Misc = Misc

type def = Types.def
type definitions = Types.definitions
type scopes = Types.scopes
type inlined_scopes = Types.inlined_scopes

(** The result of running [Scopes.run]. *)
type t = Types.t =
  { definitions : definitions
        (** All the definitions collected during scoping. Runs all the scopes passes
            (except for [Types_pass] if [with_types] is [false]). *)
  ; program : Ast_typed.program option
        (** Result of type-checking. If [types_pass] is [false], will be [None]. *)
  ; inlined_scopes : inlined_scopes lazy_t
        (** Scoping result, used by the debugger. It's calculated lazily since this field
            is rarely used. *)
  ; lambda_types : Ast_typed.ty_expr Types.LMap.t
        (** A map of all labels whose types are functions. *)
  }

(** Runs the scopes pipeline and all its passes (except [Types_pass] if
    [with_types = false]. *)
val run
  :  raise:(Main_errors.all, Main_warnings.all) Trace.raise
  -> options:Compiler_options.middle_end
  -> stdlib:Ast_typed.program * Ast_core.program
  -> prg:Ast_core.module_
  -> module_deps:string Map.Make(String).t
  -> with_types:bool
  -> t

(** Calculates scopes for use in the debugger. *)
val inlined_scopes
  :  options:Compiler_options.middle_end
  -> stdlib:Ast_typed.program * Ast_core.program
  -> prg:Ast_core.module_
  -> definitions:definitions
  -> inlined_scopes

(** Calculates scopes for use in [ligo info get-scope]. *)
val scopes
  :  options:Compiler_options.middle_end
  -> stdlib:Ast_typed.program * Ast_core.program
  -> prg:Ast_core.module_
  -> scopes
