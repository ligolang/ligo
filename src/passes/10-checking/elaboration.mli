open Ligo_prim
module Location = Simple_utils.Location
module Trace = Simple_utils.Trace

type 'a t
type error = Errors.typer_error
type warning = Main_warnings.all

include Monad.S with type 'a t := 'a t

val all_lmap : 'a t Label.Map.t -> 'a Label.Map.t t
val all_lmap_unit : unit t Label.Map.t -> unit t

include module type of Let_syntax

val decode_signature : Context.Signature.t -> Ast_typed.signature t
val decode_sig_sort : Context.Signature.sort -> Ast_typed.sig_sort t
val decode_row : Type.row -> Ast_typed.type_expression Ast_typed.Row.t t
val decode : Type.t -> Ast_typed.type_expression t

(** Under normal circumstances, the typer will halt if the scrutinee's type and the cases
    types do not match. However, if error recovery is enabled, the typer will continue,
    and will try to perform anomaly analysis while having a different pattern and
    scrutinee type, and might reach some raises. If we reached an error and typer error
    recovery is enabled, we recover from this error. *)
val check_anomalies
  :  syntax:Syntax_types.t option
  -> loc:Location.t
  -> (Ast_typed.type_expression Ast_typed.Pattern.t * Ast_typed.type_expression) list
  -> Ast_typed.type_expression
  -> unit t

val run
  :  'a t
  -> options:Compiler_options.middle_end
  -> path:Module_var.t list
  -> raise:(error, warning) Trace.raise
  -> Substitution.t
  -> 'a
