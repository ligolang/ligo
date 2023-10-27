open Ligo_prim
module Location = Simple_utils.Location
open Simple_utils.Trace

type 'a t
type error = Errors.typer_error
type warning = Main_warnings.all

include Monad.S with type 'a t := 'a t

val all_lmap : 'a t Label.Map.t -> 'a Label.Map.t t
val all_lmap_unit : unit t Label.Map.t -> unit t

include module type of Let_syntax

val decode_signature : Context.Signature.t -> Ast_typed.signature t
val decode_sig_sort : Context.Signature.sort -> Ast_typed.sig_sort t
val decode : Type.t -> Ast_typed.type_expression t

val check_anomalies
  :  syntax:Syntax_types.t option
  -> loc:Location.t
  -> (Ast_typed.type_expression Ast_typed.Pattern.t * Ast_typed.type_expression) list
  -> Ast_typed.type_expression
  -> unit t

val run : 'a t -> raise:(error, warning) raise -> Substitution.t -> 'a
