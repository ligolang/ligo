open Trace
open Types

val program_to_main : program -> string -> lambda result

module Captured_variables : sig

  type bindings = string list
  val matching : (bindings -> 'a -> bindings result) -> bindings -> 'a matching -> bindings result

  val matching_expression : bindings -> matching_expr -> bindings result

  val mem : string -> bindings -> bool
(*
  val singleton : string -> bindings
  val union : bindings -> bindings -> bindings
  val unions : bindings list -> bindings
  val empty : bindings
  val of_list : string list -> bindings

  val annotated_expression : bindings -> annotated_expression -> bindings result
  val matching_variant_case : (bindings -> 'a -> bindings result) -> bindings -> ((constructor_name * name) * 'a) -> bindings result

*)
end
