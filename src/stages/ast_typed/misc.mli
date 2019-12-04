open Trace
open Types

val assert_value_eq : ( expression * expression ) -> unit result

val assert_type_expression_eq : ( type_expression * type_expression ) -> unit result

val merge_annotation : type_expression option -> type_expression option -> error_thunk -> type_expression result

(* No information about what made it fail *)
val type_expression_eq : ( type_expression * type_expression ) -> bool

module Free_variables : sig
  type bindings = expression_variable list

  val matching_expression : bindings -> matching_expr -> bindings
  val lambda : bindings -> lambda -> bindings

  val expression : bindings -> expression -> bindings 

  val empty : bindings 
  val singleton : expression_variable -> bindings 

(*
  val mem : string -> bindings -> bool
  val union : bindings -> bindings -> bindings
  val unions : bindings list -> bindings
  val of_list : string list -> bindings

  val expression : bindings -> expression -> bindings

  val matching_variant_case : (bindings -> 'a -> bindings) -> bindings -> ((constructor_name * name) * 'a) -> bindings

  val matching : (bindings -> 'a -> bindings) -> bindings -> 'a matching -> bindings

  *)
end

module Errors : sig
  (*
  val different_kinds : type_expression -> type_expression -> unit -> error
  val different_constants : string -> string -> unit -> error
  val different_size_type : name -> type_expression -> type_expression -> unit -> error
  val different_props_in_record : string -> string -> unit -> error
  val different_size_constants : type_expression -> type_expression -> unit -> error
  val different_size_tuples : type_expression -> type_expression -> unit -> error
  val different_size_sums : type_expression -> type_expression -> unit -> error
  val different_size_records : type_expression -> type_expression -> unit -> error
  val different_types : name -> type_expression -> type_expression -> unit -> error
  val different_literals : name -> literal -> literal -> unit -> error
  val different_values : name -> value -> value -> unit -> error
  val different_literals_because_different_types : name -> literal -> literal -> unit -> error
  val different_values_because_different_types : name -> value -> value -> unit -> error
  val error_uncomparable_literals : name -> literal -> literal -> unit -> error
  val error_uncomparable_values : name -> value -> value -> unit -> error
  val different_size_values : name -> value -> value -> unit -> error
  val missing_key_in_record_value : string -> unit -> error
  *)
  val not_functional_main : Location.t -> unit -> error
end




(*
val assert_literal_eq : ( literal * literal ) -> unit result
*)

val get_entry : program -> string -> expression result
val program_environment : program -> full_environment
