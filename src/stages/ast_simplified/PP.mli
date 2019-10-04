(** Pretty printer for the Simplified Abstract Syntax Tree *)

open Types

val type_expression : Format.formatter -> type_expression -> unit

val literal : Format.formatter -> literal -> unit

val expression : Format.formatter -> expression -> unit

val option_type_name : Format.formatter -> string * type_expression option -> unit

val assoc_expression : Format.formatter -> (expr * expr) -> unit 

val access : Format.formatter -> access -> unit

val access_path : Format.formatter -> access_path -> unit

val type_annotation : Format.formatter -> type_expression option -> unit

val single_record_patch : Format.formatter -> string * expr -> unit

val single_tuple_patch : Format.formatter -> int * expr -> unit

(* Shows the type expected for the matched value *)
val matching_type : Format.formatter -> 'a matching -> unit

val matching_variant_case_type : Format.formatter -> (string * string) * 'a -> unit

val declaration : Format.formatter -> declaration -> unit

(** Pretty print a full program AST *)
val program : Format.formatter -> program -> unit
