(** Pretty printer for the Simplified Abstract Syntax Tree *)

open Types
open Format

(*
val list_sep_d : (formatter -> 'a -> unit) -> formatter -> 'a list -> unit

val smap_sep_d : (formatter -> 'a -> unit) -> formatter -> 'a Map.String.t -> unit

val type_expression : formatter -> type_expression -> unit
*)

val literal : formatter -> literal -> unit

val expression : formatter -> expression -> unit
(*
val option_type_name : formatter -> string * type_expression option -> unit
val assoc_expression : formatter -> (expr * expr) -> unit

val access : formatter -> access -> unit

val access_path : formatter -> access_path -> unit
*)

val type_annotation : formatter -> type_expression option -> unit 
val single_record_patch : formatter -> string * expr -> unit 

val single_tuple_patch : formatter -> int * expr -> unit
(*

val matching_variant_case : (formatter -> 'a -> unit) -> formatter -> (constructor_name * name) * 'a -> unit

val matching : (formatter -> 'a -> unit) -> formatter -> 'a matching -> unit
*)

(** Shows the type expected for the matched value *)
val matching_type : formatter -> 'a matching -> unit

(*
val matching_variant_case_type : formatter -> ( ( constructor_name * name) * 'a) -> unit

val declaration : formatter -> declaration -> unit

*)
(** Pretty print a full program AST *)
val program : formatter -> program -> unit
