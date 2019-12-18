open Trace

module AST = Ast_typed
module Append_tree = Tree.Append
open Mini_c

val temp_unwrap_loc : 'a Location.wrap -> 'a
(*
val temp_unwrap_loc_list : AST.declaration Location.wrap list -> AST.declaration list
val list_of_map : 'a AST.type_name_map -> 'a list
val kv_list_of_map : 'a AST.type_name_map -> ( string * 'a ) list
val map_of_kv_list : ( string * 'a ) list -> 'a AST.type_name_map
*)

module Errors : sig
  (*
  val corner_case : loc:string -> string -> unit -> error
  val unrecognized_type_constant : string -> unit -> error
  val row_loc : Location.t -> string * ( unit -> string )
  val unsupported_pattern_matching : string -> Location.t -> unit -> error
  val unsupported_iterator : Location.t -> unit -> error
  *)
  val not_functional_main : Location.t -> unit -> error
  val missing_entry_point : string -> unit -> error
  val wrong_mini_c_value : string -> value -> unit -> error
  val bad_untranspile : string -> value -> unit -> error
  val unknown_untranspile : string -> value -> unit -> error
end

(*
val translate_type : AST.type_value -> type_value result
val tuple_access_to_lr : type_value -> type_value list -> int -> (type_value * [`Left | `Right]) list result
val record_access_to_lr : type_value -> type_value AST.type_name_map -> string -> (type_value * [`Left | `Right]) list result 
val translate_literal : AST.literal -> value
val transpile_environment_element_type : AST.environment_element -> type_value result
val tree_of_sum : AST.type_value -> (type_name * AST.type_value) Append_tree.t result
*)
val transpile_annotated_expression : AST.annotated_expression -> expression result
(*
val transpile_lambda : AST.lambda -> expression result
val transpile_declaration : environment -> AST.declaration -> toplevel_statement result
*)

val transpile_program : AST.program -> program result
val check_storage : anon_function -> 'a -> Location.t -> (anon_function * 'a) result
(*
val translate_main : AST.lambda -> Location.t ->( anon_function * ( type_value * type_value )) result 

(* From an expression [expr], build the expression [fun () -> expr] *)
val translate_entry : AST.program -> string -> ( anon_function * ( type_value * type_value )) result
*)
val extract_constructor : value -> ( string * AST.type_value ) Append_tree.t' -> (string * value * AST.type_value) result
val extract_tuple : value -> AST.type_value Append_tree.t' -> (value * AST.type_value) list result
val extract_record : value -> ( string * AST.type_value ) Append_tree.t' -> ( string * ( value * AST.type_value )) list result
val untranspile : value -> AST.type_value -> AST.annotated_expression result
