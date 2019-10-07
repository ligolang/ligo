open Types
open Simple_utils.Trace
(*
module Option = Simple_utils.Option

module SMap = Map.String

module Errors : sig
  val bad_kind : name -> Location.t -> unit -> error
end
*)
val t_bool      : type_expression
val t_string    : type_expression
val t_bytes     : type_expression
val t_int       : type_expression
val t_operation : type_expression
val t_nat       : type_expression
val t_tez       : type_expression
val t_unit      : type_expression
val t_address   : type_expression
(*
val t_option    : type_expression -> type_expression
*)
val t_list      : type_expression -> type_expression
val t_variable  : type_name -> type_expression
(*
val t_tuple     : type_expression list -> type_expression
val t_record    : te_map -> type_expression
*)
val t_pair : ( type_expression * type_expression ) -> type_expression

val t_record_ez : (string * type_expression) list -> type_expression

val t_sum : te_map -> type_expression
val ez_t_sum : ( string * type_expression ) list -> type_expression

val t_function : type_expression -> type_expression -> type_expression
val t_map : type_expression -> type_expression -> type_expression
(*
val t_set : type_expression -> type_expression

val make_name : string -> name

*)
val e_var : ?loc:Location.t -> string -> expression
val e_literal : ?loc:Location.t -> literal -> expression
val e_unit : ?loc:Location.t -> unit -> expression
val e_int : ?loc:Location.t -> int -> expression 
val e_nat : ?loc:Location.t -> int -> expression
val e_timestamp : ?loc:Location.t -> int -> expression
val e_bool : ?loc:Location.t -> bool -> expression
val e_string : ?loc:Location.t -> string -> expression
val e_address : ?loc:Location.t -> string -> expression 
val e_mutez : ?loc:Location.t -> int -> expression
val e'_bytes : string -> expression' result
val e_bytes : ?loc:Location.t -> string -> expression result
val e_big_map : ?loc:Location.t -> ( expr * expr ) list -> expression
(*
val e_record  : ?loc:Location.t -> ( expr * expr ) list -> expression
*)
val e_tuple : ?loc:Location.t -> expression list -> expression
val e_some : ?loc:Location.t -> expression -> expression
val e_none : ?loc:Location.t -> unit -> expression
val e_string_cat : ?loc:Location.t -> expression -> expression -> expression
val e_map_add : ?loc:Location.t -> expression -> expression ->  expression -> expression
val e_map : ?loc:Location.t -> ( expression * expression ) list -> expression
val e_set : ?loc:Location.t -> expression list -> expression
val e_list : ?loc:Location.t -> expression list -> expression
val e_pair : ?loc:Location.t -> expression -> expression -> expression
val e_constructor : ?loc:Location.t -> name -> expression -> expression
val e_matching : ?loc:Location.t -> expression -> matching_expr -> expression
val e_matching_bool : ?loc:Location.t -> expression -> expression -> expression -> expression
val e_accessor : ?loc:Location.t -> expression -> access_path -> expression
val e_accessor_props : ?loc:Location.t -> expression -> name list -> expression
val e_variable : ?loc:Location.t -> name -> expression
val e_skip : ?loc:Location.t -> unit -> expression
val e_loop : ?loc:Location.t -> expression -> expression -> expression
val e_sequence : ?loc:Location.t -> expression -> expression -> expression
val e_let_in : ?loc:Location.t -> ( name * type_expression option ) -> expression -> expression -> expression
val e_annotation : ?loc:Location.t -> expression -> type_expression -> expression
val e_application : ?loc:Location.t -> expression -> expression -> expression
val e_binop : ?loc:Location.t -> name -> expression -> expression -> expression
val e_constant : ?loc:Location.t -> name -> expression list -> expression
val e_look_up : ?loc:Location.t -> expression -> expression -> expression
val e_assign : ?loc:Location.t -> name -> access_path -> expression -> expression

val make_option_typed : ?loc:Location.t -> expression -> type_expression option -> expression
val ez_e_record : ?loc:Location.t -> ( string * expression ) list -> expression

val e_typed_none : ?loc:Location.t -> type_expression -> expression

val e_typed_list : ?loc:Location.t -> expression list -> type_expression -> expression

val e_typed_map : ?loc:Location.t -> ( expression * expression ) list  -> type_expression -> type_expression -> expression
val e_typed_big_map : ?loc:Location.t -> ( expression * expression ) list  -> type_expression -> type_expression -> expression

val e_typed_set : ?loc:Location.t -> expression list -> type_expression -> expression

val e_lambda : ?loc:Location.t -> string -> type_expression option -> type_expression option -> expression -> expression
val e_record : ?loc:Location.t -> expr_map -> expression

val e_ez_record : ?loc:Location.t -> ( string * expr ) list -> expression
(*
val get_e_accessor : expression' -> ( expression * access_path ) result
*)

val assert_e_accessor : expression' -> unit result

val get_access_record : access -> string result

val get_e_pair : expression' -> ( expression * expression ) result

val get_e_list : expression' -> ( expression list ) result
val get_e_tuple : expression' -> ( expression list ) result
(*
val get_e_failwith : expression -> expression result 
val is_e_failwith : expression -> bool
*)
val extract_pair : expression -> ( expression * expression ) result 

val extract_list : expression -> (expression list) result

val extract_record : expression -> (string * expression) list result

val extract_map : expression -> (expression * expression) list result
