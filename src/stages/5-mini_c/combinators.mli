open Trace
open Types

module Expression : sig
  type t' = expression_content
  type t = expression

  val get_content : t -> t' 
  val get_type : t -> type_expression
  (*
  val is_toplevel : t -> bool 
*)
  val make_t   : ?loc:Location.t -> type_content -> type_expression
  val make     : ?loc:Location.t -> t' -> type_expression -> t
  val make_tpl : ?loc:Location.t -> t' * type_expression -> t

  val pair : t -> t -> t'
end

val get_bool : value ->bool result
val get_int : value -> Z.t result
val get_nat : value -> Z.t result
val get_mutez : value -> Z.t result
val get_timestamp : value -> Z.t result
val get_string : value -> string result
val get_bytes : value -> bytes result
val get_unit : value -> unit result
val get_option : value -> value option result
val get_map : value -> ( value * value ) list result
val get_big_map : value -> ( value * value ) list result
val get_list : value -> value list result
val get_set : value -> value list result
val get_function_with_ty : expression -> ( anon_function * ( type_expression * type_expression) ) result
val get_function : expression -> anon_function result
val get_t_function : type_expression -> ( type_expression * type_expression ) result
val get_t_option : type_expression -> type_expression result
val get_pair : value -> ( value * value ) result
val get_t_pair : type_expression -> ( type_expression * type_expression ) result
val get_t_or : type_expression -> ( type_expression * type_expression ) result
val get_t_map : type_expression -> ( type_expression * type_expression ) result
val get_t_big_map : type_expression -> ( type_expression * type_expression ) result
val get_t_list : type_expression -> type_expression result
val get_t_set : type_expression -> type_expression result
val get_left : value -> value result
val get_right : value -> value result
val get_or : value -> ( bool * value ) result
(*
val wrong_type : string -> type_value -> unit -> error
*)
val get_t_left  : type_expression -> type_expression result
val get_t_right : type_expression -> type_expression result
val get_t_contract  : type_expression -> type_expression result
val get_t_operation : type_expression -> type_expression result
val get_operation : value -> Memory_proto_alpha.Protocol.Alpha_context.packed_internal_operation result

val t_int      : ?loc:Location.t -> unit -> type_expression 
val t_unit     : ?loc:Location.t -> unit -> type_expression 
val t_nat      : ?loc:Location.t -> unit -> type_expression 
val t_function : ?loc:Location.t -> type_expression -> type_expression -> type_expression
val t_pair     : ?loc:Location.t -> type_expression annotated -> type_expression annotated -> type_expression
val t_union    : ?loc:Location.t -> type_expression annotated -> type_expression annotated -> type_expression
(*
val quote : string -> type_value -> type_value -> Expression.t -> anon_function


val e_int : Expression.t' -> Expression.t
*)
val e_unit    : ?loc:Location.t -> unit -> Expression.t
val e_skip    : ?loc:Location.t -> unit -> Expression.t
val e_var_int : ?loc:Location.t -> expression_variable -> Expression.t
val e_let_in  : ?loc:Location.t -> expression_variable -> type_expression -> inline -> Expression.t -> Expression.t -> Expression.t

val ez_e_sequence : ?loc:Location.t -> Expression.t' -> Expression.t -> expression
(*
val ez_e_return : Expression.t -> Expression.t
*)
val d_unit : value

val environment_wrap : environment -> environment -> environment_wrap
val id_environment_wrap : environment -> environment_wrap
val e_var : ?loc:Location.t -> var_name -> type_expression -> expression
val e_application : ?loc:Location.t -> expression -> type_expression -> expression -> expression
