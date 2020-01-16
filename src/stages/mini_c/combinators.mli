open Trace
open Types

module Expression : sig
  type t' = expression'
  type t = expression

  val get_content : t -> t' 
  val get_type : t -> type_value 
  (*
  val is_toplevel : t -> bool 
*)
  val make : t' -> type_value -> t
  val make_tpl : t' * type_value -> t

  val pair : t -> t -> t'
end

val get_bool : value ->bool result
val get_int : value -> int result
val get_nat : value -> int result
val get_mutez : value -> int result
val get_timestamp : value -> int result
val get_string : value -> string result
val get_bytes : value -> bytes result
val get_unit : value -> unit result
val get_option : value -> value option result
val get_map : value -> ( value * value ) list result
val get_big_map : value -> ( value * value ) list result
val get_list : value -> value list result
val get_set : value -> value list result
val get_function_with_ty : expression -> ( anon_function * ( type_value * type_value) ) result
val get_function : expression -> anon_function result
val get_t_function : type_value -> ( type_value * type_value ) result
val get_t_option : type_value -> type_value result
val get_pair : value -> ( value * value ) result
val get_t_pair : type_value -> ( type_value * type_value ) result
val get_t_or : type_value -> ( type_value * type_value ) result
val get_t_map : type_value -> ( type_value * type_value ) result
val get_t_big_map : type_value -> ( type_value * type_value ) result
val get_t_list : type_value -> type_value result
val get_t_set : type_value -> type_value result
val get_left : value -> value result
val get_right : value -> value result
val get_or : value -> ( bool * value ) result
(*
val wrong_type : string -> type_value -> unit -> error
*)
val get_t_left : type_value -> type_value result
val get_t_right : type_value -> type_value result
val get_t_contract : type_value -> type_value result
val get_t_operation : type_value -> unit result
val get_operation : value -> Memory_proto_alpha.Protocol.Alpha_context.packed_internal_operation result

val t_int : type_value 
val t_unit : type_value 
val t_nat : type_value 
val t_function : type_value -> type_value -> type_value
val t_pair : type_value annotated -> type_value annotated -> type_value
val t_union : type_value annotated -> type_value annotated -> type_value
(*
val quote : string -> type_value -> type_value -> Expression.t -> anon_function


val e_int : Expression.t' -> Expression.t
*)
val e_unit : Expression.t
val e_skip : Expression.t
val e_var_int : expression_variable -> Expression.t
val e_let_in  : expression_variable -> type_value -> inline -> Expression.t -> Expression.t -> Expression.t

val ez_e_sequence : Expression.t' -> Expression.t -> expression
(*
val ez_e_return : Expression.t -> Expression.t
*)
val d_unit : value

val environment_wrap : environment -> environment -> environment_wrap
val id_environment_wrap : environment -> environment_wrap
