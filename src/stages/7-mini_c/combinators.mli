open Types
open Ligo_prim

module Expression : sig
  type t' = expression_content
  type t = expression

  val make_t
    :  ?loc:Location.t
    -> ?source_type:Ast_typed.type_expression
    -> type_content
    -> type_expression

  val make : ?loc:Location.t -> t' -> type_expression -> t
  val make_tpl : ?loc:Location.t -> t' * type_expression -> t
end

val get_bool : value -> bool option
val get_int : value -> Z.t option
val get_nat : value -> Z.t option
val get_mutez : value -> Z.t option
val get_timestamp : value -> Z.t option
val get_string : value -> string option
val get_bytes : value -> bytes option
val get_unit : value -> unit option
val get_map : value -> (value * value) list option
val get_big_map : value -> (value * value) list option
val get_list : value -> value list option
val get_set : value -> value list option
val get_t_function : type_expression -> (type_expression * type_expression) option
val get_t_option : type_expression -> type_expression option
val get_pair : value -> (value * value) option
val get_t_pair : type_expression -> (type_expression * type_expression) option
val get_t_tuple : type_expression -> type_expression list option
val get_t_or : type_expression -> (type_expression * type_expression) option
val get_t_map : type_expression -> (type_expression * type_expression) option
val get_t_big_map : type_expression -> (type_expression * type_expression) option
val get_t_list : type_expression -> type_expression option
val get_t_set : type_expression -> type_expression option
val get_t_collection : type_expression -> type_expression option
val get_left : value -> value option
val get_right : value -> value option
val get_t_left : type_expression -> type_expression option
val get_t_right : type_expression -> type_expression option
val get_operation : value -> bytes option
val t_unit : ?loc:Location.t -> unit -> type_expression

val t_pair
  :  ?loc:Location.t
  -> type_expression annotated
  -> type_expression annotated
  -> type_expression

val t_union
  :  ?loc:Location.t
  -> source_type:Ast_typed.type_expression option
  -> type_expression annotated
  -> type_expression annotated
  -> type_expression

val t_tuple :
  ?loc:Location.t ->
  source_type:Ast_typed.type_expression option ->
  type_expression annotated list ->
  type_expression

val e_let_in
  :  ?loc:Location.t
  -> Value_var.t
  -> type_expression
  -> inline
  -> Expression.t
  -> Expression.t
  -> Expression.t

val e_let_mut_in
  :  ?loc:Location.t
  -> Value_var.t
  -> type_expression
  -> Expression.t
  -> Expression.t
  -> Expression.t

val e_proj : ?loc:Location.t -> expression -> type_expression -> int -> int -> expression
val e_var : ?loc:Location.t -> var_name -> type_expression -> expression
val ec_pair : expression -> expression -> expression_content

val e_application
  :  ?loc:Location.t
  -> expression
  -> type_expression
  -> expression
  -> expression
