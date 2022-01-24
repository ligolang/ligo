(* This file represente the context which give the association of values to types *)
open Ast_typed
type t
val  empty : t

val pp : Format.formatter -> t -> unit

val add_value    : t -> expression_variable -> type_expression -> t
val add_type     : t -> type_variable       -> type_expression -> t
val add_type_var : t -> type_variable       -> unit            -> t
val add_kind     : t -> type_variable       -> unit            -> t
val add_module   : t -> module_variable     -> t               -> t

val get_value  : t -> expression_variable -> type_expression option
val get_type   : t -> type_variable       -> type_expression option
val get_module : t -> module_variable     -> t option

val get_type_vars : t -> type_variable list

val add_ez_module : t -> module_variable -> module_ -> t

val init : ?env:Environment.t -> unit -> t

val get_constructor : Ast_typed.label -> t -> (type_expression * type_expression) option
val get_constructor_parametric : label -> t -> (type_variable list * type_expression * type_expression) option

val get_record : type_expression row_element_mini_c label_map -> t -> (type_variable option * rows) option
val get_sum    : type_expression row_element_mini_c label_map -> t -> rows option
