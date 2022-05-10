(* This file represente the context which give the association of values to types *)
open Ast_typed

module Typing : sig
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

  val context_of_module_expr : outer_context:t -> Ast_typed.module_expr -> t

  val init : ?env:Environment.t -> unit -> t

  val get_record : type_expression row_element_mini_c label_map -> t -> (type_variable option * rows) option
  val get_sum    : label -> t -> (type_variable * type_variable list * type_expression * type_expression) list
end

module Hashes : sig
  val set_context : Typing.t -> unit
  val hash_types : unit -> unit
  val find_type : type_expression -> (module_variable list * type_variable) option
end

module App : sig
  type t
  val pop : t -> type_expression list option
  val push : type_expression option -> type_expression list -> t -> t
  val create : type_expression option -> t
  val get_expect : t -> type_expression option
  val update_expect : type_expression option -> t -> t
end

type typing_context = Typing.t
type app_context = App.t
type t = app_context * typing_context
