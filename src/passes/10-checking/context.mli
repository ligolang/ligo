(* This file represente the context which give the association of values to types *)
open Ligo_prim
open Ast_typed

module Typing : sig
  type t
  val  empty : t

  val pp : Format.formatter -> t -> unit

  val add_value    : t -> Value_var.t -> type_expression -> t
  val add_type     : t -> Type_var.t       -> type_expression -> t
  val add_type_var : t -> Type_var.t       -> unit            -> t
  val add_kind     : t -> Type_var.t       -> unit            -> t
  val add_module   : t -> Module_var.t     -> t               -> t

  val get_value  : t -> Value_var.t -> type_expression option
  val get_type   : t -> Type_var.t       -> type_expression option
  val get_module : t -> Module_var.t     -> t option

  val get_type_vars : t -> Type_var.t list

  val context_of_module_expr : outer_context:t -> Ast_typed.module_expr -> t

  val init : ?env:Environment.t -> unit -> t

  val get_record : type_expression Rows.row_element_mini_c Record.t -> t -> (Type_var.t option * rows) option
  val get_sum    : Label.t -> t -> (Type_var.t * Type_var.t list * type_expression * type_expression) list
end

module Hashes : sig
  val set_context : Typing.t -> unit
  val hash_types : unit -> unit
  val find_type : type_expression -> (Module_var.t list * Type_var.t) option
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
