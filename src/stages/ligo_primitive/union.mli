open Ligo_prim_intf
include S1

val make : 'typ list -> 'typ t
val summands : 'typ t -> 'typ list
val number_of_summands : 'typ t -> int
val nth_summand : 'typ t -> int -> 'typ option
val for_all : ('typ -> bool) -> 'typ t -> bool

type 'typ union := 'typ t

module Injection : sig
  include S1

  val source_index_in_target : 'typ t -> int
  val source : 'typ t -> 'typ
  val target : 'typ t -> 'typ union
  val make : source_index_in_target:int -> target:'typ union -> 'typ t option
  val injections_of_union : 'typ union -> 'typ t list
  val constructor_name : 'typ t -> string
  val pp_with_type : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module Injected : sig
  include S2

  val expr_in_source : ('expr, 'typ) t -> 'expr
  val injection : ('expr, 'typ) t -> 'typ Injection.t
  val make : expr_in_source:'expr -> injection:'typ Injection.t -> ('expr, 'typ) t
end

module Match : sig
  module Pattern : sig
    include S1

    val var : 'typ t -> Var.Value_var.t
    val injection : 'typ t -> 'typ Injection.t
    val make : var:Var.Value_var.t -> injection:'typ Injection.t -> 'typ t
    val to_binder : 'typ t -> 'typ Binder.t
  end

  module Branch : sig
    include S2

    val pattern : ('expr, 'typ) t -> 'typ Pattern.t
    val body : ('expr, 'typ) t -> 'expr
    val make : pattern:'typ Pattern.t -> body:'expr -> ('expr, 'typ) t
  end

  include S2

  val matchee : ('expr, 'typ) t -> 'expr
  val branches : ('expr, 'typ) t -> ('expr, 'typ) Branch.t list
  val make : matchee:'expr -> branches:('expr, 'typ) Branch.t list -> ('expr, 'typ) t
end

module Use : sig
  include S1

  val make : before_expansion:'expr -> after_expansion:'expr -> 'expr t
  val before_expansion : 'expr t -> 'expr
  val after_expansion : 'expr t -> 'expr
end
