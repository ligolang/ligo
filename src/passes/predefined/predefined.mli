module Tree_abstraction : sig
  open Ligo_prim

  val pseudo_module_to_string : Constant.constant' -> string
end

module Michelson : sig
  include module type of Helpers.Michelson
  open Ligo_prim

  val get_operators : Constant.constant' -> predicate option
end
