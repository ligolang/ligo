module Tree_abstraction : sig
  open Ast_imperative

  val pseudo_module_to_string : constant' -> string
  val constants      : string -> rich_constant option
  val constant_to_string      : rich_constant -> string
end

module Michelson : sig
  include module type of Helpers.Michelson
  open Stage_common.Types
  val get_operators : Environment.Protocols.t -> constant' -> predicate option
end
