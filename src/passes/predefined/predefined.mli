module Tree_abstraction : sig
  open Ast_imperative

  module type Constant = sig
    val constants      : string -> rich_constant option
    val constant_to_string      : rich_constant -> string
  end

  module Pascaligo : Constant

  module Cameligo : Constant

  module Reasonligo : Constant

end

module Stacking : sig
  (*
  include Helpers.Stacking
  *)
  open Tezos_utils.Michelson
  open Mini_c

  type predicate =
    | Constant of michelson
    | Unary of michelson
    | Binary of michelson
    | Ternary of michelson
    | Tetrary of michelson
    | Pentary of michelson
    | Hexary of michelson
  val get_operators : constant' -> predicate option
  val simple_constant : t -> predicate
  val simple_unary : t -> predicate
  val simple_binary : t -> predicate
  val simple_ternary : t -> predicate
  val simple_tetrary : t -> predicate
  val simple_pentary : t -> predicate
  val simple_hexary : t -> predicate

  val unpredicate : predicate -> michelson

(*
  val predicates : predicate Map.String.t
*)
end
