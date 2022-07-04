module Michelson : sig
  open Tezos_utils.Michelson

  type predicate
  val simple_constant : unit t -> predicate
  val simple_unary : unit t -> predicate
  val simple_binary : unit t -> predicate
  val simple_ternary : unit t -> predicate

  val trivial_special : string -> predicate
  val special : ((string -> unit michelson) -> unit michelson) -> predicate

  val unpredicate :
    'a ->
    (string -> unit michelson) ->
    predicate -> 'a michelson
end