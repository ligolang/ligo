
module Concrete_to_imperative : sig
 open Ast_imperative

  module Pascaligo : sig
    val constants  : string -> constant' option
    val type_constants : string -> type_constant option
    val type_operators : string -> type_operator option
  end

  module Cameligo : sig
    val constants  : string -> constant' option
    val type_constants : string -> type_constant option
    val type_operators : string -> type_operator option
  end

end

module Compiler : sig
  (*
  include Helpers.Compiler
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

(*
  val predicates : predicate Map.String.t
*)
end
