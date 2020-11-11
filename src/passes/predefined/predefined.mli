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
  include module type of Helpers.Stacking
  open Stage_common.Types
  val get_operators : constant' -> predicate option
end
