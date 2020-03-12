open Trace
open Types


(*

module Errors : sig
  val different_literals_because_different_types : name -> literal -> literal -> unit -> error

  val different_literals : name -> literal -> literal -> unit -> error

  val error_uncomparable_literals : name -> literal -> literal -> unit -> error
end

val assert_literal_eq : ( literal * literal ) -> unit result
*)

val assert_value_eq : ( expression * expression ) -> unit result

val is_value_eq : ( expression * expression ) -> bool
