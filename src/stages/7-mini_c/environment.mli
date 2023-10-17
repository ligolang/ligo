(* open Trace *)
open Types
open Ligo_prim

module Environment : sig
  type element = environment_element
  type t = environment

  val get_i_opt : Value_var.t -> t -> (type_expression * int) option
  val get_var_opt : Value_var.t -> t -> Value_var.t option
end

type element = environment_element
type t = environment

val add : element -> t -> t
