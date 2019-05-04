(* open Trace *)
open Types

module type ENVIRONMENT = sig
  type element = environment_element
  type t = environment

  val empty : t
  val add : element -> t -> t
end

module Environment : ENVIRONMENT = struct
  type element = environment_element
  type t = environment

  let empty = []
  let add = List.cons
end

include Environment
