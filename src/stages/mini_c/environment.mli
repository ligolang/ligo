(* open Trace *)
open Types


module Environment : sig

  type element = environment_element
  type t = environment

  val empty : t
  val add : element -> t -> t
  val concat : t list -> t
  (*
  val get_opt : string -> t -> type_value option
  val has : string -> t -> bool
  *)
  val get_i : string -> t -> (type_value * int)
  val of_list : element list -> t
  val to_list : t -> element list
  val get_names : t -> string list
  val remove : int -> t -> t
  val select : ?rev:bool -> ?keep:bool -> string list -> t -> t
  (*
  val fold : ('a -> element -> 'a ) -> 'a -> t -> 'a
  val filter : ( element -> bool ) -> t -> t
  *)
  (*
  vatl closure_representation : t -> type_value
  *)
end

type element = environment_element
type t = environment

val empty : t
val add : element -> t -> t
val concat : t list -> t
(*
val get_opt : string -> t -> type_value option
*)
val has : string -> t -> bool
(*
val get_i : string -> t -> (type_value * int)
*)
val of_list : element list -> t
(*
val to_list : t -> element list
val get_names : t -> string list
val remove : int -> t -> t

*)
val select : ?rev:bool -> ?keep:bool -> string list -> t -> t
val fold : ('a -> element -> 'a ) -> 'a -> t -> 'a
val filter : ( element -> bool ) -> t -> t

(*
val closure_representation : t -> type_value
*)
