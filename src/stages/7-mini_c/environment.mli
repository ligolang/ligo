(* open Trace *)
open Types
open Ligo_prim


module Environment : sig

  type element = environment_element
  type t = environment

  val empty : t
  val add : element -> t -> t
  val concat : t list -> t
  (*
  val get_opt : Var.t -> t -> type_value option
  val has : Var.t -> t -> bool
  *)
  val get_i_opt : Value_var.t -> t -> (type_expression * int) option
  val of_list : element list -> t
  val to_list : t -> element list
  val get_names : t -> Value_var.t list
  val remove : int -> t -> t
  (* val select : ?rev:bool -> ?keep:bool -> expression_variable list -> t -> t *)
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
val get_opt : Var.t -> t -> type_value option
*)
val has : Value_var.t -> t -> bool
(*
val get_i : Var.t -> t -> (type_value * int)
*)
val of_list : element list -> t
(*
val to_list : t -> element list
val get_names : t -> Var.t list
val remove : int -> t -> t

*)
val select : ?rev:bool -> ?keep:bool -> Value_var.t list -> t -> t
val fold : ('a -> element -> 'a ) -> 'a -> t -> 'a
val filter : ( element -> bool ) -> t -> t

(*
val closure_representation : t -> type_value
*)
