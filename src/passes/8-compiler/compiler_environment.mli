open Proto_alpha_utils
open Trace
open Mini_c
open Michelson

(*
module Stack = Meta_michelson.Stack
*)
val empty: environment
val get : environment -> string -> michelson result
val set : environment -> string -> michelson result

val pack_closure : environment -> selector -> michelson result
val unpack_closure : environment -> michelson result

(*
val add : environment -> (string * type_value) -> michelson result
val select : ?rev:bool -> ?keep:bool -> environment -> string list -> michelson result

val select_env : environment -> environment -> michelson result

val clear : environment -> (michelson * environment) result

val pack : environment -> michelson result

val unpack : environment -> michelson result

val pack_select : environment -> string list -> michelson result

val add_packed_anon : environment -> type_value -> michelson result

val pop : environment -> environment result
*)
