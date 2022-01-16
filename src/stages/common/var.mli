module Location = Simple_utils.Location

(* This module abstract the idea of variable. At the moment this is the same
for epressions,types and modules but they could be different in the future *)
type t

val equal    : t -> t -> bool
val compare  : t -> t -> int
val to_yojson: t -> Yojson.Safe.t
val of_yojson: Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or

val get_location : t -> Location.t

(* Create a compiler generated variable *)
val reset_counter : unit -> unit
val generate : ?loc:Location.t -> ?name:string -> unit -> t

(* Construct a user variable directly from a string. This should only
   be used for embedding user variable names. For programmatically
   generated variables, use `fresh`. Take care not to cause
   shadowing/capture except as the user intended. *)
val of_name : ?loc:Location.t -> string -> t

(* TODO don't use this, this should not exist. *)
val to_name : t -> string

val add_prefix : string -> t -> t
val concat : ?sep:string -> t list -> t

(* Check if the*)
val is_generalizable : t -> bool
val is_generated     : t -> bool
val is_name          : t -> string -> bool
val internal_get_name_and_counter : t -> (string * int option)
val internal_transfer_to_mini_c   : unit -> int
  
(* Prints vars as %s or %s#%d *)
val pp : Format.formatter -> t -> unit

