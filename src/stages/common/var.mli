module Location = Simple_utils.Location

(* This module abstract the idea of variable. At the moment this is the same
for epressions,types and modules but they could be different in the future *)
type t

val equal    : t -> t -> bool
val compare  : t -> t -> int
val to_yojson: t -> Yojson.Safe.t
val of_yojson: Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or


(* Create a compiler generated variable *)
val reset_counter : unit -> unit
val fresh : ?loc:Location.t -> ?name:string -> unit -> t
val fresh_like : t -> t

(* Construct a user variable directly from a string. This should only
   be used for embedding user variable names. For programmatically
   generated variables, use `fresh`. Take care not to cause
   shadowing/capture except as the user intended. *)
val of_input_var : ?loc:Location.t -> string -> t

(* Warning : do not use *)
val to_name_exn : t -> string
val internal_get_name_and_counter : t -> (string * int option)

val get_location : t -> Location.t

val add_prefix : string -> t -> t

val is_generalizable : t -> bool
val is_generated     : t -> bool
val is_name          : t -> string -> bool

(* Prints vars as %s or %s#%d *)
val pp : Format.formatter -> t -> unit

