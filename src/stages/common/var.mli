module Location = Simple_utils.Location

module type VAR = sig
   type t [@@deriving hash]
   val equal : t -> t -> bool
   val compare  : t -> t -> int
   val to_yojson: t -> Yojson.Safe.t
   val of_yojson: Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
   (* Create a compiler generated variable *)
   val reset_counter : unit -> unit
   val fresh : ?loc:Location.t -> ?name:string -> unit -> t
   val fresh_like : ?loc:Location.t -> t -> t
   (* Construct a user variable directly from a string. This should only
      be used for embedding user variable names. For programmatically
      generated variables, use `fresh`. Take care not to cause
      shadowing/capture except as the user intended. *)
   val of_input_var : ?mutable_:bool -> ?loc:Location.t -> string -> t
   (* Warning : do not use *)
   val to_name_exn : t -> string

   val get_location : t -> Location.t
   val set_location : Location.t -> t -> t

   val is_generated     : t -> bool
   (* Prints vars as %s or %s#%d *)
   val pp : Format.formatter -> t -> unit
end

module ModuleVar : sig
   include VAR
end
module ValueVar : sig
   include VAR
   val is_mutable : t -> bool
   val is_name    : t -> string -> bool
   val set_namespace : t -> ModuleVar.t list -> t

   (* Maybe bad *)
   val internal_get_name_and_counter : t -> (string * int)
end

module TypeVar : sig
   include VAR
   val is_name          : t -> string -> bool
   val set_namespace : t -> ModuleVar.t list -> t
   (* Use for warning of deprecated polymorphic variable, should desapear *)
   val is_generalizable : t -> bool
end
