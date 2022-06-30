module Location = Simple_utils.Location

module type VAR = sig
   type t [@@deriving eq, compare, yojson, hash]
   (* Create a compiler generated variable *)
   val reset_counter : unit -> unit
   val fresh : ?loc:Location.t -> ?name:string -> unit -> t
   val fresh_like : ?loc:Location.t -> t -> t
   (* Construct a user variable directly from a string. This should only
      be used for embedding user variable names. For programmatically
      generated variables, use `fresh`. Take care not to cause
      shadowing/capture except as the user intended. *)
   val of_input_var : ?loc:Location.t -> string -> t
   (* Warning : do not use *)
   val to_name_exn : t -> string

   val get_location : t -> Location.t
   val set_location : Location.t -> t -> t

   val is_generated     : t -> bool
   (* Prints vars as %s or %s#%d *)
   val pp : Format.formatter -> t -> unit
end

module Internal () = struct
type t = {
  name : string;
  counter : int;
  generated : bool;
  location : Location.t [@equal.ignore] [@compare.ignore] [@hash.ignore];
  } [@@deriving equal, compare, yojson, hash]

let global_counter = ref 1
let reset_counter () = global_counter := 1

let fresh ?(loc=Location.dummy) ?(name="gen") () =
  let counter = incr global_counter ; !global_counter in
  {name;counter;generated=true;location=loc}

let fresh_like ?loc v =
  let counter = incr global_counter ; !global_counter in
  let location = Option.value ~default:v.location loc in
  {v with counter;location}

(* should be removed in favor of a lift pass before ast_imperative *)
let of_input_var ?(loc=Location.dummy) name =
  if String.equal name "_" then fresh ~name () else
 {name;counter=0;generated=false;location=loc}

(* This exception indicates that some code tried to throw away the
   counter of a generated variable. It is not supposed to happen. *)
exception Tried_to_unfreshen_variable

(* TODO delete this *)
let to_name_exn var =
  if var.generated
  then raise Tried_to_unfreshen_variable
  else var.name

(* TODO remove this *)
let internal_get_name_and_counter var = (var.name, var.counter)

let get_location var = var.location
let set_location location var = {var with location}
let is_generalizable var = String.is_prefix var.name ~prefix:"_"

let is_generated var = var.generated

let is_name var name = String.equal var.name name

(* PP *)
let pp ppf v =
  if v.counter <> 0
  then Format.fprintf ppf "%s#%d"  v.name v.counter
  else Format.fprintf ppf "%s" v.name

let _pp ppf v = Format.fprintf ppf "%s#%d" v.name v.counter

end


module ModuleVar = Internal ()
module ValueVar  = Internal ()
module TypeVar   = Internal ()
