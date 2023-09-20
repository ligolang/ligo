module Location = Simple_utils.Location

module type VAR = sig
  type t [@@deriving compare, yojson, hash, sexp]

  (* Create a compiler generated variable *)
  val reset_counter : unit -> unit
  val fresh : loc:Location.t -> ?name:string -> ?generated:bool -> unit -> t
  val fresh_like : ?loc:Location.t -> t -> t

  (* Construct a user variable directly from a string. This should only
      be used for embedding user variable names. For programmatically
      generated variables, use `fresh`. Take care not to cause
      shadowing/capture except as the user intended. *)
  val of_input_var : loc:Location.t -> string -> t

  (* Warning : do not use *)
  val to_name_exn : t -> string
  val get_location : t -> Location.t
  val set_location : Location.t -> t -> t
  val is_generated : t -> bool
  val add_prefix : string -> t -> t

  (* Prints vars as %s or %s#%d *)
  val pp : Format.formatter -> t -> unit

  include Comparable.S with type t := t
end

module Internal () = struct
  module T = struct
    type t =
      { name : string
      ; counter : int
      ; generated : bool
      ; location : Location.t [@equal.ignore] [@compare.ignore] [@hash.ignore]
      }
    [@@deriving equal, compare, yojson, hash]

    let sexp_of_t ({ name; counter = _; generated = _; location = _ } : t) : Sexp.t =
      Sexp.Atom name


    let t_of_sexp : Sexp.t -> t = function
      | Atom name ->
        { name; generated = false; counter = 0; location = Location.generated }
      | List _ as x -> Sexplib.Conv_error.ptag_no_args "can't be a list" x
  end

  include T

  let global_counter = ref 1
  let reset_counter () = global_counter := 1
  let add_prefix str var = { var with name = str ^ var.name }

  let fresh ~loc ?(name = "gen") ?(generated = true) () =
    let counter =
      incr global_counter;
      !global_counter
    in
    { name; counter; generated; location = loc }


  let fresh_like ?loc v =
    let counter =
      incr global_counter;
      !global_counter
    in
    let location = Option.value ~default:v.location loc in
    { v with counter; location }


  (* should be removed in favor of a lift pass before ast_imperative *)
  let of_input_var ~loc name =
    { name; counter = 0; generated = false; location = loc }


  (* This exception indicates that some code tried to throw away the
   counter of a generated variable. It is not supposed to happen. *)
  exception Tried_to_unfreshen_variable

  (* TODO delete this *)
  let to_name_exn var =
    if var.generated then raise Tried_to_unfreshen_variable else var.name


  (* TODO remove this *)
  let internal_get_name_and_counter var = var.name, var.counter
  let get_location var = var.location
  let set_location location var = { var with location }
  let is_generated var = var.generated
  let is_name var name = String.equal var.name name

  (* PP *)
  let pp ppf v =
    if v.counter <> 0
    then Format.fprintf ppf "%s#%d" v.name v.counter
    else Format.fprintf ppf "%s" v.name

  include Comparable.Make (T)
end

module Module_var = Internal ()
module Value_var = Internal ()
module Type_var = Internal ()
module Layout_var = Internal ()
