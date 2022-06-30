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

module Internal
  (M : sig
    type t  [@@deriving yojson, hash, eq, ord]
    val pp_full : Format.formatter -> t -> unit
  end) () = struct
type t = {
  name : string;
  counter : int;
  location : Location.t [@hash.ignore];
  generated : bool;
  mutable_ : bool;
  namespace : M.t list;
  } [@@deriving yojson, hash]

let equal {name=na;counter=ca;namespace=nsa;_} {name=nb;counter=cb;namespace=nsb;_} =
  String.equal na nb
  && Int.equal ca cb
  && List.equal M.equal nsa nsb

let compare {name=na;counter=ca;namespace=nsa;_} {name=nb;counter=cb;namespace=nsb;_} =
  let c = String.compare na nb in
  if Int.equal c 0
  then (let c = Int.compare ca cb in
  if Int.equal c 0
  then List.compare M.compare nsa nsb
  else c)
  else c


let global_counter = ref 1
let reset_counter () = global_counter := 1

let fresh ?(loc=Location.dummy) ?(name="gen") () =
  let counter = incr global_counter ; !global_counter in
  {name;counter;location=loc;generated=true;mutable_=false;namespace=[]}

let fresh_like ?loc v =
  let counter = incr global_counter ; !global_counter in
  let location = Option.value ~default:v.location loc in
  {v with counter;location}

(* should be removed in favor of a lift pass before ast_imperative *)
let of_input_var ?(mutable_=false) ?(loc=Location.dummy) name =
  if String.equal name "_" then fresh ~loc ~name () else
 {name;counter=0;location=loc;generated=false;mutable_;namespace=[]}

let set_namespace v ns = {v with namespace=ns}

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

let is_mutable var = var.mutable_
let is_generated var = var.generated

let is_name var name = String.equal var.name name

(* PP *)
let print_namespace ppf ns =
  let pp ppf ns = Format.fprintf ppf "<%a>" M.pp_full ns in
  let pp_sep ppf () = Format.fprintf ppf "" in
  Format.pp_print_list ~pp_sep pp ppf ns

let pp ppf v =
  let option_mutable ppf inline =
    if inline then
      Format.fprintf ppf "(mut)"
    else
      Format.fprintf ppf ""
  in
  if v.generated
  then Format.fprintf ppf "%a%s#%d%a" print_namespace v.namespace v.name v.counter option_mutable v.mutable_
  else Format.fprintf ppf "%a%s%a" print_namespace v.namespace v.name option_mutable v.mutable_

let pp_full ppf v = Format.fprintf ppf "%s#%d" v.name v.counter

end


module ModuleVar = Internal (struct type t = unit [@@deriving yojson,hash,eq,ord] let pp_full _ppf () = () end) ()
module ValueVar  = Internal (ModuleVar) ()
module TypeVar   = Internal (ModuleVar) ()
