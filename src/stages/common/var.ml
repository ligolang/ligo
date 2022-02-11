module Location = Simple_utils.Location

type t = {
  name : string;
  counter : int option;
  location : Location.t;
} [@@deriving yojson]

let equal {name=na;counter=ca;_} {name=nb;counter=cb;_} =
  String.equal na nb
  && Option.equal Int.equal ca cb

let compare {name=na;counter=ca;_} {name=nb;counter=cb;_} =
  let c = String.compare na nb in
  if Int.equal c 0
  then Option.compare Int.compare ca cb
  else c


let global_counter = ref 0
let reset_counter () = global_counter := 0

let fresh ?(loc=Location.dummy) ?(name="gen") () =
  let counter = incr global_counter ; Some !global_counter in
  {name;counter;location=loc}

let fresh_like v =
  fresh ~loc:v.location ~name:v.name ()

(* should be removed in favor of a lift pass before ast_imperative *)
let of_input_var ?(loc=Location.dummy) name = {name;counter=None;location=loc}

(* This exception indicates that some code tried to throw away the
   counter of a generated variable. It is not supposed to happen. *)
exception Tried_to_unfreshen_variable

(* TODO delete this *)
let to_name_exn var =
  match var.counter with
  | None -> var.name
  | Some _ -> raise Tried_to_unfreshen_variable

(* TODO remove this *)
let internal_get_name_and_counter var = (var.name, var.counter)

let get_location var = var.location

let add_prefix str var = {var with name=str^var.name}
let is_generalizable var = String.is_prefix var.name ~prefix:"_"
let is_generated var = Option.is_some var.counter

let is_name var name = String.equal var.name name

(* PP *)
let pp ppf v =
  match v.counter with
  | None -> Format.fprintf ppf "%s" v.name
  | Some i -> Format.fprintf ppf "%s#%d" v.name i
