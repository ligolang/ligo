module Location = Simple_utils.Location

type t = {
  name : string;
  counter : int option;
  location : Location.t;
} [@@deriving yojson]

let equal {name=na;counter=ca;_} {name=nb;counter=cb;_} =
  String.equal na nb &&
  Option.equal equal ca cb

let compare {name=na;counter=ca;_} {name=nb;counter=cb;_} =
  match String.compare na nb with
    0 -> Option.compare compare ca cb
  | c -> c


let global_counter = ref 0
let reset_counter () = global_counter := 0
let generate ?(loc=Location.dummy) ?(name="") () = 
  let counter = incr global_counter ; !global_counter in
  let name = Format.asprintf "#%s%i" name counter in
  {name=name;counter=Some counter;location=loc}

let of_name ?(loc=Location.dummy) name = {name;counter=None;location=loc}
(* This exception indicates that some code tried to throw away the
   counter of a generated variable. It is not supposed to happen. *)
let to_name var = var.name


let get_location var = var.location

let add_prefix str var = {var with name=str^var.name}
let concat ?(sep="") var_lst =
  let lst = List.map ~f:(fun {name;_} -> name) var_lst in
  let name = String.concat ~sep lst in
  let name = "#" ^ name in
  {name;counter=None;location=Location.dummy}

let is_generalizable var = String.equal (String.sub (var.name) ~pos:0 ~len:1) "_"
let is_generated var =
  match var.counter with
  | None -> false
  | Some _ -> true
let is_name var name = String.equal var.name name

let internal_get_name_and_counter var = (var.name, var.counter)

(* PP *)
let pp ppf v = Format.fprintf ppf "%s" v.name