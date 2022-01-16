module Location = Simple_utils.Location

type t = {
  name : string;
  location : Location.t;
} [@@deriving yojson]

let equal {name=na;_} {name=nb;_} =
  String.equal na nb

let compare {name=na;_} {name=nb;_} =
  String.compare na nb


(* TODO : remove counter, make sure that every generated_variable is corectly used *)
let global_counter = ref 0
let reset_counter () = global_counter := 0
let generate ?(loc=Location.dummy) ?(name="generated") () = 
  let counter = incr global_counter ; !global_counter in
  let name = Format.asprintf "#%s%i" name counter in
  {name=name;location=loc}

let of_name ?(loc=Location.dummy) name = {name;location=loc}
let to_name var = var.name

let get_location var = var.location

let add_prefix str var = {var with name=str^var.name}
let concat ?(sep="") var_lst =
  let lst = List.map ~f:(fun {name;_} -> name) var_lst in
  let name = String.concat ~sep lst in
  let name = "#" ^ name in
  {name;location=Location.dummy}

let is_generalizable var = String.equal (String.sub (var.name) ~pos:0 ~len:1) "_"
let is_generated var = String.equal (String.sub (var.name) ~pos:0 ~len:1) "#"

let is_name var name = String.equal var.name name

(* PP *)
let pp ppf v = Format.fprintf ppf "%s" v.name