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
  let counter = incr global_counter ; Some !global_counter in
  {name=name;counter;location=loc}

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
let internal_transfer_to_mini_c () = !global_counter

(* PP *)
type names_for_print = { get_name_for_print : t -> string }
let global_mutable_names_for_print : names_for_print ref =
  ref { get_name_for_print =  (fun _ -> "") }

let rec int_to_unicode (x : Int.t) =
  let digit =
    match (x - ((x / 10) * 10)) with
      a when Int.equal a 0 -> "₀"
    | a when Int.equal a 1 -> "₁"
    | a when Int.equal a 2 -> "₂"
    | a when Int.equal a 3 -> "₃"
    | a when Int.equal a 4 -> "₄"
    | a when Int.equal a 5 -> "₅"
    | a when Int.equal a 6 -> "₆"
    | a when Int.equal a 7 -> "₇"
    | a when Int.equal a 8 -> "₈"
    | a when Int.equal a 9 -> "₉"
    | _ -> failwith (Format.asprintf "internal error: couldn't pretty-print int64: %d (is it a negative number?)" x)
  in
  if x = 0 then "" else (int_to_unicode (Int.(/) x 10)) ^ digit

let pp ppf v =
  match v.name, v.counter with
  | "", None -> Format.fprintf ppf "%s" v.name
  | "", Some i ->
    let new_name = ((!global_mutable_names_for_print).get_name_for_print v) in
    if String.equal new_name ""
    then Format.fprintf ppf "#%d" i
    else Format.fprintf ppf "'%s%s" new_name (int_to_unicode i)
  | _, None -> Format.fprintf ppf "%s" v.name
  | _, Some i -> Format.fprintf ppf "%s#%d" v.name i