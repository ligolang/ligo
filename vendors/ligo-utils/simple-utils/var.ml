(* Serializer/Derserializer *)

type t = {
  name : string ;
  counter : int option ;
}

let to_yojson f =
  match f.counter with
    Some i ->
    `Assoc [
        ("name", `String f.name) ;
        ("counter",  `Int i) ;
      ]
  | None ->
    `Assoc [
        ("name", `String f.name) ;
      ]

let of_yojson = fun t ->
  match t with
  | `Assoc [
      ("name", `String name) ;
      ("counter", `Int i)] ->
      Ok {name; counter = Some i}
  | `Assoc [
      ("name", `String name)] ->
      Ok {name; counter = None}
  | _ ->
     Utils.error_yojson_format "{name: string; counter: int option}"

type names_for_print = { get_name_for_print : t -> string }
let global_mutable_names_for_print : names_for_print ref =
  ref { get_name_for_print =  (fun _ -> "") }

let with_names_for_print : names_for_print -> (unit -> unit) -> unit = fun names_for_print thunk ->
  let old = !global_mutable_names_for_print in
  let () = global_mutable_names_for_print := names_for_print in
  let () = thunk () in
  let () = global_mutable_names_for_print := old in
  ()

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


let equal v1 v2 =
  String.equal v1.name v2.name
  && Option.equal Int.equal v1.counter v2.counter

let compare v1 v2 =
  let cname = String.compare v1.name v2.name in
  if Int.equal cname 0
  then Option.compare Int.compare v1.counter v2.counter
  else cname

let global_counter = ref 0

let reset_counter () = global_counter := 0

let of_name name =
  { name = name ;
    counter = None
  }

let fresh ?name () =
  let name = Option.value ~default:"" name in
  let counter = incr global_counter ; Some !global_counter in
  { name ; counter }

let fresh_like v =
  fresh ~name:v.name ()

let of_ast_var name counter = { name ; counter }
let init_from_ast_var gc = global_counter := gc

