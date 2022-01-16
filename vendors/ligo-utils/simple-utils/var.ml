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


let pp ppf v =
  match v.counter with
  | None -> Format.fprintf ppf "%s" v.name
  | Some i -> Format.fprintf ppf "%s#%d" v.name i


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
