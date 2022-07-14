type t =  Jakarta
let current = Jakarta
let in_use = Jakarta (* Protocol we depend on *)

(* this list is used to print the list of protocols in the CLI help *)
let protocols_str : string list = [
  "jakarta"
]

let protocols_to_variant : string -> t option = fun p ->
	match p with
	| "current" -> Some current
  | "jakarta" -> Some Jakarta
	| i when not (List.exists ~f:(String.equal i) protocols_str) -> None
	| _ -> failwith "internal error: forgot to add the protocol string form to the list ?"

let variant_to_string : t -> string = fun s ->
	match s with
  | Jakarta -> "jakarta"

let compare : t -> t -> int = fun p q ->
  match p, q with
  | Jakarta, Jakarta -> 0

let equal : t -> t -> bool = fun a b -> (compare a b = 0)