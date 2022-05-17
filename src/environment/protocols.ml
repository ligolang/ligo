type t =  Ithaca | Jakarta
let current = Ithaca
let in_use = Jakarta (* Protocol we depend on *)

(* this list is used to print the list of protocols in the CLI help *)
let protocols_str : string list = [
  "jakarta" ; "ithaca"
]

let protocols_to_variant : string -> t option = fun p ->
	match p with
	| "current" -> Some current
	| "ithaca" -> Some Ithaca
  | "jakarta" -> Some Jakarta
	| i when not (List.exists ~f:(String.equal i) protocols_str) -> None
	| _ -> failwith "internal error: forgot to add the protocol string form to the list ?"

let variant_to_string : t -> string = fun s ->
	match s with
	| Ithaca -> "ithaca"
  | Jakarta -> "jakarta"

let compare : t -> t -> int = fun p q ->
  match p, q with
  | Ithaca, Ithaca -> 0
  | Jakarta, Jakarta -> 0
  | Ithaca, Jakarta -> -1
  | Jakarta, Ithaca -> 1

let equal : t -> t -> bool = fun a b -> (compare a b = 0)