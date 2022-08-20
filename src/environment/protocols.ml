type t =  Jakarta | Kathmandu
let current = Jakarta
let in_use = Kathmandu (* Protocol we depend on *)

(* this list is used to print the list of protocols in the CLI help *)
let protocols_str : string list = [ "jakarta" ; "kathmandu" ]

let protocols_to_variant : string -> t option = fun p ->
	match p with
	| "current" -> Some current
  | "jakarta" -> Some Jakarta
  | "kathmandu" -> Some Kathmandu
	| i when not (List.exists ~f:(String.equal i) protocols_str) -> None
	| _ -> failwith "internal error: forgot to add the protocol string form to the list ?"

let variant_to_string : t -> string = fun s ->
	match s with
  | Jakarta -> "jakarta"
  | Kathmandu -> "kathmandu"

let compare : t -> t -> int = fun p q ->
  match p, q with
  | Jakarta, Jakarta -> 0
  | Kathmandu, Kathmandu -> 0
  | Kathmandu, Jakarta -> 1
  | Jakarta, Kathmandu -> -1

let equal : t -> t -> bool = fun a b -> (compare a b = 0)