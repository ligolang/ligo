type t = Carthage | Dalphanet | Edo
let current = Carthage

(* this list is used to print the list of protocols in the CLI help *)
let protocols_str : string list = [
  "carthage" ;
  "dalphanet" ;
	"edo" ;
]

let protocols_to_variant : string -> t option = fun p ->
	match p with
	| "current" -> Some current
	| "carthage" -> Some Carthage
	| "dalphanet" -> Some Dalphanet
	| "edo"       -> Some Edo
	| i when not (List.exists (String.equal i) protocols_str) -> None 
	| _ -> failwith "internal error: forgot to add the protocol string form to the list ?"

let variant_to_string : t -> string = fun s ->
	match s with
	| Carthage -> "carthage"
	| Dalphanet -> "dalphanet"
	| Edo -> "edo"