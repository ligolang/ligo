type t =
  | Lima
  | Mumbai
[@@deriving eq, compare]

let current = Lima
let in_use = Mumbai (* Protocol we depend on *)

(* this list is used to print the list of protocols in the CLI help *)
let protocols_str : string list = [ "lima"; "mumbai" ]

let protocols_to_variant : string -> t option =
 fun p ->
  match p with
  | "current" -> Some current
  | "lima" -> Some Lima
  | "mumbai" -> Some Mumbai
  | i when not (List.exists ~f:(String.equal i) protocols_str) -> None
  | _ -> failwith "internal error: forgot to add the protocol string form to the list ?"


let variant_to_string : t -> string =
 fun s ->
  match s with
  | Lima -> "lima"
  | Mumbai -> "mumbai"
