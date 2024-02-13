type t = Oxford2 [@@deriving eq, compare]

let current = Oxford2
let in_use = Oxford2 (* Protocol we depend on *)

let variant_to_string : t -> string =
 fun s ->
  match s with
  | Oxford2 -> "oxford2"


(* this list is used to print the list of protocols in the CLI help *)
let protocols_str : string list = List.map ~f:variant_to_string [ Oxford2 ]

let protocols_to_variant : string -> t option =
 fun p ->
  match p with
  | "current" -> Some current
  | s when String.equal s (variant_to_string Oxford2) -> Some Oxford2
  | i when not (List.exists ~f:(String.equal i) protocols_str) -> None
  | _ -> failwith "internal error: forgot to add the protocol string form to the list ?"
