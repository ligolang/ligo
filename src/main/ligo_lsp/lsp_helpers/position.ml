(** Position in file: line and column  *)
open Imports

include Lsp.Types.Position

let eq = Caml.( = )
let pp = Helpers_pretty.pp_with_yojson yojson_of_t
let testable = Alcotest.testable pp eq
let to_string = Helpers_pretty.show_with_yojson yojson_of_t

let of_pos (pos : Pos.t) : t =
  let line_diff = 1 in
  let character_diff = 0 in
  create
    ~line:(pos#line - line_diff)
    ~character:(pos#point_num - pos#point_bol - character_diff)


let from_location (l : Loc.t) : t option =
  match l with
  | Virtual _ -> None
  | File region -> Some (of_pos region#start)


let ( <= ) (p1 : t) (p2 : t) : bool =
  p1.line < p2.line || (p1.line = p2.line && p1.character <= p2.character)


let file_start = create ~character:0 ~line:0
let file_end = create ~character:(Int.pow 2 31 - 1) ~line:(Int.pow 2 31 - 1)
