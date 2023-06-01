(** Position in file: line and column  *)
open Imports

include Lsp.Types.Position

type t = [%import: Lsp.Types.Position.t] [@@deriving eq, sexp]
(* Derive for compare gives wrong result! *)

let ( <= ) (p1 : t) (p2 : t) : bool =
  p1.line < p2.line || (p1.line = p2.line && p1.character <= p2.character)


let compare (p1 : t) (p2 : t) : int =
  if not (p1 <= p2) then 1 else if equal p1 p2 then 0 else -1


let pp = Helpers_pretty.pp_with_yojson yojson_of_t
let testable = Alcotest.testable pp equal
let to_string = Helpers_pretty.show_with_yojson yojson_of_t

let of_pos (pos : Pos.t) : t =
  let line_diff = 1 in
  let character_diff = 0 in
  create
    ~line:(pos#line - line_diff)
    ~character:(pos#point_num - pos#point_bol - character_diff)


let file_start = create ~character:0 ~line:0
let file_end = create ~character:(Int.pow 2 31 - 1) ~line:(Int.pow 2 31 - 1)
