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


let from_location (l : Loc.t) : t option =
  match l with
  | Virtual _ -> None
  | File region -> Some (of_pos region#start)


(** A [Position.t] whose line and column are both [0].  *)
let zero_position = create ~line:0 ~character:0

let file_start = zero_position
let file_end = create ~character:(Int.pow 2 31 - 1) ~line:(Int.pow 2 31 - 1)

(** Compare two [Position.t]s lexicographically. That is, returns
    [Ordering.Less] if [pos_lhs] is to the left of [pos_rhs], [Ordering.Equal]
    if they are equal, or [Ordering.Greater] otherwise. *)
let compare_ord (pos_lhs : t) (pos_rhs : t) : Ordering.t =
  if pos_lhs.line < pos_rhs.line
     || (pos_lhs.line = pos_rhs.line && pos_lhs.character < pos_rhs.character)
  then Less
  else if pos_lhs.line = pos_rhs.line && pos_lhs.character = pos_rhs.character
  then Equal
  else Greater


(** Returns [true] if [pos_lhs] is to the left of [pos_rhs] or they are equal.
    Returns [false] iff [pos_lhs] is to the right of [pos_rhs]. *)
let is_to_the_left (pos_lhs : t) (pos_rhs : t) : bool =
  match compare_ord pos_lhs pos_rhs with
  | Less | Equal -> true
  | Greater -> false
