(** Range between two positions *)
open Imports

include Lsp.Types.Range

type t = [%import: Lsp.Types.Range.t] [@@deriving eq, ord, sexp]

let pp = Helpers_pretty.pp_with_yojson yojson_of_t
let testable = Alcotest.testable pp equal
let to_string = Helpers_pretty.show_with_yojson yojson_of_t

let of_region (region : Region.t) : t =
  create ~start:(Position.of_pos region#start) ~end_:(Position.of_pos region#stop)


let of_loc (l : Loc.t) : t option =
  match l with
  | Virtual _ -> None
  | File region -> Some (of_region region)


let dummy : t =
  let dummy_start = Position.create ~character:0 ~line:0 in
  let dummy_end = Position.create ~character:1 ~line:0 in
  create ~end_:dummy_end ~start:dummy_start


let whole_file = create ~start:Position.file_start ~end_:Position.file_end

(** Is the [small] range contained in the [big] one? *)
let inside ~(big : t) ~(small : t) : bool =
  let open Position in
  big.start <= small.start && small.end_ <= big.end_


let contains_position (position : Position.t) (range : t) : Bool.t =
  let open Position in
  range.start <= position && position <= range.end_


(** Minimal range that contains both given ranges *)
let cover (r1 : t) (r2 : t) : t =
  let position_max (p1 : Position.t) (p2 : Position.t) : Position.t =
    let open Position in
    if p1 <= p2 then p2 else p1
  in
  let position_min (p1 : Position.t) (p2 : Position.t) : Position.t =
    let open Position in
    if p1 <= p2 then p1 else p2
  in
  create ~start:(position_min r1.start r2.start) ~end_:(position_max r1.end_ r2.end_)


(** Minimal range that contains all given ranges *)
let cover_nseq (ranges : t Simple_utils.Utils.nseq) : t =
  let h, t = ranges in
  List.fold ~f:cover ~init:h t


(** Functions to quickly construct ranges. Designed to be [open]ed *)
module Construct = struct
  let range
      ((line_start, character_start) : int * int)
      ((line_end, character_end) : int * int)
      : t
    =
    create
      ~start:(Position.create ~line:line_start ~character:character_start)
      ~end_:(Position.create ~line:line_end ~character:character_end)


  let point (line : int) (character : int) : t =
    let position = Position.create ~line ~character in
    create ~start:position ~end_:position


  let interval (line : int) (character_start : int) (character_end : int) : t =
    create
      ~start:(Position.create ~line ~character:character_start)
      ~end_:(Position.create ~line ~character:character_end)
end
