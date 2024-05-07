(** Range between two positions *)
open Imports

include Lsp.Types.Range

type t = [%import: Lsp.Types.Range.t] [@@deriving eq, ord, sexp]

let pp = Helpers_pretty.pp_with_yojson yojson_of_t
let testable = Alcotest.testable pp equal
let to_string = Helpers_pretty.show_with_yojson yojson_of_t

(** Convert a LIGO region into a LSP range. *)
let of_region (region : Region.t) : t =
  create ~start:(Position.of_pos region#start) ~end_:(Position.of_pos region#stop)


(** Attempt to convert a LIGO location into a LSP range. *)
let of_loc (l : Loc.t) : t option =
  match l with
  | Virtual _ -> None
  | File region -> Some (of_region region)


(** A region containing just the first line and character (0:0 to 0:1). *)
let dummy : t =
  let dummy_start = Position.create ~character:0 ~line:0 in
  let dummy_end = Position.create ~character:1 ~line:0 in
  create ~end_:dummy_end ~start:dummy_start


(** A region containing an entire file (0:0 to (2³¹-1):(2³¹-1)). *)
let whole_file = create ~start:Position.file_start ~end_:Position.file_end

(** Is the [small] range contained in the [big] one? *)
let inside ~(big : t) ~(small : t) : bool =
  let open Position in
  big.start <= small.start && small.end_ <= big.end_


(** Returns [true] iff [position] is inside [range] (including intersection at
    any endpoint of the range. *)
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


(** Return [true] iff [r1] touches [r2], or [false] otherwise. Returns [true] even if the
    intersection point is just a zero-length range, i.e., [r1.start] touches [r2.end_] or
    [r1.end_] touches [r2.start]. *)
let intersects (r1 : t) (r2 : t) : bool =
  contains_position r1.start r2
  || contains_position r1.end_ r2
  || contains_position r2.start r1
  || contains_position r2.end_ r1


(** Functions to quickly construct ranges. This module is designed to be [open]ed. *)
module Construct = struct
  (** Construct a range from start line:start character to end line:end character. *)
  let range
      ((line_start, character_start) : int * int)
      ((line_end, character_end) : int * int)
      : t
    =
    create
      ~start:(Position.create ~line:line_start ~character:character_start)
      ~end_:(Position.create ~line:line_end ~character:character_end)


  (** Construct a zero-length range from line:character to line:character. *)
  let point (line : int) (character : int) : t =
    let position = Position.create ~line ~character in
    create ~start:position ~end_:position


  (** Construct a range from line:start character to line:end character. *)
  let interval (line : int) (character_start : int) (character_end : int) : t =
    create
      ~start:(Position.create ~line ~character:character_start)
      ~end_:(Position.create ~line ~character:character_end)


  (** Define an empty range at a given position *)
  let empty ?(position : Position.t = Position.zero_position) () : t =
    create ~start:position ~end_:position
end
