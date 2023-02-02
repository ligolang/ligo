open Lsp.Types
module Loc = Simple_utils.Location
module Region = Simple_utils.Region
module Pos = Simple_utils.Pos
module Maybe = Maybe
module LSet = Caml.Set.Make (Loc)
module Hashtbl = Caml.Hashtbl

let file_start_position = Position.create ~character:0 ~line:0
let file_end_position = Position.create ~character:0 (* FIXME *) ~line:1000000000
let whole_file_range = Range.create ~end_:file_end_position ~start:file_start_position

let pos_to_position (pos : Pos.t) : Position.t =
  let line_diff = 1 in
  let character_diff = 0 in
  Position.create
    ~line:(pos#line - line_diff)
    ~character:(pos#point_num - pos#point_bol - character_diff)


let region_to_range (region : Region.t) : Range.t =
  Range.create ~start:(pos_to_position region#start) ~end_:(pos_to_position region#stop)


let region_to_location : Region.t -> Lsp.Types.Location.t =
 fun region ->
  Lsp.Types.Location.create
    ~uri:(DocumentUri.of_path region#file)
    ~range:(region_to_range region)


let position_of_location (l : Loc.t) : Position.t option =
  match l with
  | Virtual _ -> None
  | File region -> Some (pos_to_position region#start)


let position_equal : Position.t -> Position.t -> bool =
 fun p1 p2 -> p1.line = p2.line && p1.character = p2.character


let location_to_range (l : Loc.t) : Range.t option =
  match l with
  | Virtual _ -> None
  | File region -> Some (region_to_range region)


let dummy_range =
  let dummy_start = Position.create ~character:0 ~line:0 in
  let dummy_end = Position.create ~character:1 ~line:0 in
  Range.create ~end_:dummy_end ~start:dummy_start


let position_le (position_l : Position.t) (position_r : Position.t) : Bool.t =
  position_l.line < position_r.line
  || (position_l.line = position_r.line && position_l.character <= position_r.character)


let is_position_in_range (position : Position.t) (range : Range.t) : Bool.t =
  position_le range.start position && position_le position range.end_


let uri_location_cmp : DocumentUri.t -> Loc.t -> bool =
 fun uri -> function
  | File region -> DocumentUri.equal uri (DocumentUri.of_path @@ region#file)
  | Virtual _ -> false


let get_references : Scopes.def -> LSet.t =
 fun def ->
  match def with
  | Variable vdef -> LSet.add vdef.range vdef.references
  | Type tdef -> LSet.add tdef.range tdef.references
  | Module mdef -> LSet.add mdef.range mdef.references


let get_references_of_file : DocumentUri.t -> Scopes.def -> LSet.t =
 fun uri def -> LSet.filter (uri_location_cmp uri) @@ get_references def


let name_getter : Scopes.def -> string = function
  | Variable vdef -> vdef.name
  | Type tdef -> tdef.name
  | Module mdef -> mdef.name


let get_location : Scopes.def -> Loc.t = function
  | Variable vdef -> vdef.range
  | Type tdef -> tdef.range
  | Module mdef -> mdef.range


let is_reference : Position.t -> DocumentUri.t -> Scopes.def -> bool =
 fun pos uri defintion ->
  let check_pos : Loc.t -> bool = function
    | File reg -> is_position_in_range pos @@ region_to_range reg
    | Virtual _ -> false
  in
  LSet.exists check_pos @@ get_references_of_file uri defintion


let hashtbl_find_map : ('a -> 'b -> 'c option) -> ('a, 'b) Hashtbl.t -> 'c list =
 fun f h ->
  let go k v l =
    match f k v with
    | Some x -> x :: l
    | None -> l
  in
  Hashtbl.fold go h []


let position_to_string (position : Position.t) =
  "Position { line: "
  ^ Int.to_string position.line
  ^ ", character: "
  ^ Int.to_string position.character
  ^ " }"


let range_to_string (range : Range.t) =
  let json = Range.yojson_of_t range in
  Yojson.Safe.to_string json


let defintion_to_string (def : Scopes.def) =
  Format.asprintf "%a" Scopes.PP.definitions [ def ]


let location_to_string (location : Loc.t) = Format.asprintf "%a" Loc.pp location

let error_to_string (error : Checking.Errors.typer_error) =
  let display_format = Simple_utils.Display.Human_readable in
  Format.asprintf
    "%a"
    (Checking.Errors.error_ppformat ~display_format ~no_colour:false)
    error
