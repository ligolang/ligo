open Imports

type t = Scopes.def

let to_string (def : t) = Format.asprintf "%a" Scopes.PP.definitions [ def ]

let get_location : Scopes.def -> Loc.t = function
  | Variable vdef -> vdef.range
  | Type tdef -> tdef.range
  | Module mdef -> mdef.range


let references_getter : Scopes.def -> LSet.t =
 fun def ->
  match def with
  | Variable vdef -> LSet.add vdef.range vdef.references
  | Type tdef -> LSet.add tdef.range tdef.references
  | Module mdef -> LSet.add mdef.range mdef.references


let get_references_of_file : Document_uri.t -> Scopes.def -> LSet.t =
 fun uri def -> LSet.filter (Document_uri.matches_loc uri) @@ references_getter def


let is_reference : Position.t -> Document_uri.t -> Scopes.def -> bool =
 fun pos uri defintion ->
  let check_pos : Loc.t -> bool = function
    | File reg -> Range.contains_position pos @@ Range.of_region reg
    | Virtual _ -> false
  in
  LSet.exists check_pos @@ get_references_of_file uri defintion
