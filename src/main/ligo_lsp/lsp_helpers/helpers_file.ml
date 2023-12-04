(** Checks whether the given file refers to the LIGO stdlib. *)
let is_stdlib (file : string) : bool =
  (* stdlib regions have an empty file name. *)
  String.(file = "")


let is_packaged = Ligo_api.Api_helpers.is_packaged
