(** Checks whether the given file refers to the LIGO stdlib. *)
let is_stdlib (file : string) : bool =
  (* stdlib regions have an empty file name. *)
  String.(file = "")


(** This heuristic checks whether the given file refers to a file defined within
    a LIGO registry package. *)
let is_packaged (file : string) : bool =
  (* Here we use a heuristic: if the file is defined within any directory called
     ".ligo", we suppose that it was imported. *)
  List.mem (Filename.parts file) ".ligo" ~equal:String.( = )
