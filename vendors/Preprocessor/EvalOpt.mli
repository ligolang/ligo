(* Parsing the command-line options of the LIGO preprocessor *)

(* The type [options] gathers the command-line options. *)

type language = [`PascaLIGO | `CameLIGO | `ReasonLIGO]

val lang_to_string : language -> string

module SSet : Set.S with type elt = string and type t = Set.Make(String).t

type options = <
  input   : string option;
  libs    : string list;
  verbose : SSet.t;
  offsets : bool;
  lang    : language;
  ext     : string   (* ".ligo", ".mligo", ".religo" *)
>

val make :
  input:string option ->
  libs:string list ->
  lang:language ->
  offsets:bool ->
  verbose:SSet.t ->
  ext:string ->
  options

(* Parsing the command-line options on stdin. The first parameter is
   the name of the concrete syntax. This is needed to correctly handle
   comments. *)

val read : lang:language -> ext:string -> options
