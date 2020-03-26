(* Parsing the command-line options of the LIGO preprocessor *)

(* The type [options] gathers the command-line options. *)

type language = PascaLIGO | CameLIGO | ReasonLIGO

module SSet : Set.S with type elt = string

type options = <
  input   : string;
  libs    : string list;
  lang    : language;
  offsets : bool;
  verbose : SSet.t
>

val make :
  input:string ->
  libs:string list ->
  lang:language ->
  offsets:bool ->
  verbose:SSet.t ->
  options

(* Parsing the command-line options on stdin. The first parameter is
   the name of the concrete syntax. This is needed to correctly handle
   comments. *)

val read : unit -> options
