(* Parsing the command-line options of the LIGO preprocessor *)

(* The type [options] gathers the command-line options. *)

type language = PascaLIGO | CameLIGO | ReasonLIGO

type options = <
  input   : string;
  libs    : string list;
  lang    : language;
  offsets : bool
>

val make :
  input:string ->
  libs:string list ->
  lang:language ->
  offsets:bool ->
  options

(* Parsing the command-line options on stdin. The first parameter is
   the name of the concrete syntax. This is needed to correctly handle
   comments. *)

val read : unit -> options
