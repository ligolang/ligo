(* Parsing the command-line options of the LIGO preprocessor *)

(* The type [options] gathers the command-line options. *)

module SSet : Set.S with type elt = string and type t = Set.Make(String).t

type line_comment = string (* Opening of a line comment *)
type block_comment = <opening : string; closing : string>

val mk_block : opening:string -> closing:string -> block_comment

type options = <
  input   : string option;
  libs    : string list;
  verbose : SSet.t;
  offsets : bool;
  block   : block_comment option;
  line    : line_comment option;
  ext     : string
>

val make :
  input:string option ->
  libs:string list ->
  ?block:block_comment ->
  ?line:line_comment ->
  offsets:bool ->
  verbose:SSet.t ->
  ext:string ->
  options

(* Parsing the command-line options on stdin. The first parameter is
   the name of the concrete syntax. This is needed to correctly handle
   comments. *)

type extension = string

val read :
  ?block:block_comment -> ?line:line_comment -> extension -> options
