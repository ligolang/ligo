(* Command-line options for the Mini-ML compiler/interpreter *)

(* If the value of [input] is [Some src], the name of the Mini-ML
   source file, with the extension ".mml", is [src]. If [input] is
   [Some "-"] or [None], the source file is read from standard
   input. *)

val input : string option

(* The Mini-ML source file can be processed in two mutually exclusive
   manners: if the value [eval] is set to [true], the source is
   interpreted; if the value [compile] is not [None], the source is
   compiled to OCaml; if [eval] is [false] and [compile] is [None],
   nothing is done with the source. Note: if [compile] is [Some "-"],
   the compiled code is sent to standard output. *)

val eval      : bool
val compile   : string option

(*  TODO *)

val libs      : string list
val verbose   : Utils.String.Set.t
val raw_edits : bool
