(* Command-line options for CameLIGO *)

(* If the value of [input] is [Some src], the name of the CameLIGO
   source file, with the extension ".mligo", is [src]. If [input] is
   [Some "-"] or [None], the source file is read from standard
   input. *)

type options = {
  input     : string option;
  libs      : string list;
  verbose   : Utils.String.Set.t;
}

val read : unit -> options
