(* Command-line options for CameLIGO *)

(* The type [options] gathers the command-line options.

     If the field [input] is [Some src], the name of the CameLIGO
   source file, with the extension ".mligo", is [src]. If [input] is
   [Some "-"] or [None], the source file is read from standard input.

     The field [libs] is made of library paths (colon-separated).

     The field [verbose] is a set of stages of the compiler chain,
   about which more information may be displayed.
*)

type options = {
  input     : string option;
  libs      : string list;
  verbose   : Utils.String.Set.t;
}

(* Parsing the command-line options on stdin *)

val read : unit -> options
