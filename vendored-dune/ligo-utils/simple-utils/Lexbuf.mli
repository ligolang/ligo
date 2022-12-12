(* Side-effects on lexing buffers (see [Stdlib.Lexing]) *)

type file_path = string

(* Line number of the current position in the lexing buffer *)

val current_linenum  : Lexing.lexbuf -> int

(* File name of the start position in the lexing buffer. *)

val current_filename : Lexing.lexbuf -> file_path

(* Rolling back one lexeme _within the current semantic action_.
   DO NOT USE IF eof matched. *)

val rollback : Lexing.lexbuf -> unit

(* Resetting file name and/or line number and/or offset in a lexing
   buffer. *)

val reset :
  ?file:file_path -> ?line:int -> ?offset:int -> Lexing.lexbuf -> unit

val reset_file   : string -> Lexing.lexbuf -> unit
val reset_line   : int -> Lexing.lexbuf -> unit
val reset_offset : int -> Lexing.lexbuf -> unit

(* Making a lexing buffer from various sources *)

type input =
  File    of file_path
| Buffer  of file_path * Buffer.t
| String  of file_path * string
| Channel of file_path * In_channel.t
| Lexbuf  of file_path * Lexing.lexbuf

type close = unit -> unit

val from_input :
  input -> (Lexing.lexbuf * close, file_path Region.reg) result

val file_from_input : input -> file_path
