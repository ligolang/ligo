(* Side-effects on lexing buffers (see [Stdlib.Lexing]) *)

type file_path = string

(* Extracting information *)

let current_linenum lexbuf = Lexing.(lexbuf.lex_curr_p.pos_lnum)

let current_filename lexbuf = Lexing.(lexbuf.lex_curr_p.pos_fname)

(* Rolling back one lexeme _within the current semantic action_ *)

let rollback lexbuf =
  let open Lexing in
  let len = String.length (lexeme lexbuf) in
  let pos_cnum = lexbuf.lex_curr_p.pos_cnum - len in
  lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - len;
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_cnum}

(* Resetting file name and line number in the lexing buffer

   The call [reset ~file ~line lexbuf] modifies in-place the lexing
   buffer [lexbuf] so the lexing engine records that the file
   associated with [lexbuf] is named [file] _at the current position_,
   and the current line is [line]. *)

let reset_file file lexbuf =
  let open Lexing in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = file}

  let reset_line line lexbuf =
  assert (line >= 0);
  let open Lexing in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_lnum = line}

let reset_offset offset lexbuf =
  assert (offset >= 0);
  let open Lexing in
  let bol = lexbuf.lex_curr_p.pos_bol in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_cnum = bol + offset}

let reset ?file ?(line=1) ?offset lexbuf =
  let () =
    match file with
      Some file -> reset_file file lexbuf
    |      None -> () in
  let () = reset_line line lexbuf in
  match offset with
    Some offset -> reset_offset offset lexbuf
  |        None -> ()

(* Making a lexing buffer from various sources *)

type input =
  File    of file_path
| Buffer  of file_path * Buffer.t
| String  of file_path * string
| Channel of file_path * In_channel.t
| Lexbuf  of file_path * Lexing.lexbuf

type close = unit -> unit

(* The function [Lexbuf.reset] is useful when lexing a file that has
   been previously preprocessed, in which case the argument [file] is
   the name of the file that was preprocessed, _not_ the preprocessed
   file (of which the user is not normally aware). *)

let rec from_input = function
  Lexbuf (file, lexbuf) ->
    let () = if String.(file <> "") then reset ~file lexbuf
    in Stdlib.Ok (lexbuf, fun () -> ())
| String (file, string) ->
    from_input (Lexbuf (file, Lexing.from_string string))
| Buffer (file, buffer) ->
    from_input (String (file, Buffer.contents buffer))
| Channel (file, in_chan) ->
    let close () = Core.close_in in_chan in
    let lexbuf   = Lexing.from_channel in_chan in
    let () = if String.(file <> "") then reset ~file lexbuf
    in Ok (lexbuf, close)
| File "" ->
    from_input (Channel ("", Core.stdin))
| File file ->
    try
      from_input (Channel (file, Core.open_in file))
    with Sys_error msg ->
      let region = Region.min ~file
      in Error Region.{region; value=msg}

let file_from_input = function
  Buffer (file, _)
| String (file, _)
| Channel (file, _)
| Lexbuf (file, _)
| File file -> file
