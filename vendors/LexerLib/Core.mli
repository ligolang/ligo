(* A library for writing UTF8-aware lexers *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos
module FQueue = Simple_utils.FQueue

(* Utility types *)

type file_path = string
type lexeme = string

(* The function [rollback] resets the lexing buffer to the state it
   was when it matched the last regular expression. This function is
   safe to use only in the semantic action of the rule which last
   matched. *)

val rollback : Lexing.lexbuf -> unit

(* Resetting file name and/or line number and/or offset in a lexing
   buffer. This function is useful when lexing a file that has been
   previously preprocessed, in which case the argument [file] is the
   name of the file that was preprocessed, _not_ the preprocessed file
   (of which the user is not normally aware). *)

val reset :
  ?file:file_path ->
  ?line:int ->
  ?offset:int ->
  Lexing.lexbuf ->
  unit

(* THREAD FOR STRUCTURED CONSTRUCTS (STRINGS, COMMENTS) *)

(* When scanning structured constructs, like strings and comments, we
   need to keep the region of the opening symbol (like double quote,
   "//" or "(*") in order to report any error more precisely. Since
   ocamllex is byte-oriented, we need to store the parsed bytes as
   characters in an accumulator [acc] and also its length [len], so,
   we are done, it is easy to build the string making up the
   structured construct with [mk_str] (see above).

   The resulting data structure is called a _thread_. (Note for Emacs:
   "*)".)  *)

type thread = <
  opening     : Region.t;
  length      : int;
  acc         : char list;
  to_string   : string;
  push_char   : char -> thread;
  push_string : string -> thread;
  set_opening : Region.t -> thread
>

val mk_thread : Region.t -> thread

(* STATE *)

(* Beyond producing tokens, the result of lexing is a _state_. The
   type [state] represents the abstract logical state of the lexing
   engine, that is, a value which is threaded during scanning and
   which denotes useful, high-level information beyond what the type
   [Lexing.lexbuf] in the standard library already provides for all
   generic lexers. We qualify it as "logical state" because the lexing
   buffer itself has a "physical state" defined by the type
   [Lexing.lexbuf].

     Tokens are the smallest units used by the parser to build the
   abstract syntax tree. The state includes a queue of recognised
   tokens, with the markup at the left of its lexeme until either the
   start of the file or the end of the previously recognised token.

     The markup from the last recognised token or, if the first token
   has not been recognised yet, from the beginning of the file is
   stored in the field [markup] of the state because it is a
   side-effect, with respect to the output token list, and we use a
   record with a single field [units] because that record may be
   easily extended during the future maintenance of this lexer.

     The state also includes a field [pos] which holds the current
   position in the source file. The position is not always updated
   after a single character has been matched: that depends on the
   regular expression that matched the lexing buffer.

     The field [window] is a two-token window, that is, a buffer that
   contains the last recognised token, and the penultimate (if any).

     The fields [decoder] and [supply] offer the support needed for
   the lexing of UTF-8 encoded characters in comments (the only place
   where they are allowed in our input languages). The former is the
   decoder proper and the latter is the effectful function [supply]
   that takes a byte, a start index and a length and feed it to
   [decoder]. See the documentation of the third-party library Uutf.

     Some methods are now documented.

     The call [state#enqueue token] updates functionally the state
   [state] by associating the token [token] with the stored markup and
   enqueuing the pair into the units queue. The field [markup] is then
   reset to the empty list.

     The call [state#slide_token token] pushes the token [token] in
   the buffer [buffer]. If the buffer is full, that is, it is [Two
   (t1,t2)], then the token [t2] is discarded to make room for
   [token].

     The call [state#sync buffer] updates the current position in
   accordance with the contents of the lexing buffer, more precisely,
   depending on the length of the string which has just been
   recognised by the scanner: that length is used as a positive offset
   to the current column. *)

(* The signature of the functor application *)

type 'token window = <
  last_token    : 'token option;
  current_token : 'token           (* Including EOF *)
>

type line_comment  = string (* Opening of a line comment *)
type block_comment = <opening : string; closing : string>

type command = [`Copy | `Units | `Tokens] option

type 'token config = <
  block     : block_comment option;
  line      : line_comment option;
  input     : file_path option;
  offsets   : bool;
  mode      : [`Byte | `Point];
  command   : command;
  is_eof    : 'token -> bool;
  to_region : 'token -> Region.t;
  to_lexeme : 'token -> string;
  to_string : offsets:bool -> [`Byte | `Point] -> 'token -> string
>

type 'token state = <
  config       : 'token config;

  units        : (Markup.t list * 'token) FQueue.t;
  markup       : Markup.t list;
  comments     : Markup.comment FQueue.t;
  window       : 'token window option;
  last         : Region.t;
  pos          : Pos.t;
  decoder      : Uutf.decoder;
  supply       : Bytes.t -> int -> int -> unit;

  enqueue      : 'token -> 'token state;
  set_units    : (Markup.t list * 'token) FQueue.t -> 'token state;
  set_last     : Region.t -> 'token state;
  set_pos      : Pos.t -> 'token state;
  slide_token  : 'token -> 'token state;

  sync         : Lexing.lexbuf -> 'token sync;

  push_newline : Lexing.lexbuf -> 'token state;
  push_line    : thread -> 'token state;
  push_block   : thread -> 'token state;
  push_space   : Lexing.lexbuf -> 'token state;
  push_tabs    : Lexing.lexbuf -> 'token state;
  push_bom     : Lexing.lexbuf -> 'token state;
  push_markup  : Markup.t -> 'token state;
  push_comment : Markup.comment -> 'token state
>

and 'token sync = {
  region : Region.t;
  lexeme : lexeme;
  state  : 'token state
}

type message = string Region.reg

(* LEXING COMMENTS AND STRINGS *)

(* Updating the state after scanning a line preprocessing directive.

   Note: The flags that may appear and which may be scanned, are
   dropped. *)

val line_preproc :
  line:string ->
  file:string ->
  'token state ->
  Lexing.lexbuf ->
  'token state

type 'token scanner =
  'token state -> Lexing.lexbuf -> ('token state, message) Stdlib.result

type 'token cut = thread * 'token state -> 'token state

type 'token client = <
  mk_string : 'token cut;
  mk_eof    : 'token scanner;
  callback  : 'token scanner
>

val mk_scan : 'token client -> 'token scanner

(* LEXER INSTANCE *)

(* The function [open_token_stream] returns a lexer instance made of
     * the input [input] of type [input];
     * a function [read] that extracts a token from a lexing
       buffer;
     * a lexing buffer [buffer] to read tokens from;
     * a function [close] that closes that buffer;
     * a function [get_win] that returns a window of zero, one or
       two tokens;
     * a function [get_pos] that returns the current position;
     * a function [get_last] that returns the region of the last
       recognised token.
     * a function [get_file] that returns the name of the file
       being scanned (empty string if [stdin]);
     * a function [get_comments] that returns all the comments
       recognised so far.

     Note that a module [Token] is exported too, because the
   signature of the exported functions depend on it.

     The type [window] is a two-token window, that is, a buffer
   that contains the last recognised token, and the penultimate
   (if any). *)

type input =
  File    of file_path
| String  of string
| Channel of in_channel
| Buffer  of Lexing.lexbuf

type 'token instance = {
  input        : input;
  read         : Lexing.lexbuf -> ('token, message) Stdlib.result;
  buffer       : Lexing.lexbuf;
  close        : unit -> unit;
  get_win      : unit -> 'token window option;
  get_pos      : unit -> Pos.t;
  get_last     : unit -> Region.t;
  get_file     : unit -> file_path;
  get_comments : unit -> Markup.comment FQueue.t
}

val lexbuf_from_input :
  'token config ->
  input ->
  (Lexing.lexbuf * (unit -> unit), message) Stdlib.result

type 'token style_checker =
  'token config ->
  'token ->
  (Lexing.lexbuf -> (Markup.t list * 'token) option) ->
  Lexing.lexbuf ->
  (unit, message) Stdlib.result

val open_token_stream :
  'token config ->
  scan:('token scanner) ->
  style:('token style_checker) ->
  input ->
  ('token instance, message) Stdlib.result
