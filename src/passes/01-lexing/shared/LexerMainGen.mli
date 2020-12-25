(* Vendor dependencies *)

module Region = Simple_utils.Region

(* This module factors the common actions expected from LexerMain in
   all LIGO syntaxes, like reading and checking the command-line,
   building the preprocessor, the lexer, composing them and calling
   them. Note the use of a generative functor to remind the callers
   that a side-effect is performed (reading from and writing to
   [Sys.argv]: see module [LexerLib.CLI].). *)

module Make (Comments    : Comments.S)
            (File        : File.S)
            (Token'      : Token.S)
            (CLI         : LexerLib.CLI.S)
            (Self_lexing : Self_lexing.S with type token = Token'.t):
  sig
    module Token : Token.S
    type token = Token.t

    (* Scanning one token *)

    type window = <
      last_token    : token option;
      current_token : token           (* Including EOF *)
    >

    type message = string Region.reg

    val scan : Lexing.lexbuf -> (token, message) Stdlib.result

    val get_window : (unit -> window option) ref

    val clear : unit -> unit

    (* Scanning all tokens in the input given by the CLI, after the
       preprocessor is run. *)

    val scan_all : unit -> unit

    (* Check the CLI *)

    val check_cli : unit -> unit
  end with module Token = Token'
