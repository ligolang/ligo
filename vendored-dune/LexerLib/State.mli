(* State threaded along the scanning functions of [LowAPI] *)

(* Vendor dependencies *)

module Region    = Simple_utils.Region
module Pos       = Simple_utils.Pos
module Directive = Preprocessor.Directive

(* State *)

type lexeme = string

type 'token state = <
  pos            : Pos.t;
  set_pos        : Pos.t -> 'token state;
  sync           : Lexing.lexbuf -> 'token state * lexeme Region.reg;
  newline        : Lexing.lexbuf -> 'token state;

  decoder        : Uutf.decoder;
  supply         : Bytes.t -> int -> int -> unit;

  lexical_units  : 'token Unit.lex_unit list;

  push_token     :        'token -> 'token state;
  push_directive :   Directive.t -> 'token state;
  push_markup    :      Markup.t -> 'token state;

  push_line      :      Thread.t -> 'token state;
  push_block     :      Thread.t -> 'token state;
  push_space     : Lexing.lexbuf -> 'token state;
  push_tabs      : Lexing.lexbuf -> 'token state;
  push_bom       : Lexing.lexbuf -> 'token state;
  push_newline   : string Region.reg option ->
                   Lexing.lexbuf -> 'token state
>

type 'token t = 'token state

val empty : file:string -> 'token t
