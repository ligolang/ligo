(* This module defines the sorts of markup recognised by the lexer *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* A lexeme is piece of concrete syntax belonging to a token. In
   algebraic terms, a token is also a piece of abstract lexical
   syntax. Lexical units emcompass both markup and lexemes. *)

type lexeme = string

type markup =
  Tabs      of int    Region.reg  (* Tabulations *)
| Space     of int    Region.reg  (* Space *)
| Newline   of lexeme Region.reg  (* "\n" or "\c\r" escape characters *)
| LineCom   of lexeme Region.reg  (* Line comments *)
| BlockCom  of lexeme Region.reg  (* Block comments *)
| BOM       of lexeme Region.reg  (* Byte-Order Mark for UTF-8 *)

type t = markup

(* Pretty-printing of markup

   The difference between [to_lexeme] and [to_string] is that the
   former builds the corresponding concrete syntax (the lexeme),
   whilst the latter makes up a textual representation of the abstract
   syntax (the OCaml data constructors).

   The result of [to_string] is escaped to avoid capture by the
   terminal. *)

val to_lexeme : t -> lexeme
val to_string : offsets:bool -> [`Byte | `Point] -> t -> string

(* Extracting regions *)

val to_region : t -> Region.t

(* Comments *)

(* Basic comments classify all comments in two categories. *)

type basic_comment =
  Line  of lexeme Region.reg
| Block of lexeme Region.reg
