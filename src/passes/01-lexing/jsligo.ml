(* Interfacing the JsLIGO lexer with the rest of the compiler. *)

(* LIGO dependencies *)

module Config = Preprocessing_jsligo.Config

(* Internal dependencies *)

include Lexing_shared.Common.Make (Config) (Lexing_jsligo.Token)
