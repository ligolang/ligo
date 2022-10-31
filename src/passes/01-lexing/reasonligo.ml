(* Interfacing the ReasonLIGO lexer with the rest of the compiler. *)

(* LIGO dependencies *)

module Config = Preprocessing_reasonligo.Config

(* Internal dependencies *)

include Lexing_shared.Common.Make (Config) (Lexing_reasonligo.Token)
