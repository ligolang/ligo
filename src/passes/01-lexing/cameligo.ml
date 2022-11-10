(* Interfacing the CameLIGO lexer with the rest of the compiler. *)

(* LIGO dependencies *)

module Config = Preprocessing_cameligo.Config

(* Internal dependencies *)

include Lexing_shared.Common.Make (Config) (Lexing_cameligo.Token)
