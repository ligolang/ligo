(* Interfacing the PascaLIGO lexer. *)

(* LIGO dependencies *)

module Config = Preprocessing_pascaligo.Config

(* Internal dependencies *)

include Lexing_shared.Common.Make (Config) (Lexing_pascaligo.Token)
