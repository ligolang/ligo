(* Driver for the ReasonLIGO lexer *)

module Comments = Lexer_reasonligo.Comments
module File     = Lexer_reasonligo.File
module Token    = Lexer_reasonligo.Token

module Preproc_CLI = Preprocessor.CLI.Make (Comments)
module Lexer_CLI   = LexerLib.CLI.Make (Preproc_CLI)

module MainGen = Shared.LexerMainGen
module Main    = MainGen.Make (Comments) (File) (Token) (Lexer_CLI)

let () = Main.check_cli ()
let () = Main.scan_all ()
