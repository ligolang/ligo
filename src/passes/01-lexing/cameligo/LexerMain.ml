(* Driver for the CameLIGO lexer *)

module Comments    = Lexer_cameligo.Comments
module File        = Lexer_cameligo.File
module Token       = Lexer_cameligo.Token
module Self_lexing = Lexer_cameligo.Self_lexing

module Preproc_CLI = Preprocessor.CLI.Make (Comments)
module Lexer_CLI   = LexerLib.CLI.Make (Preproc_CLI)

module MainGen = Shared_lexer.LexerMainGen
module Main    = MainGen.Make (Comments) (File) (Token) (Lexer_CLI) (Self_lexing)

let () = Main.check_cli ()
let () = Main.scan_all ()
