(* Driver for the PascaLIGO lexer *)

module Comments = Lexer_pascaligo.Comments
module File     = Lexer_pascaligo.File
module Token    = Lexer_pascaligo.Token

module Preproc_CLI = Preprocessor.CLI.Make (Comments)
module Lexer_CLI   = LexerLib.CLI.Make (Preproc_CLI)

module MainGen = Shared.LexerMainGen
module Main    = MainGen.Make (Comments) (File) (Token) (Lexer_CLI)

let () = Main.check_cli ()
let () = Main.scan_all ()
