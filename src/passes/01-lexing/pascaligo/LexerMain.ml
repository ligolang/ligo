(* Driver for the PascaLIGO lexer *)

module Comments         = Preprocessing_pascaligo.Comments
module Modules          = Preprocessing_pascaligo.Modules
module File             = Preprocessing_pascaligo.File
module Token            = Lexing_pascaligo.Token
module Preprocessor_CLI = Preprocessor.CLI.Make (Comments) (Modules)
module Lexer_CLI        = LexerLib.CLI.Make (Preprocessor_CLI)
module Self_tokens      = Lexing_pascaligo.Self_tokens
module MainGen          = Lexing_shared.LexerMainGen
module Main = MainGen.Make (File) (Token) (Lexer_CLI) (Self_tokens)

(* TODO: this run in dune build, make it run with dune runtest instead *)
module None_warning = struct
   let add_warning = fun _ -> ()
end

module Main_test = Main(None_warning)
let () = Main_test.check_cli ()
let () = Main_test.scan_all ()
