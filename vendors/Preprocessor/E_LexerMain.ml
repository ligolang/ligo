(* Standalone lexer for Boolean expressions in preprocessing
   directives *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos

(* Internal dependencies *)

module E_Lexer  = Preprocessor.E_Lexer
module E_Parser = Preprocessor.E_Parser
module State    = Preprocessor.State
module Error    = Preprocessor.Error

(* All exits *)

let lexing_error Region.{region; value} =
  begin
    Printf.eprintf "%s\n%s\n%!" (region#to_string `Point) value;
    exit 1
  end

let sys_error msg =
  Printf.eprintf "\027[31m%s\027[0m\n%!" msg; exit 1

let print_token token =
  Printf.printf "%s\n" (E_Lexer.string_of_token token)

(* Wrapper for the lexer *)

let scan filename =
  let in_chan =
    try open_in filename with
      Sys_error msg -> sys_error msg in
  let lexbuf = Lexing.from_channel in_chan
  and scan = E_Lexer.scan (new State.t (Pos.min ~file:filename)) in
  let rec iter () =
    match scan lexbuf with
      E_Parser.EOL _ as token -> print_token token
    | token -> print_token token; iter ()
    | exception Error.Error (_buffer, msg) -> lexing_error msg
  in iter (); close_in in_chan

(* Command-line parsing *)

let usage_msg = "E_LexerMain.exe <file>"

let input_file = ref ""

let anon_fun filename = input_file := filename

let speclist = []

let () = Arg.parse speclist anon_fun usage_msg

(* Main *)

let () =
  if !input_file <> "" then scan !input_file
