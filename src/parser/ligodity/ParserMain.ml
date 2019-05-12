(* Driver for the parser of Mini-ML *)

(* Error printing and exception tracing *)

Printexc.record_backtrace true;;

(* Path to the Mini-ML standard library *)

let lib_path =
  match EvalOpt.libs with
      [] -> ""
  | libs -> let mk_I dir path = Printf.sprintf " -I %s%s" dir path
            in List.fold_right mk_I libs ""

(* Opening the input channel and setting the lexing engine *)

let cin, reset =
  match EvalOpt.input with
    None | Some "-" -> stdin, fun ?(line=1) _buffer -> ignore line
  |       Some file -> open_in file, Lexer.reset ~file

let buffer = Lexing.from_channel cin
let     () = reset buffer

(* Tokeniser *)

let tokeniser =
  if Utils.String.Set.mem "lexer" EvalOpt.verbose then
    Lexer.get_token ~log:(stdout, Lexer.output_token buffer)
  else Lexer.get_token ?log:None

let () =
  try
    let ast = Parser.program tokeniser buffer in
    if Utils.String.Set.mem "unparsing" EvalOpt.verbose then
      AST.print_tokens ~undo:true ast
    else () (* AST.print_tokens ast *)
  with
    Lexer.Error diag ->
      close_in cin; Lexer.prerr ~kind:"Lexical" diag
  | Parser.Error ->
      let start  = Pos.from_byte (Lexing.lexeme_start_p buffer)
      and stop   = Pos.from_byte (Lexing.lexeme_end_p buffer) in
      let region = Region.make ~start ~stop in
      close_in cin;
      Lexer.prerr ~kind:"Syntactical"
                  Region.{value="Parse error."; region}
  | Sys_error msg -> Utils.highlight msg
