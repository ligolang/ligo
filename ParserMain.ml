(* Driver for the parser of Ligo *)

open! EvalOpt (* Reads the command-line options: Effectful! *)

let sprintf = Printf.sprintf

(* Error printing and exception tracing *)

let () = Printexc.record_backtrace true

let external_ text =
  Utils.highlight (Printf.sprintf "External error: %s" text); exit 1;;

type Error.t += ParseError

let error_to_string = function
  ParseError -> "Syntax error.\n"
| _ -> assert false

let print_error ?(offsets=true) mode Region.{region; value} =
  let  msg = error_to_string value in
  let file = match EvalOpt.input with
               None | Some "-" -> false
             |         Some _  -> true in
  let  reg = region#to_string ~file ~offsets mode in
  Utils.highlight (sprintf "Parse error %s:\n%s%!" reg msg)

(* Path to the Ligo standard library *)
(*
let lib_path =
  match EvalOpt.libs with
      [] -> ""
  | libs -> let mk_I dir path = Printf.sprintf " -I %s%s" dir path
            in List.fold_right mk_I libs ""
*)

(* Instanciating the lexer *)

module Lexer = Lexer.Make (LexToken)

let Lexer.{read; buffer; get_pos; get_last; close} =
  Lexer.open_token_stream EvalOpt.input

and cout = stdout

let log = Lexer.output_token ~offsets:EvalOpt.offsets
                             EvalOpt.mode EvalOpt.cmd cout

and close_all () = close (); close_out cout

(* Tokeniser *)

let tokeniser = read ~log

(* Main *)

let () =
  try
    let ast = Parser.program tokeniser buffer in
    if Utils.String.Set.mem "ast" EvalOpt.verbose
    then AST.print_tokens ast
  with
    Lexer.Error err ->
      close_all ();
      Lexer.print_error ~offsets EvalOpt.mode err
  | Parser.Error ->
      let region = get_last () in
      let error = Region.{region; value=ParseError} in
      let () = close_all () in
      print_error ~offsets EvalOpt.mode error
  | Sys_error msg -> Utils.highlight msg
