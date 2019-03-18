(* Driver for the parser of LIGO *)

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

(* Path for CPP inclusions (#include) *)

let lib_path =
  match EvalOpt.libs with
      [] -> ""
  | libs -> let mk_I dir path = Printf.sprintf " -I %s%s" dir path
            in List.fold_right mk_I libs ""

(* Preprocessing the input source and opening the input channels *)

let prefix =
  match EvalOpt.input with
    None | Some "-" -> "temp"
  | Some file ->  Filename.(file |> basename |> remove_extension)

let suffix = ".pp.li"

let pp_input =
  if Utils.String.Set.mem "cpp" EvalOpt.verbose
  then prefix ^ suffix
  else let pp_input, pp_out = Filename.open_temp_file prefix suffix
       in close_out pp_out; pp_input

let cpp_cmd =
  match EvalOpt.input with
    None | Some "-" ->
      Printf.sprintf "cpp -traditional-cpp%s - -o %s"
                     lib_path pp_input
  | Some file ->
      Printf.sprintf "cpp -traditional-cpp%s %s -o %s"
                     lib_path file pp_input

let () =
  if Utils.String.Set.mem "cpp" EvalOpt.verbose
  then Printf.eprintf "%s\n%!" cpp_cmd;
  if Sys.command cpp_cmd <> 0 then
    external_ (Printf.sprintf "the command \"%s\" failed." cpp_cmd)

(* Instanciating the lexer *)

module Lexer = Lexer.Make (LexToken)

let Lexer.{read; buffer; get_pos; get_last; close} =
  Lexer.open_token_stream (Some pp_input)

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

                                    (*
(* Temporary: force dune to build AST2.ml *)
let () =
  let open AST2 in
  let _ = s_ast in
  ()

    (*
(* Temporary: force dune to build AST2.ml *)
let () =
  if false then
    let _ = Typecheck2.annotate in
    ()
  else
    ()
     *)
     *)
