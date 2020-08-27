(* Standalone lexer for booleans expression of preprocessing
   directives for PascaLIGO *)

module Region = Simple_utils.Region

open Preprocessor

let highlight msg = Printf.eprintf "\027[31m%s\027[0m%!" msg

let options = EvalOpt.read ".ligo" (* No comments allowed *)

let lex in_chan =
  let buffer = Lexing.from_channel in_chan in
  let open Lexing in
  let () =
    match options#input with
      Some "-" | None -> ()
    | Some pos_fname ->
        buffer.lex_curr_p <- {buffer.lex_curr_p with pos_fname} in
  let rec iter () =
    match E_Lexer.scan buffer with
      token -> Printf.printf "%s\n" (E_Lexer.string_of_token token);
              if token <> E_Parser.EOL then iter ()
    | exception E_Lexer.Error err ->
        let formatted =
          E_Lexer.format ~offsets:options#offsets ~file:true err
        in highlight formatted.Region.value
  in iter (); close_in in_chan

let () =
  match options#input with
    Some "-" | None -> lex stdin
  | Some file_path ->
     try open_in file_path |> lex with
       Sys_error msg -> highlight msg
