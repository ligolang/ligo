(* Standalone parser for booleans expression of preprocessing
   directives for PascaLIGO *)

module Region = Simple_utils.Region

open Preprocessor

let highlight msg = Printf.eprintf "\027[31m%s\027[0m%!" msg

let options = EvalOpt.read ".ligo" (* No comments allowed *)

let parse in_chan =
  let buffer = Lexing.from_channel in_chan in
  let open Lexing in
  let () =
    match options#input with
      Some "-" | None -> ()
    | Some pos_fname ->
        buffer.lex_curr_p <- {buffer.lex_curr_p with pos_fname} in
    let () =
      try
        let tree  = E_Parser.expr E_Lexer.scan buffer in
        let value = Preproc.(eval Env.empty tree)
        in Printf.printf "%s\n" (string_of_bool value)
      with
        E_Lexer.Error error ->
          let formatted =
            E_Lexer.format ~offsets:options#offsets ~file:true error
          in highlight formatted.Region.value
      | E_Parser.Error ->
          let region = Preproc.mk_reg buffer
          and value  = Preproc.Parse_error in
          let error  = Region.{value; region} in
          let formatted =
            Preproc.format ~offsets:options#offsets
                           ~file:true error
          in highlight formatted.Region.value
    in close_in in_chan

let () =
  match options#input with
    Some "-" | None -> parse stdin
  | Some file_path ->
     try open_in file_path |> parse with
       Sys_error msg -> highlight msg
