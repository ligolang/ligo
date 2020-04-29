(* Standalone preprocessor for PascaLIGO *)

module Region  = Simple_utils.Region
module Preproc = Preprocessor.Preproc
module EvalOpt = Preprocessor.EvalOpt

let highlight msg = Printf.eprintf "\027[31m%s\027[0m%!" msg

let options =
  let open EvalOpt in
  let block = mk_block ~opening:"(*" ~closing:"*)"
  in read ~block ~line:"//" ".ligo"

let preproc cin =
  let buffer = Lexing.from_channel cin in
  let open Lexing in
  let () =
    match options#input with
      None -> ()
    | Some pos_fname ->
        buffer.lex_curr_p <- {buffer.lex_curr_p with pos_fname} in
  match Preproc.lex options buffer with
    Stdlib.Ok pp_buffer -> print_string (Buffer.contents pp_buffer)
  | Stdlib.Error (pp_buffer, err) ->
      let formatted =
        Preproc.format ~offsets:options#offsets ~file:true err in
      begin
        if EvalOpt.SSet.mem "preproc" options#verbose then
          Printf.printf "%s\n%!" (Buffer.contents pp_buffer);
        highlight formatted.Region.value
      end

let () =
  match options#input with
    None -> preproc stdin
  | Some file_path ->
     try open_in file_path |> preproc with
       Sys_error msg -> highlight msg
