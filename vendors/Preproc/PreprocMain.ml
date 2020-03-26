module Region = Simple_utils.Region

let highlight msg = Printf.eprintf "\027[31m%s\027[0m\n%!" msg

let options = EvalOpt.read ();;

match open_in options#input with
  exception Sys_error msg -> highlight msg
| cin ->
    let buffer = Lexing.from_channel cin in
    let open Lexing in
    let () =
      buffer.lex_curr_p <-
        {buffer.lex_curr_p with pos_fname = options#input} in
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
