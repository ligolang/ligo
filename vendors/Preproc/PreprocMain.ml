module Region = Simple_utils.Region

let highlight msg = Printf.eprintf "\027[31m%s\027[0m%!" msg

let options = EvalOpt.read ();;

match open_in options#input with
  exception Sys_error msg -> highlight msg
| cin ->
    let buffer = Lexing.from_channel cin in
    let open Lexing in
    let () =
      buffer.lex_curr_p <-
        {buffer.lex_curr_p with pos_fname = options#input} in
    match Preproc.lex buffer with
      pp -> print_string (Buffer.contents pp)
    | exception E_Lexer.Error err ->
        let formatted =
          E_Lexer.format ~offsets:options#offsets ~file:true err
        in highlight formatted.Region.value
    | exception Preproc.Error (out, err) ->
        let formatted =
          Preproc.format ~offsets:options#offsets ~file:true err in
        begin
          print_string (Buffer.contents out);
          highlight formatted.Region.value
        end
