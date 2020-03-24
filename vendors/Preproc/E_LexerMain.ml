module Region = Simple_utils.Region

let highlight msg = Printf.eprintf "\027[31m%s\027[0m%!" msg

let options = EvalOpt.read ();;

match open_in options#input with
  exception Sys_error msg -> prerr_endline msg
| cin ->
    let buffer = Lexing.from_channel cin in
    let open Lexing in
    let () =
      buffer.lex_curr_p <-
        {buffer.lex_curr_p with pos_fname = options#input} in
    let rec iter () =
      match E_Lexer.scan buffer with
        token -> Printf.printf "%s\n" (E_Lexer.string_of_token token);
                 if token <> E_Parser.EOL then iter ()
      | exception E_Lexer.Error err ->
          let formatted =
            E_Lexer.Error.format ~offsets:options#offsets ~file:true err
          in highlight formatted.Region.value
    in iter (); close_in cin
