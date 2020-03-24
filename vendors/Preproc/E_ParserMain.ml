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
    let () =
      try
        let tree  = E_Parser.expr E_Lexer.scan buffer in
        let value = Preproc.(eval Env.empty tree)
        in Printf.printf "%s\n" (string_of_bool value)
      with
        E_Lexer.Error error ->
          let formatted =
            E_Lexer.format
              ~offsets:options#offsets ~file:true error
          in highlight formatted.Region.value
      | E_Parser.Error ->
          let region = Preproc.mk_reg buffer
          and value  = Preproc.Parse_error in
          let error  = Region.{value; region} in
          let formatted =
            Preproc.format ~offsets:options#offsets
                           ~file:true error
          in highlight formatted.Region.value
    in close_in cin
