module Region = Simple_utils.Region

let options = EvalOpt.read ();;

match open_in options#input with
  exception Sys_error msg -> prerr_endline msg
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
          E_Lexer.Error.format ~offsets:options#offsets ~file:true err
        in prerr_endline formatted.Region.value
    | exception Preproc.Error (state, err) ->
        let formatted =
          Preproc.Error.format ~offsets:options#offsets ~file:true err in
        begin
          print_string (Buffer.contents state.Preproc.out);
          prerr_endline formatted.Region.value
        end
