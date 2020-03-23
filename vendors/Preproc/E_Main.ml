(* This module is only used for testing modules [Escan] and [E_Parser]
   as units *)

module Lexer = struct
  open E_Lexer

  let run () =
    let options = EvalOpt.read () in
    match open_in options#input with
      cin ->
        let buffer = Lexing.from_channel cin in
        let rec iter () =
          match E_Lexer.scan buffer with
            E_Parser.EOL -> close_in cin; close_out stdout
          |           t -> begin
                            output_string stdout (string_of_token t);
                            output_string stdout "\n";
                            flush stdout;
                            iter ()
                          end
          | exception E_Lexer.Error err ->
              let form = Error.format ~offsets:options#offsets
                                      err
                                      ~file:options#input
              in output_string stdout (form ^ "\n")
        in iter ()
    | exception Sys_error msg -> prerr_endline msg

end

module Parser = struct
  let run () =
    if Array.length Sys.argv = 2
    then
      match open_in Sys.argv.(1) with
        exception Sys_error msg -> prerr_endline msg
      | cin ->
          let buffer = Lexing.from_channel cin in
          let open Error in
          let () =
            try
              let tree = E_Parser.pp_expression E_Lexer.token buffer in
              let value = Preproc.(eval Env.empty tree)
              in (print_string (string_of_bool value);
                  print_newline ())
            with Lexer diag    -> print "Lexical" diag
               | Parser diag   -> print "Syntactical" diag
               | E_Parser.Error -> print "" ("Parse", mk_seg buffer, 1)
          in close_in cin
    else prerr_endline ("Usage: " ^ Sys.argv.(0) ^ " [file]")
end

let _ = Parser.run()
