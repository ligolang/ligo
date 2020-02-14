(* This module is only used for testing modules [Escan] and [Eparser]
   as units *)

module Lexer = struct
  let run () =
    match Array.length Sys.argv with
      2 -> Escan.trace Sys.argv.(1)
    | _ -> prerr_endline ("Usage: " ^ Sys.argv.(0) ^ " [file]")
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
              let tree = Eparser.pp_expression Escan.token buffer in
              let value = Preproc.(eval Env.empty tree)
              in (print_string (string_of_bool value);
                  print_newline ())
            with Lexer diag    -> print "Lexical" diag
               | Parser diag   -> print "Syntactical" diag
               | Eparser.Error -> print "" ("Parse", mk_seg buffer, 1)
          in close_in cin
    else prerr_endline ("Usage: " ^ Sys.argv.(0) ^ " [file]")
end

let _ = Parser.run()
