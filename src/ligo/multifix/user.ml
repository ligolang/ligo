let () =
  (match Array.length Sys.argv with
  | 1 -> exit 1
  | _ -> ()) ;
  let path = Sys.argv.(1) in
  let chan = open_in path in
  let lexbuf = Lexing.from_channel chan in
  let _ast = Parser.entry_point Lex.Lexer.token lexbuf in
  Format.printf "parse ok\n" ;
  ()
