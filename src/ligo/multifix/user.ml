open Trace

let parse_file (source: string) : Ast.entry_point result =
  let pp_input =
    let prefix = Filename.(source |> basename |> remove_extension)
    and suffix = ".pp.ligo"
    in prefix ^ suffix in

  let cpp_cmd = Printf.sprintf "cpp -traditional-cpp %s -o %s"
                               source pp_input in
  let%bind () = sys_command cpp_cmd in

  let%bind channel =
    generic_try (simple_error "error opening file") @@
    (fun () -> open_in pp_input) in
  let lexbuf = Lexing.from_channel channel in
  let module Lexer = Lex.Lexer in
  specific_try (function
      | Parser.Error -> (
          let start = Lexing.lexeme_start_p lexbuf in
          let end_ = Lexing.lexeme_end_p lexbuf in
          let str = Format.sprintf
              "Parse error at \"%s\" from (%d, %d) to (%d, %d)\n"
              (Lexing.lexeme lexbuf)
              start.pos_lnum (start.pos_cnum - start.pos_bol)
              end_.pos_lnum (end_.pos_cnum - end_.pos_bol) in
          simple_error str
        )
      | _ -> simple_error "unrecognized parse_ error"
    ) @@ (fun () ->
      let raw = Parser.entry_point Lexer.token lexbuf in
      raw
    ) >>? fun raw ->
  ok raw
