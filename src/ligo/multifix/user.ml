open Trace

let parse_file (source: string) : Ast.entry_point result =
  (* let pp_input =
   *   let prefix = Filename.(source |> basename |> remove_extension)
   *   and suffix = ".pp.ligo"
   *   in prefix ^ suffix in
   *
   * let cpp_cmd = Printf.sprintf "cpp -traditional-cpp %s -o %s"
   *                               source pp_input in
   * let%bind () = sys_command cpp_cmd in
   *
   * let%bind channel =
   *   generic_try (simple_error "error opening file") @@
   *   (fun () -> open_in pp_input) in *)
  let%bind channel =
    generic_try (fun () -> simple_error (thunk "error opening file") ()) @@
    (fun () -> open_in source) in
  let lexbuf = Lexing.from_channel channel in
  let module Lexer = Lex.Lexer in
  (specific_try (fun () -> fun e ->
      let error s () =
        let start = Lexing.lexeme_start_p lexbuf in
        let end_ = Lexing.lexeme_end_p lexbuf in
        let str () = Format.sprintf
          "at \"%s\" from (%d, %d) to (%d, %d)\n"
          (Lexing.lexeme lexbuf)
          start.pos_lnum (start.pos_cnum - start.pos_bol)
          end_.pos_lnum (end_.pos_cnum - end_.pos_bol) in
        error s str () in
      match e with
      | Parser.Error -> (fun () -> error (thunk "Parse") ())
      | Lexer.Error s -> (fun () -> error (fun () -> "Lexer " ^ s) ())
      | Lexer.Unexpected_character _ -> error (thunk "Unexpected char")
      | _ -> simple_error (thunk "unrecognized parse_ error")
    )) @@ (fun () ->
      let raw = Parser.entry_point Lexer.token lexbuf in
      raw
    ) >>? fun raw ->
  ok raw
