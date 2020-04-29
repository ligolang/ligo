(* Functor to build a LIGO lexer *)

module Region = Simple_utils.Region
module Preproc = Preprocessor.Preproc
module SSet = Set.Make (String)

module type IO =
  sig
    val options : EvalOpt.options (* CLI options *)
  end

module Make (IO: IO) (Lexer: Lexer.S) =
  struct
    (* Error printing and exception tracing *)

    let () = Printexc.record_backtrace true

    (* Preprocessing and lexing the input source *)

    let scan () : (Lexer.token list, string Region.reg) Stdlib.result =
      (* Preprocessing the input source *)

      let preproc cin =
        let buffer = Lexing.from_channel cin in
        let open Lexing in
        let () =
          match IO.options#input with
            None -> ()
          | Some pos_fname ->
              buffer.lex_curr_p <- {buffer.lex_curr_p with pos_fname} in
        let opt = (IO.options :> Preprocessor.EvalOpt.options) in
        match Preproc.lex opt buffer with
          Stdlib.Error (pp_buffer, err) ->
            if SSet.mem "preproc" IO.options#verbose then
              Printf.printf "%s\n%!" (Buffer.contents pp_buffer);
            let formatted =
              Preproc.format ~offsets:IO.options#offsets ~file:true err
            in Stdlib.Error formatted
        | Stdlib.Ok pp_buffer ->
           (* Running the lexer on the preprocessed input *)

            let source = LexerLib.String (Buffer.contents pp_buffer) in
            match LexerLib.open_token_stream
                    ?line:IO.options#line
                    ?block:IO.options#block
                    ~scan:Lexer.scan
                    ~token_to_region:Lexer.Token.to_region
                    ~style:Lexer.Token.check_right_context
                    source with
              Ok LexerLib.{read; buffer; close; _} ->
                let close_all () = flush_all (); close () in
                let rec read_tokens tokens =
                  match read ~log:(fun _ _ -> ()) buffer with
                    token ->
                      if   Lexer.Token.is_eof token
                      then Stdlib.Ok (List.rev tokens)
                      else read_tokens (token::tokens)
                  | exception Lexer.Token.Error error ->
                      let file =
                        match IO.options#input with
                          None | Some "-" -> false
                        |         Some _  -> true in
                      let msg =
                        Lexer.Token.format_error
                          ~offsets:IO.options#offsets
                          IO.options#mode ~file error
                      in Stdlib.Error msg
                  | exception Lexer.Error error ->
                      let file =
                        match IO.options#input with
                          None | Some "-" -> false
                        |         Some _  -> true in
                      let msg =
                        Lexer.format_error ~offsets:IO.options#offsets
                                           IO.options#mode ~file error
                      in Stdlib.Error msg in
                let result = read_tokens []
                in close_all (); result
            | Stdlib.Error (LexerLib.File_opening msg) ->
                flush_all (); Stdlib.Error (Region.wrap_ghost msg) in
      match IO.options#input with
        None -> preproc stdin
      | Some file_path ->
          try open_in file_path |> preproc with
            Sys_error msg -> Stdlib.Error (Region.wrap_ghost msg)

    (* Tracing the lexing *)

    module Log = LexerLog.Make (Lexer)

    let trace () : (unit, string Region.reg) Stdlib.result =
      (* Preprocessing the input *)
      let preproc cin =
        let buffer = Lexing.from_channel cin in
        let open Lexing in
        let () =
          match IO.options#input with
            None | Some "-" -> ()
          | Some pos_fname ->
             buffer.lex_curr_p <- {buffer.lex_curr_p with pos_fname} in
        let opt = (IO.options :> Preprocessor.EvalOpt.options) in
        match Preproc.lex opt buffer with
          Stdlib.Error (pp_buffer, err) ->
            if SSet.mem "preproc" IO.options#verbose then
              Printf.printf "%s\n%!" (Buffer.contents pp_buffer);
            let formatted =
              Preproc.format ~offsets:IO.options#offsets ~file:true err
            in Stdlib.Error formatted
        | Stdlib.Ok pp_buffer ->
            let preproc_str = Buffer.contents pp_buffer in
            if SSet.mem "preproc" IO.options#verbose then
              begin
                Printf.printf "%s\n%!" (Buffer.contents pp_buffer);
                Stdlib.Ok ()
              end
            else Log.trace ~offsets:IO.options#offsets
                           IO.options#mode
                           ?block:IO.options#block
                           ?line:IO.options#line
                           ~token_to_region:Lexer.Token.to_region
                           ~style:Lexer.Token.check_right_context
                           (LexerLib.String preproc_str)
                           IO.options#cmd
      in match IO.options#input with
           None -> preproc stdin
         | Some file_path ->
             try open_in file_path |> preproc with
               Sys_error msg -> Stdlib.Error (Region.wrap_ghost msg)
  end
