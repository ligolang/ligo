(* Embedding the LIGO lexer in a debug module *)

module Region = Simple_utils.Region

module type S =
  sig
    module Lexer : Lexer.S
    type token = Lexer.token

    val output_token :
      ?offsets:bool ->
      [`Byte | `Point] ->
      EvalOpt.command ->
      out_channel ->
      Markup.t list ->
      token ->
      unit

    type file_path = string

    val trace :
      ?offsets:bool ->
      [`Byte | `Point] ->
      ?block:EvalOpt.block_comment ->
      ?line:EvalOpt.line_comment ->
      token_to_region:(token -> Region.t) ->
      style:(token ->
             (Lexing.lexbuf -> (Markup.t list * token) option) ->
             Lexing.lexbuf ->
             unit) ->
      LexerLib.input ->
      EvalOpt.command ->
      (unit, string Region.reg) Stdlib.result
  end

module Make (Lexer: Lexer.S) : (S with module Lexer = Lexer) =
  struct
    module Lexer = Lexer
    module Token = Lexer.Token
    type token = Lexer.token

    (* Pretty-printing in a string the lexemes making up the markup
       between two tokens, concatenated with the last lexeme
       itself. *)

    let output_token ?(offsets=true) mode command
                     channel left_mark token : unit =
      let output    str = Printf.fprintf channel "%s%!" str in
      let output_nl str = output (str ^ "\n") in
      match command with
        EvalOpt.Quiet -> ()
      | EvalOpt.Tokens ->
          Token.to_string token ~offsets mode |> output_nl
      | EvalOpt.Copy ->
          let lexeme = Token.to_lexeme token
          and apply acc markup = Markup.to_lexeme markup :: acc
          in List.fold_left apply [lexeme] left_mark
             |> String.concat "" |> output
      | EvalOpt.Units ->
          let abs_token = Token.to_string token ~offsets mode
          and apply acc markup =
            Markup.to_string markup ~offsets mode :: acc
          in List.fold_left apply [abs_token] left_mark
             |> String.concat "\n" |> output_nl

    type file_path = string

    let trace ?(offsets=true) mode ?block ?line
              ~token_to_region ~style input command :
          (unit, string Region.reg) Stdlib.result =
      match LexerLib.open_token_stream
              ~scan:Lexer.scan
              ~token_to_region
              ~style
              ?line ?block input
      with
        Ok LexerLib.{read; buffer; close; _} ->
          let log = output_token ~offsets mode command stdout
          and close_all () = flush_all (); close () in
          let rec iter () =
            match read ~log buffer with
              token ->
                if   Token.is_eof token
                then Stdlib.Ok ()
                else iter ()
            | exception Lexer.Token.Error error ->
                let msg =
                  Lexer.Token.format_error
                    ~offsets mode ~file:true error
                in Stdlib.Error msg
            | exception Lexer.Error error ->
                let msg =
                  Lexer.format_error ~offsets mode ~file:true error
                in Stdlib.Error msg in
            let result = iter ()
            in close_all (); result
        | Stdlib.Error (LexerLib.File_opening msg) ->
            flush_all (); Stdlib.Error (Region.wrap_ghost msg)
  end
