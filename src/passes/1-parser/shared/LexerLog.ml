(* Embedding the LIGO lexer in a debug module *)

module Region = Simple_utils.Region

module type S =
  sig
    module Lexer : Lexer.S

    val output_token :
      ?offsets:bool -> [`Byte | `Point] ->
      EvalOpt.command -> out_channel ->
      Markup.t list -> Lexer.token -> unit

    type file_path = string

    val trace :
      ?offsets:bool -> [`Byte | `Point] ->
      file_path option -> EvalOpt.command ->
      (unit, string Region.reg) Stdlib.result
  end

module Make (Lexer: Lexer.S) : (S with module Lexer = Lexer) =
  struct
    module Lexer = Lexer
    module Token = Lexer.Token

    (** Pretty-printing in a string the lexemes making up the markup
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

    let trace ?(offsets=true) mode file_path_opt command :
          (unit, string Region.reg) Stdlib.result =
      let input =
        match file_path_opt with
          Some file_path -> Lexer.File file_path
        | None -> Lexer.Stdin in
      match Lexer.open_token_stream input with
        Ok Lexer.{read; buffer; close; _} ->
          let log = output_token ~offsets mode command stdout
          and close_all () = close (); close_out stdout in
          let rec iter () =
            match read ~log buffer with
              token ->
                if   Token.is_eof token
                then Stdlib.Ok ()
                else iter ()
            | exception Lexer.Error error ->
                let file =
                  match file_path_opt with
                    None | Some "-" -> false
                  |         Some _  -> true in
                let msg =
                  Lexer.format_error ~offsets mode ~file error
                in Stdlib.Error msg in
            let result = iter ()
            in close_all (); result
        | Stdlib.Error (Lexer.File_opening msg) ->
            close_out stdout; Stdlib.Error (Region.wrap_ghost msg)
  end
