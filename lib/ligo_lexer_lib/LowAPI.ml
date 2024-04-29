(* Vendor dependencies *)

module Region = Simple_utils.Region
module Lexbuf = Simple_utils.Lexbuf
module Config = Preprocessor.Config

(* The functor itself *)

module type S =
  sig
    type lex_unit

    type units = lex_unit list

    type error = {
      used_units : units;
      message    : string Region.reg
    }

    type 'src lexer = 'src -> (units, error) result

    val from_lexbuf  : ?file:string -> Lexing.lexbuf lexer
    val from_channel : ?file:string ->  In_channel.t lexer
    val from_string  : ?file:string ->        string lexer
    val from_buffer  : ?file:string ->      Buffer.t lexer
    val from_file    : string lexer
  end

(* THE FUNCTOR *)

module Make (Config : Config.S) (Client : Client.S) =
  struct
    module LexCore = LexCore.Make (Config) (Client)

    type lex_unit = LexCore.lex_unit

    type units = LexCore.units

    type error = LexCore.error = {
      used_units : units;
      message    : string Region.reg
    }

    type 'src lexer = 'src -> (units, error) result

    (* Lexing the input given a lexer instance *)

    let scan_all_units = function
      Error message ->
        Error {used_units=[]; message}
    | Ok LexCore.{read_units; lexbuf; close; _} ->
        let result = read_units lexbuf
        in (close (); result)

    (* Lexing all lexical units from various sources *)

    let from_lexbuf ?(file="") lexbuf =
      LexCore.(open_stream (Lexbuf (file, lexbuf))) |> scan_all_units

    let from_channel ?(file="") channel =
      LexCore.(open_stream (Channel (file, channel))) |> scan_all_units

    let from_string ?(file="") string =
      LexCore.(open_stream (String (file, string))) |> scan_all_units

    let from_buffer ?(file="") buffer =
      LexCore.(open_stream (Buffer (file, buffer))) |> scan_all_units

    let from_file file = LexCore.(open_stream (File file)) |> scan_all_units
  end
