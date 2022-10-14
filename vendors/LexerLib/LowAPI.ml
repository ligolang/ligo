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
    val from_channel : ?file:string ->    in_channel lexer
    val from_string  : ?file:string ->        string lexer
    val from_buffer  : ?file:string ->      Buffer.t lexer
    val from_file    : string lexer
  end

(* THE FUNCTOR *)

module Make (Config : Config.S) (Client : Client.S) =
  struct
    module Core = Core.Make (Config) (Client)

    type lex_unit = Core.lex_unit

    type units = Core.units

    type error = Core.error = {
      used_units : units;
      message    : string Region.reg
    }

    type 'src lexer = 'src -> (units, error) result

    (* Lexing the input given a lexer instance *)

    let scan_all_units = function
      Stdlib.Error message ->
        flush_all (); Error {used_units=[]; message}
    | Ok Core.{read_units; lexbuf; close; _} ->
        let result = read_units lexbuf
        in (flush_all (); close (); result)

    (* Lexing all lexical units from various sources *)

    let from_lexbuf ?(file="") lexbuf =
      Core.(open_stream (Lexbuf (file, lexbuf))) |> scan_all_units

    let from_channel ?(file="") channel =
      Core.(open_stream (Channel (file, channel))) |> scan_all_units

    let from_string ?(file="") string =
      Core.(open_stream (String (file, string))) |> scan_all_units

    let from_buffer ?(file="") buffer =
      Core.(open_stream (Buffer (file, buffer))) |> scan_all_units

    let from_file file = Core.(open_stream (File file)) |> scan_all_units
  end
