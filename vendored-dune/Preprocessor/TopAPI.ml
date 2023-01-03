(* Vendor dependencies *)

module Region  = Simple_utils.Region
module Std     = Simple_utils.Std
module Lexbuf  = Simple_utils.Lexbuf
module Snippet = Simple_utils.Snippet

(* Return signature *)

module type S =
  sig
    (* Checking the CLI *)

    type cli_status =
      Ok
    | Info  of string
    | Error of string

    val check_cli : unit -> cli_status

    (* Running the preprocessor *)

    val preprocess : Lexbuf.input -> Std.t * LowAPI.result
  end

(* The functor *)

module Make (Parameters : CLI.PARAMETERS) =
  struct
    module Config  = Parameters.Config
    module Options = Parameters.Options
    module Scan    = LowAPI.Make (Config) (Options)

    (* Checking for errors and valid exits *)

    type cli_status =
      Ok
    | Info  of string
    | Error of string

    let cli_error = Std.redden

    let check_cli () : cli_status =
      match Parameters.Status.status with
        `SyntaxError  msg
      | `WrongFileExt msg
      | `FileNotFound msg -> Error (cli_error msg)
      | `Help         buf
      | `CLI          buf -> Info (Buffer.contents buf)
      | `Version      ver -> Info (ver ^ "\n")
      | `Done             -> Ok

    (* Formatting errors for display *)

    let format_error (msg : string Region.reg) : string =
      let Region.{value; region} = msg in
      let no_colour = Options.no_colour in
      match Options.input with
        None ->
          let header =
            region#to_string
              ~file:false
              ~offsets:Options.offsets
              `Point
          in sprintf "%s:\n%s" header value
      | Some file ->
          sprintf "%s%s"
            (Format.asprintf "%a" (Snippet.pp_lift ~no_colour) region)
            (Std.redden value)

    (* Logging preprocessing results *)

    let log (preprocessed : LowAPI.result) : Std.t =
      let std = Std.empty in
      let open Std in
      let () =
        match preprocessed with
          Stdlib.Ok (text, _) ->
            if Options.show_pp then
              add_line std.out @@ Buffer.contents text;
              add_nl std.out
        | Error (Some text, msg) ->
            (if Options.show_pp then
               add_line std.out @@ Buffer.contents text);
            add_line std.err @@ redden @@ format_error msg;
            add_nl std.err
        | Error (None, msg) ->
            add_line std.err @@ redden @@ format_error msg;
            add_nl std.err
      in std

    let preprocess input : Std.t * LowAPI.result =
      match Lexbuf.from_input input with
        Ok (lexbuf, close) ->
          let preprocessed = Scan.from_lexbuf lexbuf
          in log preprocessed, preprocessed
      | Error msg -> Std.empty, Error (None, msg)
  end
