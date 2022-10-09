(* Vendors dependencies *)

module Trace   = Simple_utils.Trace
module Config  = Preprocessor.Config
module Options = LexerLib.Options
module CLI     = LexerLib.CLI

(* Signature *)

module type S =
  sig
    module Token : Token.S
    module Warnings = Warnings

    (* Some inputs *)

    type file_path = string
    type dirs = file_path list

    (* Results *)

    module Errors = Errors

    type raise = (Errors.t, Warnings.t) Trace.raise

    type tokens = Token.t list

    (* Lexing various sources *)

    type 'src lexer = raise:raise -> dirs -> 'src -> tokens

    val from_file    : file_path lexer
    val from_string  : ?file:string -> string       lexer
    val from_buffer  : ?file:string -> Buffer.t     lexer
    val from_channel : ?file:string -> In_channel.t lexer

    (* Aliases *)

    val lex_file     : file_path lexer
    val lex_string   : ?file:string -> string       lexer
    val lex_buffer   : ?file:string -> Buffer.t     lexer
    val lex_channel  : ?file:string -> In_channel.t lexer
  end

(* Making lexers *)

module Make (Config : Config.S) (Token : Token.S) =
  struct
    module Token = Token
    module Warnings = Warnings

    (* Some inputs *)

    type file_path = string
    type dirs = file_path list

    (* Results *)

    module Errors = Errors

    type raise = (Errors.t, Warnings.t) Trace.raise

    type tokens = Token.t list

    (* Partially instantiating the final lexer *)

    module Scan (Options : Options.S) =
      LexerLib.LowAPI.Make (Config) (Lexer.Make (Options) (Token))

    (* Filtering out the markup *)

    let filter_tokens units : tokens =
      let apply tokens = function
        `Token token -> token :: tokens
      | `Markup _    -> tokens
      | `Directive d -> Token.mk_directive d :: tokens
      in List.fold_left ~f:apply ~init:[] units |> List.rev

    (* LEXERS *)

    type 'src lexer = raise:raise -> dirs -> 'src -> tokens

    (* Default parameters for the lexer *)

    module Default =
      CLI.MakeDefault (Preprocessor.CLI.MakeDefault (Config))

    (* Lexing a file *)

    let from_file ~(raise:raise) dirs' file =
      let module Options =
        struct
          include Default.Options
          (* We shadow the defaults: *)
          let input = Some file
          let dirs = dirs'
        end in
      let open Scan (Options) in
      match from_file file with
        Ok units -> filter_tokens units
      | Error {message; _} -> raise.error @@ Errors.generic message

    let lex_file = from_file

    (* Lexing a string *)

    let from_string ?file ~(raise:raise) dirs' string =
      let module Options =
        struct
          include Default.Options
          (* We shadow the defaults: *)
          let input = file
          let dirs = dirs'
        end in
     let open Scan (Options) in
     match from_string ?file string with
       Ok units -> filter_tokens units
     | Error {message; _} -> raise.error @@ Errors.generic message

    let lex_string = from_string

    (* Lexing a string buffer *)

    let from_buffer ?file ~(raise:raise) dirs' buffer =
     let module Options =
        struct
          include Default.Options
          (* We shadow the defaults: *)
          let input = file
          let dirs  = dirs'
        end in
     let open Scan (Options) in
     let file = Option.value file ~default:"" in
     match from_buffer ~file buffer with
       Ok units -> filter_tokens units
     | Error {message; _} -> raise.error @@ Errors.generic message

    let lex_buffer = from_buffer

    (* Lexing an input channel *)

    let from_channel ?file ~(raise:raise) dirs' channel =
      let module Options =
        struct
          include Default.Options
          (* We shadow the defaults: *)
          let input = file
          let dirs = dirs'
        end in
     let open Scan (Options) in
     match from_channel ?file channel with
       Ok units -> filter_tokens units
     | Error {message; _} -> raise.error @@ Errors.generic message

    let lex_channel = from_channel
  end
