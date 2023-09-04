(* Vendor dependencies *)

module Region  = Simple_utils.Region
module Std     = Simple_utils.Std
module Lexbuf  = Simple_utils.Lexbuf
module Snippet = Simple_utils.Snippet
module Unit    = LexerLib.Unit

module type PARAMETERS = LexerLib.CLI.PARAMETERS

(* Registering warnings *)

module type WARNING =
  sig
    val add : Main_warnings.all -> unit
  end

(* Utilities *)

let sprintf = Printf.sprintf

(* The return signature *)

module type S =
  sig
    module Token : Token.S

    (* Checking the CLI

       Note that the function [check_cli] is pure and therefore leaves
       to its callers to perform the side-effect of printing any error
       and information, nor not. *)

    type cli_status =
      Ok
    | Info  of string
    | Error of string

    val check_cli : unit -> cli_status

    (* Errors *)

    (* The type ['item error] denotes erroneous values. The field
       [preprocessed] holds the result of preprocessing, if any (it
       could have failed). The field [use_items] contains the list of
       items, which can either be lexical units or tokens, up to the
       error. The field [message] is the error message. *)

    type message = string Region.reg

    type 'item error = {
      preprocessed : Buffer.t option;
      used_items   : 'item list;
      message      : message
    }

    (* Lexer for tokens with Menhir in mind. *)

    (* Menhir expects tokens one by one, but [scan_token] internally
       scans all tokens first (see [scan_all_tokens] below) before
       feeding them to the parser. *)

    type token = Token.t

    val scan_token : no_colour:bool -> Lexing.lexbuf -> (token, token error) result

    val used_tokens : unit -> token list

    val clear : unit -> unit

    (* Scanning all lexical units *)

    (* The first component returned by a lexer of type ['a lexer] is a
       value of type [Std.t], which is a record containing two string
       fields: one meant to be sent to [stdout] and the other to
       [stderr]. It is up to the client of this module to perform the
       side-effect of printing them or not. *)

    type 'a lexer = Lexbuf.input -> Std.t * ('a list, 'a error) result

    (* Scanning all lexical units. If specified by [Options], the
       preprocessor may be run before, and/or the pipeline of
       self-passes [UnitPasses] after. *)

    val scan_all_units : no_colour:bool -> token Unit.t lexer

    (* Scanning all tokens. *)

    (* Calling [scan_all_tokens] is equivalent to calling first
       [scan_all_units], then the self-pass on the lexical units,
       followed by filtering the tokens (see [filter_tokens] below),
       and then applying the self-pass on the tokens. The value of
       type [Std.t] produced by [scan_all_units] and [scan_all_tokens]
       has been used by the self-passes to accumulate their I/O. *)

    val scan_all_tokens : no_colour:bool -> token lexer
  end

(* The functor *)

module Make
         (Preprocessor : Preprocessor.TopAPI.S)
         (Parameters   : PARAMETERS)
         (Token        : Token.S)
         (UnitPasses   : Pipeline.PASSES with type item = Token.t Unit.t)
         (TokenPasses  : Pipeline.PASSES with type item = Token.t)
         (Warning      : WARNING) =
  struct
    (* Re-exporting *)

    module Token = Token

    let add_warning = Warning.add

    type token = Token.t

    (* CLI *)

    module Options = Parameters.Options

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
      | `Conflict (o1,o2) ->
           let msg = sprintf "Choose either %s or %s." o1 o2
           in Error (cli_error msg)

    (* PIPELINES OF SELF-PASSES ON LEXICAL UNITS AND TOKENS *)

    (* The value [Options.postprocess] (corresponding to
       the CLI option "--post") specifies the number of passes to be
       composed, the first pass of [UnitPasses] being numbered 1, up
       to the last pass of [TokenPasses], since the latter are
       composed after the former. If the number of passes requested
       ("--post") is greater than the available passes, all of them
       are executed. *)

    module UnitPipeline  = Pipeline.Make (UnitPasses)
    module TokenPipeline = Pipeline.Make (TokenPasses)

    let num_of_unit_passes  = List.length UnitPasses.filters
    let num_of_token_passes = List.length TokenPasses.filters
    let num_of_passes       = num_of_unit_passes + num_of_token_passes

    let last =
      match Options.postprocess with
        Some last when last <= num_of_passes -> last
      | _ -> num_of_passes

    (* INSTANTIATING THE LIBRARY LEXER (LexerLib) *)

    (* Instantiating the client lexer *)

    module Client = Lexer.Make (Options) (Token)

    (* Instantiating the library lexer with the client lexer,
       resulting in the final, bespoke lexer *)

    module Scan = LexerLib.LowAPI.Make (Parameters.Config) (Client)

    (* LEXING ERRORS *)

    type message = string Region.reg

    type 'item error = {
      preprocessed : Buffer.t option;
      used_items   : 'item list;
      message      : message
    }

    let format_error no_colour file msg : string =
      let Region.{value; region} = msg in
      if String.(file = "") then
        let header =
          region#to_string
            ~file:false
            ~offsets:Options.offsets
            `Point
        in sprintf "%s:\n%s" header value
      else
         sprintf "%s%s"
           (Format.asprintf "%a" (Snippet.pp_lift ~no_colour) region)
           (Std.redden value)

    (* PRINTING *)

    (* Committing lexical units after running the pipeline to standard
       output/error if the CLI option "--units" was given. *)

    let commit_units units std : unit =
      match Options.command with
        Some `Units ->
          let offsets = Options.offsets in
          let f unit =
            Unit.to_string
              ~token_to_string:Token.to_string ~offsets `Point unit
            |> Std.(add_line std.out)
          in List.iter ~f units; Std.(add_nl std.out)
      | _ -> ()

    (* Committing tokens after running the two pipelines to standard
       output/error if the CLI option "--tokens" was given. *)

    let commit_tokens tokens std : unit =
      match Options.command with
        Some `Tokens ->
          let f token =
            Token.to_string ~offsets:Options.offsets `Point token
            |> Std.(add_line std.out)
          in List.iter ~f tokens; Std.(add_nl std.out)
      | _ -> ()

    (* Committing lexemes before running any pipeline to standard
       output/error if the CLI option "--copy" was given. *)

    let commit_lexemes lexemes std : unit =
      match Options.command with
        Some `Copy ->
          let f unit =
            List.iter ~f:Std.(add_string std.out)
              (Unit.to_lexeme ~token_to_lexeme:Token.to_lexeme unit)
          in List.iter ~f lexemes; Std.(add_nl std.out)
      | _ -> ()

    (* OUR LEXERS (USING THE LIBRARY LEXER) *)

    (* The type of the lexers that scan all their input into a list of
       items of type ['a]. *)

    type 'a lexer = Lexbuf.input -> Std.t * ('a list, 'a error) result

    (* Scanning all lexical units in the input given by the CLI. *)

    let scan_all_units ~no_colour (input : Lexbuf.input) =
      let file = Lexbuf.file_from_input input in
      let std, preprocessed, result =
        if Options.preprocess then
          (* Running the preprocessor *)
          match Preprocessor.preprocess input with
            std, Error (preprocessed, message) ->
              let error = Stdlib.Error Scan.{used_units=[]; message}
              in std, preprocessed, error
          | std, Ok (preprocessed, _deps) ->
              let lexbuf =
                Lexing.from_string @@ Buffer.contents preprocessed in
              let () = Lexbuf.reset_file file lexbuf in
              let preprocessed = Some preprocessed in
              (* Running the lexer *)
              std, preprocessed, Scan.from_lexbuf ~file lexbuf
        else
          (* Running only the lexer *)
          let result =
            match input with
              File    file               -> Scan.from_file file
            | Buffer  (file, buf)        -> Scan.from_buffer  ~file buf
            | String  (file, string)     -> Scan.from_string  ~file string
            | Channel (file, in_channel) -> Scan.from_channel ~file in_channel
            | Lexbuf  (file, lexbuf)     -> Scan.from_lexbuf  ~file lexbuf
          in
          Std.empty, None, result
      in
      match result with
        Stdlib.Error {used_units; message} ->
          let used_items = used_units in
          commit_units used_units std; (* If "--units" *)
          Std.(add_line std.err @@ format_error no_colour file message);
          Std.(add_nl std.err);
          std, Stdlib.Error {preprocessed; used_items; message}

      | Ok units ->
          let () = commit_lexemes units std in (* If "--copy" *)
          (* Applying the pipeline of self-passes on lexical units. *)
          let print_passes =
            if Options.print_passes then Some std else None in
          match
            UnitPipeline.run ?print_passes ~add_warning ~last units
          with
            Ok units as ok ->
              commit_units units std; (std, ok)
          | Error {used_items; message} ->
              commit_units used_items std;
              Std.(add_line std.err @@ format_error no_colour file message);
              Std.(add_nl std.err);
              std, Error {preprocessed; used_items; message}

    (* On the one hand, parsers generated by Menhir are functions that
       expect a parameter of type [Lexing.lexbuf -> token]. On the
       other hand, we want to enable self-passes on the tokens (See
       module [Pipeline]), which implies that we scan the whole
       input before parsing. Therefore, we make believe to the
       Menhir-generated parser that we scan tokens one by one, when,
       in fact, we have scanned them all already.

       We need to use global references to store information as a way
       to work around the signature of the parser generated by Menhir.

         * The global reference [called] tells us whether the lexer
           [scan] has been called before or not. If not, this triggers
           the scanning of all the tokens; if so, a token is extracted
           from the global reference [tokens]. The reference [called]
           is reset by calling [clear]. This is useful when the
           process running the compiler scans multiple inputs
           sequentially.

         * The global reference [used_tokens] holds all the tokens
           from a given source. The function [scan] updates it the
           first time it is called (see [called] above).

       In particular, when running the parsers twice, we have to call
       [clear] in-between to reset the global state and force the
       scanning of all the tokens from the new lexing buffer. *)

    (* Filtering out the markup *)

    let filter_tokens units : token list =
      let apply tokens = function
        `Token token -> token :: tokens
      | `Markup _    -> tokens
      | `Directive d -> Token.mk_directive d :: tokens
      in List.fold_left ~f:apply ~init:[] units |> List.rev

    (* Scanning all tokens in the input given by the CLI. *)

    let scan_all_tokens : no_colour:bool -> token lexer =
      fun ~no_colour ->
      fun (input : Lexbuf.input) ->
        match scan_all_units ~no_colour input with
          std, Error {preprocessed; used_items; message} ->
            let used_items = filter_tokens used_items in
            std, Error {preprocessed; used_items; message}

        | std, Stdlib.Ok units ->
            let tokens = filter_tokens units
            in
            if last <= num_of_unit_passes then
              (* There are no self-passes requested on the tokens *)
              (commit_tokens tokens std; (std, Ok tokens))
            else
              (* There are token self-passes requested, so applying
                 them here after the self-passes on the lexical
                 units. *)
              let last = last - num_of_unit_passes in
              let print_passes =
                if Options.print_passes then Some std else None in
              match
                TokenPipeline.run ?print_passes ~add_warning ~last tokens
              with
                Error {used_items; message} ->
                  let file = Lexbuf.file_from_input input in
                  Std.(add_line std.err (format_error no_colour file message));
                  Std.(add_nl std.err);
                  std, Error {preprocessed=None; used_items; message}
              | Ok tokens as ok ->
                  commit_tokens tokens std; (std, ok)

    (* Scanning tokens one by one, based on [scan_all_tokens]. Aimed
       at Menhir-generated parers (module the [result] type, which
       needs to be converted to an exception by the client of this
       function). *)

    (* All tokens scanned up to a parse error: for debugging *)

    let used_tokens : token list ref = ref []

    (* Whether the lexer has been called or not *)

    let called : bool ref = ref false

    (* Resetting the lexer *)

    let clear () = (used_tokens := []; called := false)

    let rec scan_token : no_colour:bool -> Lexing.lexbuf -> (token, token error) result =
      let store = ref ([] : token list) in
      fun ~no_colour lexbuf ->
        if !called then
          let token =
            match !store with
              [] ->
                Token.mk_eof Region.ghost
            | token :: tokens ->
                let region = Token.to_region token in
                let start, stop = region#byte_pos in
                (* Patching the lexing buffer with the positions from
                   the token's region, so the view of Menhir is
                   coherent. *)
                lexbuf.Lexing.lex_start_p <- start;
                lexbuf.Lexing.lex_curr_p  <- stop;
                (* Updating the remaining tokens and the used tokens. *)
                used_tokens := token :: !used_tokens;
                store := tokens;
                token
          in Stdlib.Ok token
        else
          (* Dropping standard output/error *)
          let file  = Lexbuf.current_filename lexbuf in
          let input = Lexbuf.Lexbuf (file, lexbuf) in
          let _std, result = scan_all_tokens ~no_colour input in  (* TODO *)
          match result with
            Stdlib.Error {used_items; _} as err ->
              used_tokens := used_items; err
          | Ok tokens ->
              store  := tokens;
              called := true;
              scan_token ~no_colour lexbuf

    (* Getting the used tokens *)

    let used_tokens () = List.rev !used_tokens
  end
