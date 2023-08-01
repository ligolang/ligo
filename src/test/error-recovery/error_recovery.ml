module type CST = sig type t type expr end

module PrettyPrinterType (CST : CST) = struct
  module type PrettyPrinter = sig
    type state
    val default_state : state
    val print : state -> CST.t -> PPrint.document
  end
end

module CSTPrinterType (CST : CST) = struct
  module type CSTPrinter = sig
    val to_string : Cst_shared.Tree.state -> CST.t -> string
  end
end

module RawParserType (CST : CST) = struct
  module type RawParser = sig
    type token
    exception Error
    val interactive_expr : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> CST.expr
    val contract : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> CST.t
    module MenhirInterpreter : MenhirLib.IncrementalEngine.EVERYTHING
           with type token = token
    module Incremental :
      sig
        val interactive_expr :
          Lexing.position -> CST.expr MenhirInterpreter.checkpoint

        val contract :
          Lexing.position -> CST.t MenhirInterpreter.checkpoint
      end
  end
end

module RecoveryType (MenhirInterpreter : MenhirLib.IncrementalEngine.EVERYTHING) = struct
  module type RecoverParser = sig
    include Merlin_recovery.RECOVERY_GENERATED
      with module I := MenhirInterpreter
    val default_value : Simple_utils.Region.t -> 'a MenhirInterpreter.symbol -> 'a
  end
end

module RecoveryTester
  (CST : CST)
  (PrettyPrinter : PrettyPrinterType(CST).PrettyPrinter)
  (CSTPrinter : CSTPrinterType(CST).CSTPrinter)
  (Config : Preprocessor.Config.S)
  (Token : Lexing_shared.Token.S)
  (ParErr : ParserLib.LowAPI.PAR_ERR)
  (UnitPasses : Lexing_shared.Pipeline.PASSES with type item = Token.t LexerLib.Unit.t)
  (TokenPasses : Lexing_shared.Pipeline.PASSES with type item = Token.t)
  (RawParser : RawParserType(CST).RawParser with type token = Token.t)
  (RecoverParser : RecoveryType(RawParser.MenhirInterpreter).RecoverParser) = struct

  module Parser = struct
      module CST = CST
      include RawParser
      module Recovery = RecoverParser

      type tree = CST.t

      let main = contract

      module Incremental =
        struct
          let main = Incremental.contract
        end
  end

  module ParserLexerConfig = struct
    let jsligo = None
    let preprocess = false
    let project_root = None
    let file_path = None
  end

  module ParserLexerBuilder = Parsing_shared.Common.MakeParser (Config) (Token)
    (ParErr) (UnitPasses) (TokenPasses) (struct type tree = CST.t end) (Parser)

  let raise : (Parsing_shared.Errors.t, Main_warnings.all) Simple_utils.Trace.raise = {
    error = (fun _ -> failwith "Shouldn't raise");
    warning = (fun _ -> ());
    log_error = (fun _ -> ());
    fast_fail = false;
  }

  module Options = struct
    let jsligo = None
    let preprocess = false
    let project_root = None
    let raise = raise
    let file_path = None
  end

  module ParserLexer = ParserLexerBuilder.ParserLexerGenerator (Options)

  open ParserLexer.Lexer
  open ParserLexer.Parser
  open File
  open Diff

  let no_colour = true

  let print_pretty cst out_buffer =
    PPrint.ToBuffer.pretty 1.0 80 out_buffer PrettyPrinter.(print default_state cst)
  let cst_state () =
    Cst_shared.Tree.mk_state ~offsets:false ~regions:false ~layout:true `Point
  let cst_symbols_state () =
    Cst_shared.Tree.mk_state ~offsets:false ~regions:false ~layout:false `Point
  let print_cst state cst out_buffer =
    Buffer.add_string out_buffer @@ CSTPrinter.to_string state cst
  let print_list ?(sep = "") out_buffer =
    List.iter ~f:(fun s -> Buffer.add_string out_buffer s; Buffer.add_string out_buffer sep)

  let tokens_formatter token = Token.to_string ~offsets:false `Point token
  let print_tokens tokens out_buffer =
    tokens
    |> List.map  ~f:tokens_formatter
    |> print_list ~sep:"\n" out_buffer

  let parser_error_formatter : pass_error -> string = function
    | Parsing e | Lexing e | System e -> e.message.value

  let recovered_error_formatter (error : message) =
    (format_error ~no_colour ~file:true error.value error.region).value
  let print_recovered_errors errors out_buffer =
    let formatter_errors = List.map ~f:recovered_error_formatter errors in
    List.iter ~f:(Buffer.add_string out_buffer) formatter_errors;
    formatter_errors

  let print_parser_erorrs error out_buffer =
    let parser_error = parser_error_formatter error in
    Buffer.add_string out_buffer parser_error;
    parser_error

  let parse_file = incr_from_file ~no_colour (module ParErr)
  let parse_string = incr_from_string ~no_colour (module ParErr)
  let recovery_file = recov_from_file ~no_colour:true (module ParErr)
  let lex_tokens in_buffer =
    clear ();
    snd @@ scan_all_tokens ~no_colour (Simple_utils.Lexbuf.Buffer (".", in_buffer))
  let print_buffer buffer file =
    Printf.fprintf (Out_channel.create file) "%s" (Buffer.contents buffer)

  let reverse_result : ('a, 'b) result -> ('b, 'a) result = function
  | Error x -> Ok x
  | Ok x -> Error x

  let test_file file =
    let open Result.Let_syntax in
    let default_buffer () = Buffer.create 1024 in
    let original_pretty       = default_buffer () in
    let original_cst          = default_buffer () in
    let original_cst_symbols  = default_buffer () in
    let original_tokens       = default_buffer () in
    let original_errors       = default_buffer () in
    let recovered_pretty      = default_buffer () in
    let recovered_cst         = default_buffer () in
    let recovered_cst_symbols = default_buffer () in
    let recovered_tokens      = default_buffer () in
    let recovered_errors      = default_buffer () in

    let replace = Result.map ~f:(fun _ -> "FAIL : can parse test file (but shouldn't)") in
    let%bind error = reverse_result @@ replace @@ parse_file (to_string file (Some Blank) Test) in

    let parser_error = print_parser_erorrs error original_errors in

    let replace = Result.map_error ~f:(fun _ -> "FAIL : can't parse original file") in
    let%bind cst = replace @@ parse_file (to_string file (Some Blank) Original) in

    print_pretty cst original_pretty;
    print_cst (cst_state ()) cst original_cst;
    print_cst (cst_symbols_state ()) cst original_cst_symbols;

    let replace = Result.map_error ~f:(fun _ -> "FAIL : can't get original file tokens") in
    let%bind tokens = replace @@ lex_tokens original_pretty in

    print_tokens tokens original_tokens;

    let replace = Result.map_error ~f:(fun _ -> "FAIL : can't recover test file") in
    let%bind (cst, error) = replace @@ recovery_file (to_string file (Some Blank) Test) in

    print_pretty cst recovered_pretty;
    print_cst (cst_state ()) cst recovered_cst;
    print_cst (cst_symbols_state ()) cst recovered_cst_symbols;
    let recovered_errors_list = print_recovered_errors error recovered_errors in

    let replace = Result.map_error ~f:(fun _ -> "FAIL : can't parse recovered file") in
    let%bind _ = replace @@ parse_string (Buffer.contents recovered_pretty) in

    let replace = Result.map_error ~f:(fun _ -> "FAIL : can't get recovered file tokens") in
    let%bind tokens = replace @@ lex_tokens recovered_pretty in

    print_tokens tokens recovered_tokens;

    let loc_diff = Diff.diff_files original_pretty recovered_pretty in
    let cst_diff = Diff.diff_files original_cst recovered_cst in
    let symbols_diff = Diff.diff_files original_cst_symbols  recovered_cst_symbols in
    let tokens_diff = Diff.diff_files original_tokens recovered_tokens in
    let replace = Result.map_error ~f:(fun _ -> "FAIL : original error is missing") in
    let%bind err_diff = replace @@ Diff.diff_errors parser_error recovered_errors_list in

    print_buffer original_pretty @@ to_string file (Some Blank) Original_generated;
    print_buffer original_cst @@ to_string file (Some Cst) Original_generated;
    print_buffer original_cst_symbols @@ to_string file (Some Cst_symbols) Original_generated;
    print_buffer original_tokens @@ to_string file (Some Tokens) Original_generated;
    print_buffer original_errors @@ to_string file (Some Errors) Original_generated;

    print_buffer recovered_pretty @@ to_string file (Some Blank) Recovered;
    print_buffer recovered_cst @@ to_string file (Some Cst) Recovered;
    print_buffer recovered_cst_symbols @@ to_string file (Some Cst_symbols) Recovered;
    print_buffer recovered_tokens @@ to_string file (Some Tokens) Recovered;
    print_buffer recovered_errors @@ to_string file (Some Errors) Recovered;

    Ok (Printf.sprintf "PASS, %d, %d, %d, %d, %d, "
      loc_diff cst_diff symbols_diff tokens_diff err_diff)

  let from_result (result : ('a, 'a) result) : 'a =
    match result with
    | Ok x | Error x -> x

  let test () =
    Ligo_unix.mkdir_p ~perm:0o700 (to_string "" None Recovered);
    Ligo_unix.mkdir_p ~perm:0o700 (to_string "" None Original_generated);
    let files = List.sort ~compare:String.compare @@ files_in_dir () in
    let result = Buffer.create 1024 in
    Buffer.add_string result "STATUS, LOC, CST, SYMBOLS, TOKENS, ERRORS, FILE\n";
    List.iter ~f:(fun file -> Buffer.add_string result @@ from_result @@ test_file file;
                              Buffer.add_string result @@ to_string file (Some Blank) Test;
                              Buffer.add_string result "\n"; ) files;
    Buffer.contents result
end

let test_cameligo =
  let module LexerOptions =
    LexerLib.Options.MakeDefault (Preprocessor.Options.Default) in
  let module CST            = Cst_cameligo.CST in
  let module Pretty         = Parsing_cameligo.Pretty in
  let module Print          = Cst_cameligo.Print in
  let module Config         = Preprocessing_cameligo.Config in
  let module Token          = Lexing_cameligo.Token in
  let module ParErr         = Parsing_cameligo.ParErr in
  let module UnitPasses     = Lx_ml_self_units.Self.Make (LexerOptions) in
  let module TokenPasses    = Lx_ml_self_tokens.Self.Make (LexerOptions) in
  let module RawParser      = Parsing_cameligo.Parser in
  let module Recovery       = Parsing_cameligo.RecoverParser in
  let module RecoveryTester = RecoveryTester (CST) (Pretty) (Print) (Config)
    (Token) (ParErr) (UnitPasses) (TokenPasses) (RawParser) (Recovery) in
  RecoveryTester.test

let test_jsligo =
  let module LexerOptions =
    LexerLib.Options.MakeDefault (Preprocessor.Options.Default) in
  let module CST            = Cst_jsligo.CST in
  let module Pretty         = Parsing_jsligo.Pretty in
  let module Print          = Cst_jsligo.Print in
  let module Config         = Preprocessing_jsligo.Config in
  let module Token          = Lexing_jsligo.Token in
  let module ParErr         = Parsing_jsligo.ParErr in
  let module UnitPasses     = Lx_js_self_units.Self.Make (LexerOptions) in
  let module TokenPasses    = Lx_js_self_tokens.Self.Make (LexerOptions) in
  let module RawParser      = Parsing_jsligo.Parser in
  let module Recovery       = Parsing_jsligo.RecoverParser in
  let module RecoveryTester = RecoveryTester (CST) (Pretty) (Print) (Config)
    (Token) (ParErr) (UnitPasses) (TokenPasses) (RawParser) (Recovery) in
  RecoveryTester.test

  let test_pascaligo =
    let module LexerOptions =
      LexerLib.Options.MakeDefault (Preprocessor.Options.Default) in
    let module CST            = Cst_pascaligo.CST in
    let module Pretty         = Parsing_pascaligo.Pretty in
    let module Print          = Cst_pascaligo.Print in
    let module Config         = Preprocessing_pascaligo.Config in
    let module Token          = Lexing_pascaligo.Token in
    let module ParErr         = Parsing_pascaligo.ParErr in
    let module UnitPasses     = Lx_psc_self_units.Self.Make (LexerOptions) in
    let module TokenPasses    = Lx_psc_self_tokens.Self.Make (LexerOptions) in
    let module RawParser      = Parsing_pascaligo.Parser in
    let module Recovery       = Parsing_pascaligo.RecoverParser in
    let module RecoveryTester = RecoveryTester (CST) (Pretty) (Print) (Config)
      (Token) (ParErr) (UnitPasses) (TokenPasses) (RawParser) (Recovery) in
    RecoveryTester.test
