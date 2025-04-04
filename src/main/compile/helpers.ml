module Trace = Simple_utils.Trace
open Main_errors

type meta = { syntax : Syntax_types.t }

(* Preprocessing *)

type options = Compiler_options.t

let preprocess_file ~raise ~(options : Compiler_options.frontend) ~(meta : meta) file_path
    : Preprocessor.LowAPI.success
  =
  let open Preprocessing in
  let Compiler_options.{ project_root; libraries; preprocess_define; _ } = options in
  let preprocess_file =
    match meta.syntax with
    | CameLIGO -> Cameligo.preprocess_file
    | JsLIGO ->
      fun ?project_root ~preprocess_define dirs src ->
        let input : string = In_channel.read_all src in
        let buffer = Buffer.create (String.length input) in
        let () = Buffer.add_string buffer input in
        Ok (buffer, [])
  in
  Trace.trace ~raise preproc_tracer
  @@ Simple_utils.Trace.from_result
       (preprocess_file ?project_root ~preprocess_define libraries file_path)


let preprocess_string
    ~raise
    ~(options : Compiler_options.frontend)
    ~(meta : meta)
    file_path
  =
  let open Preprocessing in
  let Compiler_options.{ project_root; libraries; preprocess_define; _ } = options in
  let preprocess_string =
    match meta.syntax with
    | CameLIGO -> Cameligo.preprocess_string
    | JsLIGO ->
      fun ?project_root ~preprocess_define dirs input ->
        let buffer = Buffer.create (String.length input) in
        let () = Buffer.add_string buffer input in
        Ok (buffer, [])
  in
  Trace.trace ~raise preproc_tracer
  @@ Trace.from_result
       (preprocess_string ?project_root ~preprocess_define libraries file_path)


let preprocess_raw_input
    ~raise
    ~(options : Compiler_options.frontend)
    ~(meta : meta)
    file_path
    input
  =
  let open Preprocessing in
  let Compiler_options.{ project_root; libraries; preprocess_define; _ } = options in
  let preprocess_raw_input =
    match meta.syntax with
    | CameLIGO -> Cameligo.preprocess_raw_input
    | JsLIGO ->
      fun ?project_root ~preprocess_define dirs (_file, input) ->
        let buffer = Buffer.create (String.length input) in
        let () = Buffer.add_string buffer input in
        Ok (buffer, [])
  in
  Trace.trace ~raise preproc_tracer
  @@ Trace.from_result
       (preprocess_raw_input
          ?project_root
          ~preprocess_define
          libraries
          (file_path, input))


(* Front-end compilation *)

type file_path = string

module Make (Config : Preprocessor.Config.S) = struct
  module PreprocParams = Preprocessor.CLI.MakeDefault (Config)
  module LexerParams = LexerLib.CLI.MakeDefault (PreprocParams)
  module Parameters = ParserLib.CLI.MakeDefault (LexerParams)
  module Options = Parameters.Options
end

module Cameligo = Make (Preprocessing_cameligo.Config)
module Jsligo = Make (Preprocessing_jsligo.Config)

let parse_and_abstract_cameligo ~raise ~preprocess_define buffer file_path =
  let module Parse = Parsing.Cameligo.Make (Cameligo.Options) in
  let raw =
    Trace.trace ~raise parser_tracer
    @@ Parse.parse_file buffer ~preprocess_define file_path
  in
  Unification.Cameligo.compile_program raw


let parse_and_abstract_expression_cameligo ~raise ~preprocess_define buffer =
  let module Parse = Parsing.Cameligo.Make (Cameligo.Options) in
  let raw =
    Trace.trace ~raise parser_tracer @@ Parse.parse_expression ~preprocess_define buffer
  in
  Unification.Cameligo.compile_expression raw


let parse_and_abstract_type_expression_cameligo ~raise ~preprocess_define buffer =
  let module Parse = Parsing.Cameligo.Make (Cameligo.Options) in
  let raw =
    Trace.trace ~raise parser_tracer
    @@ Parse.parse_type_expression ~preprocess_define buffer
  in
  Unification.Cameligo.compile_type_expression raw


(* Tree-sitter ctypes-APIs for types and related functions *)

module TS_types = Tree_sitter.Api.Types
module TS_fun = Tree_sitter.Api.Functions
module Ts_wrap = Typescript_ast.Ts_wrap
module Loc_map = Typescript_ast.Loc_map
module Decode = Typescript_decoder.Decode
module Strip = Typescript_stripper.Strip
module Ast = Typescript_ast.Ast
module Ast_stripped = Typescript_stripper.Ast_stripped
module Region = Simple_utils.Region

let ( let* ) v f = Result.bind v ~f

let lift ~(raise : (Main_errors.all, Main_warnings.all) Simple_utils.Trace.raise)
  = function
  | Ok tree -> tree
  | Error error -> raise.error @@ `Parser_tracer (`Parsing error)


(* JsLIGO programs *)

let decode_jsligo_program ~raise buffer filename : (Ast.t, string Region.reg) result =
  (* Loading the code as a string *)
  let input : string = Buffer.contents buffer in
  (* Building the map from line-column pairs to positions [Pos.t] *)
  let line_map : Loc_map.t = Loc_map.scan_string input in
  (* Parsing the code into a tree *)
  let tree : Ts_wrap.ts_tree_ptr = Ts_wrap.parse_typescript_string input in
  (* Getting ahold of the root of the tree *)
  let program_node : Ts_wrap.ts_tree = TS_fun.ts_tree_root_node tree in
  (* Decoding the tree *)
  let ast = Decode.dec_program ~filename ~file:input line_map program_node in
  (* Releasing the memory allocated to the tree *)
  let () = TS_fun.ts_tree_delete tree in
  ast


let decode_jsligo_program ~raise buffer filename =
  lift ~raise @@ decode_jsligo_program ~raise buffer filename


let parse_and_abstract_jsligo ~raise ~preprocess_define (buffer : Buffer.t) file_path =
  ignore preprocess_define;
  let ast = decode_jsligo_program ~raise buffer file_path in
  let stripped = lift ~raise (Strip.statements ast) in
  Unification.Jsligo.compile_program stripped


(* JsLIGO expressions *)

let decode_jsligo_expression ~raise buffer : (Ast.expression, string Region.reg) result =
  let input = Buffer.contents buffer in
  (* Building the map from line-column pairs to positions [Pos.t] *)
  let line_map : Loc_map.t = Loc_map.scan_string input in
  (* Parsing the code into a tree *)
  let tree : Ts_wrap.ts_tree_ptr = Ts_wrap.parse_typescript_string input in
  (* Getting ahold of the root of the tree *)
  let program_node : Ts_wrap.ts_tree = TS_fun.ts_tree_root_node tree in
  (* Decoding the tree *)
  let ast = Decode.dec_standalone_expression ~file:input line_map program_node in
  (* Releasing the memory allocated to the tree *)
  let () = TS_fun.ts_tree_delete tree in
  ast


let decode_jsligo_expression ~raise buffer =
  lift ~raise @@ decode_jsligo_expression ~raise buffer


let parse_and_abstract_expression_jsligo ~raise ~preprocess_define buffer =
  ignore preprocess_define;
  let ast = decode_jsligo_expression ~raise buffer in
  let stripped = lift ~raise (Strip.strip_expression ast) in
  Unification.Jsligo.compile_expression stripped


(* JsLIGO type expressions *)

let decode_jsligo_type_expression ~raise buffer
    : (Ast.type_expr, string Region.reg) result
  =
  let line_map : Loc_map.t = Map.set Int.Map.empty ~key:1 ~data:0 in
  let input = Buffer.contents buffer in
  (* We prefix the string "type t = " to the input to parse it as a program *)
  let input = "type t = " ^ input in
  (* Parsing the code into a tree *)
  let tree : Ts_wrap.ts_tree_ptr = Ts_wrap.parse_typescript_string input in
  (* Getting ahold of the root of the tree *)
  let program_node : Ts_wrap.ts_tree = TS_fun.ts_tree_root_node tree in
  (* Decoding the tree *)
  let ast = Decode.dec_standalone_type_expr line_map program_node in
  (* Releasing the memory allocated to the tree *)
  let () = TS_fun.ts_tree_delete tree in
  ast


let decode_jsligo_type_expression ~raise buffer =
  lift ~raise @@ decode_jsligo_type_expression ~raise buffer


let parse_and_abstract_type_expression_jsligo ~raise ~preprocess_define buffer =
  ignore preprocess_define;
  let ast = decode_jsligo_type_expression ~raise buffer in
  let stripped = lift ~raise (Strip.strip_type_expr ast) in
  Unification.Jsligo.compile_type_expression stripped


(* CameLIGO or JsLIGO *)

let parse_and_abstract ~raise ~(meta : meta) ~preprocess_define buffer file_path
    : Ast_unified.program
  =
  let parse_and_abstract =
    match meta.syntax with
    | CameLIGO -> parse_and_abstract_cameligo
    | JsLIGO -> parse_and_abstract_jsligo
  in
  parse_and_abstract ~raise ~preprocess_define buffer file_path


let parse_and_abstract_expression
    ~(raise : (Main_errors.all, Main_warnings.all) Simple_utils.Trace.raise)
    ~(meta : meta)
    ~preprocess_define
    buffer
    : Ast_unified.expr
  =
  let parse_and_abstract =
    match meta.syntax with
    | CameLIGO -> parse_and_abstract_expression_cameligo
    | JsLIGO -> parse_and_abstract_expression_jsligo
  in
  parse_and_abstract ~raise ~preprocess_define buffer


let parse_and_abstract_type_expression
    ~(raise : (Main_errors.all, Main_warnings.all) Simple_utils.Trace.raise)
    ~(meta : meta)
    ~preprocess_define
    buffer
    : Ast_unified.ty_expr
  =
  let parse_and_abstract =
    match meta.syntax with
    | CameLIGO -> parse_and_abstract_type_expression_cameligo
    | JsLIGO -> parse_and_abstract_type_expression_jsligo
  in
  parse_and_abstract ~raise ~preprocess_define buffer


let parse_and_abstract_string_cameligo ~raise ~preprocess_define buffer =
  let module Parse = Parsing.Cameligo.Make (Cameligo.Options) in
  let raw =
    Trace.trace ~raise parser_tracer @@ Parse.parse_string ~preprocess_define buffer
  in
  Unification.Cameligo.compile_program raw


let decode_string_jsligo ~raise buffer : (Ast.t, string Region.reg) result =
  (* Loading the code as a string *)
  let input : string = Buffer.contents buffer in
  (* Building the map from line-column pairs to positions [Pos.t] *)
  let line_map : Loc_map.t = Loc_map.scan_string input in
  (* Parsing the code into a tree *)
  let tree : Ts_wrap.ts_tree_ptr = Ts_wrap.parse_typescript_string input in
  (* Getting ahold of the root of the tree *)
  let program_node : Ts_wrap.ts_tree = TS_fun.ts_tree_root_node tree in
  (* Decoding the tree *)
  let ast = Decode.dec_program ~filename:"" ~file:input line_map program_node in
  (* Releasing the memory allocated to the tree *)
  let () = TS_fun.ts_tree_delete tree in
  ast


let decode_string_jsligo ~raise buffer = lift ~raise @@ decode_string_jsligo ~raise buffer

let parse_and_abstract_string_jsligo ~raise ~preprocess_define (buffer : Buffer.t) =
  ignore preprocess_define;
  let ast = decode_string_jsligo ~raise buffer in
  let stripped = lift ~raise (Strip.statements ast) in
  Unification.Jsligo.compile_program stripped


let parse_and_abstract_string ~raise (syntax : Syntax_types.t) (buffer : Buffer.t) =
  let parse_and_abstract =
    match syntax with
    | CameLIGO -> parse_and_abstract_string_cameligo
    | JsLIGO -> parse_and_abstract_string_jsligo
  in
  parse_and_abstract ~raise buffer


let pretty_print_cameligo_cst ?preprocess ?project_root ~raise buffer file_path =
  let module Parse = Parsing.Cameligo.Make (Cameligo.Options) in
  Parse.pretty_print_cst ?preprocess ?project_root ~raise buffer file_path


let pretty_print_jsligo_cst ?preprocess ?project_root ~raise buffer file_path =
  let module Parse = Parsing.Jsligo.Make (Jsligo.Options) in
  Parse.pretty_print_cst ?preprocess ?project_root ~raise buffer file_path


let pretty_print_cst ~raise ~(meta : meta) buffer file_path =
  let print =
    match meta.syntax with
    | CameLIGO -> pretty_print_cameligo_cst
    | JsLIGO -> pretty_print_jsligo_cst
  in
  Trace.trace ~raise parser_tracer @@ print buffer file_path


let pretty_print_cameligo ?jsligo ?preprocess ?project_root ~raise buffer file_path =
  let module Options = struct
    include Cameligo.Options

    let jsligo = jsligo
  end
  in
  let module Parse = Parsing.Cameligo.Make (Options) in
  Parse.pretty_print_file
    Parsing.Cameligo.Pretty.default_state
    ?jsligo
    ?preprocess
    ?project_root
    ~raise
    buffer
    file_path


let pretty_print_jsligo ?jsligo ?preprocess ?project_root ~raise buffer file_path =
  let module Options = struct
    include Jsligo.Options

    let jsligo = jsligo
  end
  in
  let module Parse = Parsing.Jsligo.Make (Options) in
  Parse.pretty_print_file
    Parsing.Jsligo.Pretty.default_state
    ?jsligo
    ?preprocess
    ?project_root
    ~raise
    buffer
    file_path


let pretty_print ?preprocess ~raise ~(meta : meta) buffer file_path =
  let print =
    match meta.syntax with
    | CameLIGO -> pretty_print_cameligo
    | JsLIGO -> pretty_print_jsligo
  in
  Trace.trace ~raise parser_tracer @@ print ?preprocess buffer file_path
