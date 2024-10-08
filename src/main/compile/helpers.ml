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
    | JsLIGO -> Jsligo.preprocess_file
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
    | JsLIGO -> Jsligo.preprocess_string
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
    | JsLIGO -> Jsligo.preprocess_raw_input
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


let parse_and_abstract_jsligo ~raise ~preprocess_define buffer file_path =
  let module Parse = Parsing.Jsligo.Make (Jsligo.Options) in
  let raw =
    Trace.trace ~raise parser_tracer
    @@ Parse.parse_file ~preprocess_define buffer file_path
  in
  Unification.Jsligo.compile_program raw


let parse_and_abstract_expression_jsligo ~raise ~preprocess_define buffer =
  let module Parse = Parsing.Jsligo.Make (Jsligo.Options) in
  let raw =
    Trace.trace ~raise parser_tracer @@ Parse.parse_expression ~preprocess_define buffer
  in
  Unification.Jsligo.compile_expression raw


let parse_and_abstract_type_expression_jsligo ~raise ~preprocess_define buffer =
  let module Parse = Parsing.Jsligo.Make (Jsligo.Options) in
  let raw =
    Trace.trace ~raise parser_tracer
    @@ Parse.parse_type_expression ~preprocess_define buffer
  in
  Unification.Jsligo.compile_type_expression raw


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


let parse_and_abstract_string_jsligo ~raise ~preprocess_define buffer =
  let module Parse = Parsing.Jsligo.Make (Jsligo.Options) in
  let raw =
    Trace.trace ~raise parser_tracer @@ Parse.parse_string ~preprocess_define buffer
  in
  Unification.Jsligo.compile_program raw


let parse_and_abstract_string ~raise (syntax : Syntax_types.t) buffer =
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
