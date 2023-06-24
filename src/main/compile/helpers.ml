open Simple_utils.Trace
open Main_errors

type meta = { syntax : Syntax_types.t }

let protocol_to_variant ~raise : string -> Environment.Protocols.t =
 fun s ->
  trace_option
    ~raise
    (main_invalid_protocol_version Environment.Protocols.protocols_str s)
  @@ Environment.Protocols.protocols_to_variant s


(* Preprocessing *)

type options = Compiler_options.t

let preprocess_file ~raise ~(options : Compiler_options.frontend) ~(meta : meta) file_path
    : Preprocessor.LowAPI.success
  =
  let open Preprocessing in
  let Compiler_options.{ project_root; libraries; _ } = options in
  let preprocess_file =
    match meta.syntax with
    | CameLIGO -> Cameligo.preprocess_file
    | JsLIGO -> Jsligo.preprocess_file
    | PascaLIGO -> Pascaligo.preprocess_file
  in
  trace ~raise preproc_tracer
  @@ Simple_utils.Trace.from_result (preprocess_file ?project_root libraries file_path)


let preprocess_string
    ~raise
    ~(options : Compiler_options.frontend)
    ~(meta : meta)
    file_path
  =
  let open Preprocessing in
  let Compiler_options.{ project_root; libraries; _ } = options in
  let preprocess_string =
    match meta.syntax with
    | CameLIGO -> Cameligo.preprocess_string
    | JsLIGO -> Jsligo.preprocess_string
    | PascaLIGO -> Pascaligo.preprocess_string
  in
  trace ~raise preproc_tracer
  @@ from_result (preprocess_string ?project_root libraries file_path)


let preprocess_raw_input
    ~raise
    ~(options : Compiler_options.frontend)
    ~(meta : meta)
    file_path
    input
  =
  let open Preprocessing in
  let Compiler_options.{ project_root; libraries; _ } = options in
  let preprocess_raw_input =
    match meta.syntax with
    | CameLIGO -> Cameligo.preprocess_raw_input
    | JsLIGO -> Jsligo.preprocess_raw_input
    | PascaLIGO -> Pascaligo.preprocess_raw_input
  in
  trace ~raise preproc_tracer
  @@ from_result (preprocess_raw_input ?project_root libraries (file_path, input))


(* Front-end compilation *)

type file_path = string

module Config = Preprocessing_cameligo.Config
module PreprocParams = Preprocessor.CLI.MakeDefault (Config)
module LexerParams = LexerLib.CLI.MakeDefault (PreprocParams)
module Parameters = ParserLib.CLI.MakeDefault (LexerParams)
module Options = Parameters.Options

let parse_and_abstract_pascaligo
    ~(raise : (Main_errors.all, Main_warnings.all) Simple_utils.Trace.raise)
    buffer
    file_path
  =
  let module Parse = Parsing.Pascaligo.Make (Options) in
  let raw = trace ~raise parser_tracer @@ Parse.parse_file buffer file_path in
  Unification.Pascaligo.compile_program raw


let parse_and_abstract_expression_pascaligo
    ~(raise : (Main_errors.all, Main_warnings.all) Simple_utils.Trace.raise)
    buffer
  =
  let module Parse = Parsing.Pascaligo.Make (Options) in
  let raw = trace ~raise parser_tracer @@ Parse.parse_expression buffer in
  Unification.Pascaligo.compile_expression raw


let parse_and_abstract_cameligo ~raise buffer file_path =
  let module Parse = Parsing.Cameligo.Make (Options) in
  let raw = trace ~raise parser_tracer @@ Parse.parse_file buffer file_path in
  Unification.Cameligo.compile_program raw


let parse_and_abstract_expression_cameligo ~raise buffer =
  let module Parse = Parsing.Cameligo.Make (Options) in
  let raw = trace ~raise parser_tracer @@ Parse.parse_expression buffer in
  Unification.Cameligo.compile_expression raw


let parse_and_abstract_jsligo ~raise buffer file_path =
  let module Parse = Parsing.Jsligo.Make (Options) in
  let raw = trace ~raise parser_tracer @@ Parse.parse_file buffer file_path in
  Unification.Jsligo.compile_program raw


let parse_and_abstract_expression_jsligo ~raise buffer =
  let module Parse = Parsing.Jsligo.Make (Options) in
  let raw = trace ~raise parser_tracer @@ Parse.parse_expression buffer in
  Unification.Jsligo.compile_expression raw


let parse_and_abstract ~raise ~(meta : meta) buffer file_path : Ast_unified.program =
  let parse_and_abstract =
    match meta.syntax with
    | CameLIGO -> parse_and_abstract_cameligo
    | JsLIGO -> parse_and_abstract_jsligo
    | PascaLIGO -> parse_and_abstract_pascaligo
  in
  parse_and_abstract ~raise buffer file_path


let parse_and_abstract_expression
    ~(raise : (Main_errors.all, Main_warnings.all) Simple_utils.Trace.raise)
    ~(meta : meta)
    buffer
    : Ast_unified.expr
  =
  let parse_and_abstract =
    match meta.syntax with
    | CameLIGO -> parse_and_abstract_expression_cameligo
    | JsLIGO -> parse_and_abstract_expression_jsligo
    | PascaLIGO -> parse_and_abstract_expression_pascaligo
  in
  parse_and_abstract ~raise buffer


let parse_and_abstract_string_cameligo ~raise buffer =
  let module Parse = Parsing.Cameligo.Make (Options) in
  let raw = trace ~raise parser_tracer @@ Parse.parse_string buffer in
  Unification.Cameligo.compile_program raw


let parse_and_abstract_string_jsligo ~raise buffer =
  let module Parse = Parsing.Jsligo.Make (Options) in
  let raw = trace ~raise parser_tracer @@ Parse.parse_string buffer in
  Unification.Jsligo.compile_program raw


let parse_and_abstract_string_pascaligo ~raise buffer =
  let module Parse = Parsing.Pascaligo.Make (Options) in
  let raw = trace ~raise parser_tracer @@ Parse.parse_string buffer in
  Unification.Pascaligo.compile_program raw


let parse_and_abstract_string ~raise (syntax : Syntax_types.t) buffer =
  let parse_and_abstract =
    match syntax with
    | CameLIGO -> parse_and_abstract_string_cameligo
    | JsLIGO -> parse_and_abstract_string_jsligo
    | PascaLIGO -> parse_and_abstract_string_pascaligo
  in
  parse_and_abstract ~raise buffer


let pretty_print_cameligo_cst ?preprocess ?project_root ~raise buffer file_path =
  let module Parse = Parsing.Cameligo.Make (Options) in
  Parse.pretty_print_cst ?preprocess ?project_root ~raise buffer file_path


let pretty_print_jsligo_cst ?preprocess ?project_root ~raise buffer file_path =
  let module Parse = Parsing.Jsligo.Make (Options) in
  Parse.pretty_print_cst ?preprocess ?project_root ~raise buffer file_path


let pretty_print_pascaligo_cst ?preprocess ?project_root ~raise buffer file_path =
  let module Parse = Parsing.Pascaligo.Make (Options) in
  Parse.pretty_print_cst ?preprocess ?project_root ~raise buffer file_path


let pretty_print_cst ~raise ~(meta : meta) buffer file_path =
  let print =
    match meta.syntax with
    | CameLIGO -> pretty_print_cameligo_cst
    | JsLIGO -> pretty_print_jsligo_cst
    | PascaLIGO -> pretty_print_pascaligo_cst
  in
  trace ~raise parser_tracer @@ print buffer file_path


let pretty_print_cameligo ?jsligo ?preprocess ?project_root ~raise buffer file_path =
  let module Options = struct
    include Options

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
    include Options

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


let pretty_print_pascaligo ?jsligo ?preprocess ?project_root ~raise buffer file_path =
  let module Options = struct
    include Options

    let jsligo = jsligo
  end
  in
  let module Parse = Parsing.Pascaligo.Make (Options) in
  Parse.pretty_print_file
    Parsing.Pascaligo.Pretty.default_state
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
    | PascaLIGO -> pretty_print_pascaligo
  in
  trace ~raise parser_tracer @@ print ?preprocess buffer file_path
