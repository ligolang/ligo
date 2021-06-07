open Trace
open Main_errors

type s_syntax = Syntax_name of string
type v_syntax = PascaLIGO | CameLIGO | ReasonLIGO | JsLIGO

type meta = {
  syntax : v_syntax;
}

let protocol_to_variant : string -> (Environment.Protocols.t, all) result =
  fun s ->
  trace_option (invalid_protocol_version Environment.Protocols.protocols_str s)
  @@ Environment.Protocols.protocols_to_variant s

let get_initial_env  : ?test_env:bool -> string -> (Ast_typed.environment, all) result = fun ?(test_env=false) protocol_as_str ->
  let* protocol = protocol_to_variant protocol_as_str in
  ok @@ (if test_env then Environment.default_with_test else Environment.default) protocol

(*TODO : move this function to src/helpers so that src/build/.. can use it *)
let file_extension_to_variant sf =
  match sf with
  | ".ligo" | ".pligo" -> Some PascaLIGO
  | ".mligo"           -> Some CameLIGO
  | ".religo"          -> Some ReasonLIGO
  | ".jsligo"          -> Some JsLIGO
  | _                  -> None

let syntax_to_variant (Syntax_name syntax) source =
  match syntax, source with
  | "auto", Some sf ->
    let sf = Filename.extension sf in
    trace_option (syntax_auto_detection sf) @@
      file_extension_to_variant sf
  | ("pascaligo" | "PascaLIGO"),   _ -> ok PascaLIGO
  | ("cameligo" | "CameLIGO"),     _ -> ok CameLIGO
  | ("reasonligo" | "ReasonLIGO"), _ -> ok ReasonLIGO
  | ("jsligo" | "JsLIGO"),         _ -> ok JsLIGO
  | _ -> fail (invalid_syntax syntax)

(* Preprocessing *)

type options = Compiler_options.t

let preprocess_file ~(options:options) ~meta file_path
  : (Preprocessing.Pascaligo.success, Main_errors.all) Trace.result =
  let open Preprocessing in
  let preprocess_file =
    match meta.syntax with
      PascaLIGO  -> Pascaligo.preprocess_file
    | CameLIGO   -> Cameligo.preprocess_file
    | ReasonLIGO -> Reasonligo.preprocess_file
    | JsLIGO     -> Jsligo.preprocess_file
  in trace preproc_tracer @@
     preprocess_file options.libs file_path

let preprocess_string ~(options:options) ~meta file_path =
  let open Preprocessing in
  let preprocess_string =
    match meta.syntax with
      PascaLIGO  -> Pascaligo.preprocess_string
    | CameLIGO   -> Cameligo.preprocess_string
    | ReasonLIGO -> Reasonligo.preprocess_string
    | JsLIGO     -> Jsligo.preprocess_string
  in trace preproc_tracer @@
     preprocess_string options.libs file_path

(* Front-end compilation *)

type file_path = string

let parse_and_abstract_pascaligo buffer file_path =
  let* raw =
    trace parser_tracer @@
    Parsing.Pascaligo.parse_file buffer file_path in
  let* applied =
    trace self_cst_pascaligo_tracer @@
    Self_cst.Pascaligo.all_module raw in
  let* imperative =
    trace cit_pascaligo_tracer @@
    Tree_abstraction.Pascaligo.compile_module applied
  in ok imperative

let parse_and_abstract_expression_pascaligo buffer =
  let* raw =
    trace parser_tracer @@
    Parsing.Pascaligo.parse_expression buffer in
  let* applied =
    trace self_cst_pascaligo_tracer @@
    Self_cst.Pascaligo.all_expression raw in
  let* imperative =
    trace cit_pascaligo_tracer @@
    Tree_abstraction.Pascaligo.compile_expression applied
  in ok imperative

let parse_and_abstract_cameligo buffer file_path =
  let* raw =
    trace parser_tracer @@
    Parsing.Cameligo.parse_file buffer file_path in
  let* applied =
    trace self_cst_cameligo_tracer @@
    Self_cst.Cameligo.all_module raw in
  let* imperative =
    trace cit_cameligo_tracer @@
    Tree_abstraction.Cameligo.compile_module applied
  in ok imperative

let parse_and_abstract_expression_cameligo buffer =
  let* raw =
    trace parser_tracer @@
    Parsing.Cameligo.parse_expression buffer in
  let* applied =
    trace self_cst_cameligo_tracer @@
    Self_cst.Cameligo.all_expression raw in
  let* imperative =
    trace cit_cameligo_tracer @@
    Tree_abstraction.Cameligo.compile_expression applied
  in ok imperative

let parse_and_abstract_reasonligo buffer file_path =
  let* raw =
    trace parser_tracer @@
    Parsing.Reasonligo.parse_file buffer file_path in
  let* applied =
    trace self_cst_reasonligo_tracer @@
    Self_cst.Reasonligo.all_module raw in
  let* imperative =
    trace cit_reasonligo_tracer @@
    Tree_abstraction.Reasonligo.compile_module applied
  in ok imperative

let parse_and_abstract_expression_reasonligo buffer =
  let* raw =
    trace parser_tracer @@
    Parsing.Reasonligo.parse_expression buffer in
  let* applied =
    trace self_cst_reasonligo_tracer @@
    Self_cst.Reasonligo.all_expression raw in
  let* imperative =
    trace cit_reasonligo_tracer @@
    Tree_abstraction.Reasonligo.compile_expression applied
  in ok imperative

let parse_and_abstract_jsligo buffer file_path =
  let* raw =
    trace parser_tracer @@
    Parsing.Jsligo.parse_file buffer file_path in
  let* applied =
    trace self_cst_jsligo_tracer @@
    Self_cst.Jsligo.all_module raw in
  let* imperative =
    trace cit_jsligo_tracer @@
    Tree_abstraction.Jsligo.compile_module applied
  in ok imperative

let parse_and_abstract_expression_jsligo buffer =
  let* raw =
    trace parser_tracer @@
    Parsing.Jsligo.parse_expression buffer in
  let* applied =
    trace self_cst_jsligo_tracer @@
    Self_cst.Jsligo.all_expression raw in
  let* imperative =
    trace cit_jsligo_tracer @@
    Tree_abstraction.Jsligo.compile_expression applied
  in ok imperative

let parse_and_abstract ~meta buffer file_path
    : (Ast_imperative.module_, _) Trace.result =
  let* parse_and_abstract =
    match meta.syntax with
      PascaLIGO  -> ok parse_and_abstract_pascaligo
    | CameLIGO   -> ok parse_and_abstract_cameligo
    | ReasonLIGO -> ok parse_and_abstract_reasonligo
    | JsLIGO     -> ok parse_and_abstract_jsligo in
  let* abstracted =
    parse_and_abstract buffer file_path in
  let* applied =
    trace self_ast_imperative_tracer @@
    Self_ast_imperative.all_module abstracted in
  ok applied

let parse_and_abstract_expression ~meta buffer =
  let* parse_and_abstract =
    match meta.syntax with
      PascaLIGO ->
        ok parse_and_abstract_expression_pascaligo
    | CameLIGO ->
        ok parse_and_abstract_expression_cameligo
    | ReasonLIGO ->
        ok parse_and_abstract_expression_reasonligo
    | JsLIGO ->
        ok parse_and_abstract_expression_jsligo
      in
  let* abstracted =
    parse_and_abstract buffer in
  let* applied =
    trace self_ast_imperative_tracer @@
    Self_ast_imperative.all_expression abstracted
  in ok applied

let parse_and_abstract_string_reasonligo buffer =
  let* raw = trace parser_tracer @@
    Parsing.Reasonligo.parse_string buffer in
  let* imperative = trace cit_reasonligo_tracer @@
    Tree_abstraction.Reasonligo.compile_module raw
  in ok imperative

let parse_and_abstract_string_pascaligo buffer =
  let* raw =
    trace parser_tracer @@
    Parsing.Pascaligo.parse_string buffer in
  let* imperative =
    trace cit_pascaligo_tracer @@
    Tree_abstraction.Pascaligo.compile_module raw
  in ok imperative

let parse_and_abstract_string_cameligo buffer =
  let* raw =
    trace parser_tracer @@
    Parsing.Cameligo.parse_string buffer in
  let* imperative =
    trace cit_cameligo_tracer @@
    Tree_abstraction.Cameligo.compile_module raw
  in ok imperative

let parse_and_abstract_string_jsligo buffer =
  let* raw =
    trace parser_tracer @@
    Parsing.Jsligo.parse_string buffer in
  let* imperative =
    trace cit_jsligo_tracer @@
    Tree_abstraction.Jsligo.compile_module raw
  in ok imperative

let parse_and_abstract_string syntax buffer =
  let* parse_and_abstract =
    match syntax with
      PascaLIGO ->
        ok parse_and_abstract_string_pascaligo
    | CameLIGO ->
        ok parse_and_abstract_string_cameligo
    | ReasonLIGO ->
        ok parse_and_abstract_string_reasonligo
    | JsLIGO ->
      ok parse_and_abstract_string_jsligo in
  let* abstracted =
    parse_and_abstract buffer in
  let* applied =
    trace self_ast_imperative_tracer @@
    Self_ast_imperative.all_module abstracted
  in ok applied

let pretty_print_pascaligo_cst =
  Parsing.Pascaligo.pretty_print_cst

let pretty_print_cameligo_cst =
  Parsing.Cameligo.pretty_print_cst

let pretty_print_reasonligo_cst =
  Parsing.Reasonligo.pretty_print_cst

let pretty_print_jsligo_cst =
  Parsing.Jsligo.pretty_print_cst

let pretty_print_cst ~meta buffer file_path=
  let print =
    match meta.syntax with
      PascaLIGO  -> pretty_print_pascaligo_cst
    | CameLIGO   -> pretty_print_cameligo_cst
    | ReasonLIGO -> pretty_print_reasonligo_cst
    | JsLIGO     -> pretty_print_jsligo_cst
  in trace parser_tracer @@ print buffer file_path

let pretty_print_pascaligo =
  Parsing.Pascaligo.pretty_print_file

let pretty_print_cameligo =
  Parsing.Cameligo.pretty_print_file

let pretty_print_reasonligo =
  Parsing.Reasonligo.pretty_print_file

let pretty_print_jsligo =
  Parsing.Jsligo.pretty_print_file

let pretty_print ~meta buffer file_path =
  let print =
    match meta.syntax with
      PascaLIGO  -> pretty_print_pascaligo
    | CameLIGO   -> pretty_print_cameligo
    | ReasonLIGO -> pretty_print_reasonligo
    | JsLIGO     -> pretty_print_jsligo
  in trace parser_tracer @@ print buffer file_path
