open Trace
open Main_errors

type s_syntax = Syntax_name of string
type v_syntax = PascaLIGO | CameLIGO | ReasonLIGO

let protocol_to_variant : string -> (Environment.Protocols.t, all) result = fun s ->
  trace_option (invalid_protocol_version Environment.Protocols.protocols_str s) @@ Environment.Protocols.protocols_to_variant s

let get_initial_env  : string -> (Ast_typed.environment, all) result = fun protocol_as_str ->
  let%bind protocol = protocol_to_variant protocol_as_str in
  ok @@ Environment.default protocol

let syntax_to_variant (Syntax_name syntax) source =
  match syntax, source with
    "auto", Some sf ->
      (match Filename.extension sf with
         ".ligo" | ".pligo" -> ok PascaLIGO
       | ".mligo"           -> ok CameLIGO
       | ".religo"          -> ok ReasonLIGO
       | ext                -> fail (syntax_auto_detection ext))
  | ("pascaligo" | "PascaLIGO"),   _ -> ok PascaLIGO
  | ("cameligo" | "CameLIGO"),     _ -> ok CameLIGO
  | ("reasonligo" | "ReasonLIGO"), _ -> ok ReasonLIGO
  | _ -> fail (invalid_syntax syntax)

let typer_switch_to_variant t =
  match t with
  | "old" -> ok Ast_typed.Old
  | "new" -> ok Ast_typed.New
  | _ -> fail (invalid_typer_switch t)

(* Preprocessing *)
let preprocess_pascaligo  = Preproc.Pascaligo.preprocess
let preprocess_cameligo   = Preproc.Cameligo.preprocess
let preprocess_reasonligo = Preproc.Reasonligo.preprocess

let preprocess_file ?(libs=[]) syntax source =
  trace preproc_tracer @@
  match syntax with
    PascaLIGO  -> preprocess_pascaligo  libs source
  | CameLIGO   -> preprocess_cameligo   libs source
  | ReasonLIGO -> preprocess_reasonligo libs source

let preprocess_pascaligo_string  = Preproc.Pascaligo.preprocess_string
let preprocess_cameligo_string   = Preproc.Cameligo.preprocess_string
let preprocess_reasonligo_string = Preproc.Reasonligo.preprocess_string

let preprocess_string ?(libs=[]) syntax source =
  trace preproc_tracer @@
  match syntax with
    PascaLIGO  -> preprocess_pascaligo_string  libs source
  | CameLIGO   -> preprocess_cameligo_string   libs source
  | ReasonLIGO -> preprocess_reasonligo_string libs source

(* Front-end compilation *)

let parse_and_abstract_pascaligo libs c_unit source =
  let%bind raw = trace parser_tracer @@
    Parser.Pascaligo.parse_file libs c_unit source in
  let%bind imperative = trace cit_pascaligo_tracer @@
    Tree_abstraction.Pascaligo.compile_program raw
  in ok imperative

let parse_and_abstract_expression_pascaligo libs c_unit =
  let%bind raw = trace parser_tracer @@
    Parser.Pascaligo.parse_expression libs c_unit in
  let%bind imperative = trace cit_pascaligo_tracer @@
    Tree_abstraction.Pascaligo.compile_expression raw
  in ok imperative

let parse_and_abstract_cameligo libs c_unit source =
  let%bind raw = trace parser_tracer @@
    Parser.Cameligo.parse_file libs c_unit source in
  let%bind imperative = trace cit_cameligo_tracer @@
    Tree_abstraction.Cameligo.compile_program raw
  in ok imperative

let parse_and_abstract_expression_cameligo libs source =
  let%bind raw = trace parser_tracer @@
    Parser.Cameligo.parse_expression libs source in
  let%bind imperative = trace cit_cameligo_tracer @@
    Tree_abstraction.Cameligo.compile_expression raw
  in ok imperative

let parse_and_abstract_reasonligo libs c_unit source =
  let%bind raw = trace parser_tracer @@
    Parser.Reasonligo.parse_file libs c_unit source in
  let%bind imperative = trace cit_reasonligo_tracer @@
    Tree_abstraction.Reasonligo.compile_program raw
  in ok imperative

let parse_and_abstract_expression_reasonligo libs source =
  let%bind raw = trace parser_tracer @@
    Parser.Reasonligo.parse_expression libs source in
  let%bind imperative = trace cit_reasonligo_tracer @@
    Tree_abstraction.Reasonligo.compile_expression raw
  in ok imperative

let parse_and_abstract ~libs syntax c_unit source : (Ast_imperative.program, _) Trace.result =
  let%bind parse_and_abstract =
    match syntax with
      PascaLIGO  -> ok parse_and_abstract_pascaligo
    | CameLIGO   -> ok parse_and_abstract_cameligo
    | ReasonLIGO -> ok parse_and_abstract_reasonligo in
  let%bind parsified = parse_and_abstract libs c_unit source in
  let%bind applied = trace self_ast_imperative_tracer @@
    Self_ast_imperative.all_program parsified in
  ok applied

let parse_and_abstract_expression ~libs syntax source =
  let%bind parse_and_abstract = match syntax with
    PascaLIGO  -> ok parse_and_abstract_expression_pascaligo
  | CameLIGO   -> ok parse_and_abstract_expression_cameligo
  | ReasonLIGO -> ok parse_and_abstract_expression_reasonligo in
  let%bind parsified = parse_and_abstract libs source in
  let%bind applied = trace self_ast_imperative_tracer @@
    Self_ast_imperative.all_expression parsified
  in ok applied

let parse_and_abstract_string_reasonligo libs source =
  let%bind raw = trace parser_tracer @@
    Parser.Reasonligo.parse_program_string libs source
  in
  let%bind imperative = trace cit_reasonligo_tracer @@
    Tree_abstraction.Reasonligo.compile_program raw
  in ok imperative

let parse_and_abstract_string_pascaligo libs source =
  let%bind raw = trace parser_tracer @@
    Parser.Pascaligo.parse_program_string libs source
  in
  let%bind imperative = trace cit_pascaligo_tracer @@
    Tree_abstraction.Pascaligo.compile_program raw
  in ok imperative

let parse_and_abstract_string_cameligo libs source =
  let%bind raw = trace parser_tracer @@
    Parser.Cameligo.parse_program_string libs source
  in
  let%bind imperative = trace cit_cameligo_tracer @@
    Tree_abstraction.Cameligo.compile_program raw
  in ok imperative

let parse_and_abstract_string ~libs syntax source =
  let%bind parse_and_abstract =
    match syntax with
      PascaLIGO  -> ok parse_and_abstract_string_pascaligo
    | CameLIGO   -> ok parse_and_abstract_string_cameligo
    | ReasonLIGO -> ok parse_and_abstract_string_reasonligo in
  let%bind parsified = parse_and_abstract libs source in
  let%bind applied = trace self_ast_imperative_tracer @@
    Self_ast_imperative.all_program parsified
  in ok applied

let pretty_print_pascaligo_cst libs c_unit source =
  let%bind ast = trace parser_tracer @@ Parser.Pascaligo.parse_file libs c_unit source in
  let buffer = Buffer.create 59 in
  let state =
    Cst_pascaligo.Printer.mk_state
      ~offsets:true
      ~mode:`Byte
      ~buffer in
  Cst_pascaligo.Printer.pp_cst state ast;
  ok buffer

let pretty_print_cameligo_cst libs c_unit source =
  let%bind ast = trace parser_tracer @@ Parser.Cameligo.parse_file libs c_unit source in
  let buffer = Buffer.create 59 in
  let state = (* TODO: Should flow from the CLI *)
    Cst_cameligo.Printer.mk_state
      ~offsets:true
      ~mode:`Point
      ~buffer in
  Cst_cameligo.Printer.pp_cst state ast;
  ok buffer

let pretty_print_reasonligo_cst libs c_unit source =
  let%bind ast = trace parser_tracer @@ Parser.Reasonligo.parse_file libs c_unit source in
  let buffer = Buffer.create 59 in
  let state = (* TODO: Should flow from the CLI *)
    Cst_reasonligo.Printer.mk_state
      ~offsets:true
      ~mode:`Point
      ~buffer in
  Cst_reasonligo.Printer.pp_cst state ast;
  ok buffer

let pretty_print_cst ?(libs=[]) syntax c_unit source =
  let%bind v_syntax =
    syntax_to_variant syntax (Some source) in
  match v_syntax with
    PascaLIGO  -> pretty_print_pascaligo_cst  libs c_unit source
  | CameLIGO   -> pretty_print_cameligo_cst   libs c_unit source
  | ReasonLIGO -> pretty_print_reasonligo_cst libs c_unit source

let pretty_print_pascaligo libs c_unit source =
  let%bind ast = Parser.Pascaligo.parse_file libs c_unit source in
  let doc    = Parser_pascaligo.Pretty.print ast in
  let buffer = Buffer.create 131 in
  let width  =
    match Terminal_size.get_columns () with
      None -> 60
    | Some c -> c in
  let () = PPrint.ToBuffer.pretty 1.0 width buffer doc
  in Trace.ok buffer

let pretty_print_cameligo libs c_unit source =
  let%bind ast = Parser.Cameligo.parse_file libs c_unit source in
  let doc    = Parser_cameligo.Pretty.print ast in
  let buffer = Buffer.create 131 in
  let width  =
    match Terminal_size.get_columns () with
      None -> 60
    | Some c -> c in
  let () = PPrint.ToBuffer.pretty 1.0 width buffer doc
  in Trace.ok buffer

let pretty_print_reasonligo libs c_unit source =
  let%bind ast = Parser.Reasonligo.parse_file libs c_unit source in
  let doc    = Parser_reasonligo.Pretty.print ast in
  let buffer = Buffer.create 131 in
  let width  =
    match Terminal_size.get_columns () with
      None -> 60
    | Some c -> c in
  let () = PPrint.ToBuffer.pretty 1.0 width buffer doc
  in Trace.ok buffer

let pretty_print ?(libs=[]) syntax c_unit source =
  let%bind v_syntax =
    syntax_to_variant syntax (Some source) in
  match v_syntax with
    PascaLIGO  -> trace parser_tracer @@ pretty_print_pascaligo  libs c_unit source
  | CameLIGO   -> trace parser_tracer @@ pretty_print_cameligo   libs c_unit source
  | ReasonLIGO -> trace parser_tracer @@ pretty_print_reasonligo libs c_unit source
