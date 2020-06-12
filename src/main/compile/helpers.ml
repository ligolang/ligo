open Trace
open Main_errors

type s_syntax = Syntax_name of string
type v_syntax = PascaLIGO | CameLIGO | ReasonLIGO

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


let parsify_pascaligo source =
  let%bind raw = trace parser_tracer @@
    Parser.Pascaligo.parse_file source in
  let%bind imperative = trace cit_pascaligo_tracer @@
    Concrete_to_imperative.Pascaligo.compile_program raw
  in ok imperative

let parsify_expression_pascaligo source =
  let%bind raw = trace parser_tracer @@
    Parser.Pascaligo.parse_expression source in
  let%bind imperative = trace cit_pascaligo_tracer @@
    Concrete_to_imperative.Pascaligo.compile_expression raw
  in ok imperative

let parsify_cameligo source =
  let%bind raw = trace parser_tracer @@
    Parser.Cameligo.parse_file source in
  let%bind imperative = trace cit_cameligo_tracer @@
    Concrete_to_imperative.Cameligo.compile_program raw
  in ok imperative

let parsify_expression_cameligo source =
  let%bind raw = trace parser_tracer @@
    Parser.Cameligo.parse_expression source in
  let%bind imperative = trace cit_cameligo_tracer @@
    Concrete_to_imperative.Cameligo.compile_expression raw
  in ok imperative

let parsify_reasonligo source =
  let%bind raw = trace parser_tracer @@
    Parser.Reasonligo.parse_file source in
  let%bind imperative = trace cit_cameligo_tracer @@
    Concrete_to_imperative.Cameligo.compile_program raw
  in ok imperative

let parsify_expression_reasonligo source =
  let%bind raw = trace parser_tracer @@
    Parser.Reasonligo.parse_expression source in
  let%bind imperative = trace cit_cameligo_tracer @@
    Concrete_to_imperative.Cameligo.compile_expression raw
  in ok imperative

let parsify syntax source : (Ast_imperative.program, _) Trace.result =
  let%bind parsify =
    match syntax with
      PascaLIGO  -> ok parsify_pascaligo
    | CameLIGO   -> ok parsify_cameligo
    | ReasonLIGO -> ok parsify_reasonligo in
  let%bind parsified = parsify source in
  let%bind applied = trace self_ast_imperative_tracer @@
    Self_ast_imperative.all_program parsified in
  ok applied

let parsify_expression syntax source =
  let%bind parsify = match syntax with
    PascaLIGO  -> ok parsify_expression_pascaligo
  | CameLIGO   -> ok parsify_expression_cameligo
  | ReasonLIGO -> ok parsify_expression_reasonligo in
  let%bind parsified = parsify source in
  let%bind applied = trace self_ast_imperative_tracer @@
    Self_ast_imperative.all_expression parsified
  in ok applied

let parsify_string_reasonligo source =
  let%bind raw = trace parser_tracer @@
    Parser.Reasonligo.parse_string source in
  let%bind imperative = trace cit_cameligo_tracer @@
    Concrete_to_imperative.Cameligo.compile_program raw
  in ok imperative

let parsify_string_pascaligo source =
  let%bind raw = trace parser_tracer @@
    Parser.Pascaligo.parse_string source in
  let%bind imperative = trace cit_pascaligo_tracer @@
    Concrete_to_imperative.Pascaligo.compile_program raw
  in ok imperative

let parsify_string_cameligo source =
  let%bind raw = trace parser_tracer @@
    Parser.Cameligo.parse_string source in
  let%bind imperative = trace cit_cameligo_tracer @@
    Concrete_to_imperative.Cameligo.compile_program raw
  in ok imperative

let parsify_string syntax source =
  let%bind parsify =
    match syntax with
      PascaLIGO  -> ok parsify_string_pascaligo
    | CameLIGO   -> ok parsify_string_cameligo
    | ReasonLIGO -> ok parsify_string_reasonligo in
  let%bind parsified = parsify source in
  let%bind applied = trace self_ast_imperative_tracer @@
    Self_ast_imperative.all_program parsified
  in ok applied

let pretty_print_pascaligo_cst source =
  let%bind ast = trace parser_tracer @@ Parser.Pascaligo.parse_file source in
  let buffer = Buffer.create 59 in
  let state =
    Parser_pascaligo.ParserLog.mk_state
      ~offsets:true
      ~mode:`Byte
      ~buffer in
  Parser_pascaligo.ParserLog.pp_cst state ast;
  ok buffer

let pretty_print_cameligo_cst source =
  let%bind ast = trace parser_tracer @@ Parser.Cameligo.parse_file source in
  let buffer = Buffer.create 59 in
  let state = (* TODO: Should flow from the CLI *)
    Parser_cameligo.ParserLog.mk_state
      ~offsets:true
      ~mode:`Point
      ~buffer in
  Parser_cameligo.ParserLog.pp_cst state ast;
  ok buffer

let pretty_print_reasonligo_cst source =
  let%bind ast = trace parser_tracer @@ Parser.Reasonligo.parse_file source in
  let buffer = Buffer.create 59 in
  let state = (* TODO: Should flow from the CLI *)
    Parser_cameligo.ParserLog.mk_state
      ~offsets:true
      ~mode:`Point
      ~buffer in
  Parser_cameligo.ParserLog.pp_cst state ast;
  ok buffer

let pretty_print_cst syntax source =
  let%bind v_syntax =
    syntax_to_variant syntax (Some source) in
  match v_syntax with
    PascaLIGO  -> pretty_print_pascaligo_cst  source
  | CameLIGO   -> pretty_print_cameligo_cst   source
  | ReasonLIGO -> pretty_print_reasonligo_cst source

let preprocess_pascaligo = Parser.Pascaligo.preprocess

let preprocess_cameligo = Parser.Cameligo.preprocess

let preprocess_reasonligo = Parser.Reasonligo.preprocess

let preprocess syntax source =
  let%bind v_syntax =
    syntax_to_variant syntax (Some source) in
  trace parser_tracer @@
  match v_syntax with
    PascaLIGO  -> preprocess_pascaligo  source
  | CameLIGO   -> preprocess_cameligo   source
  | ReasonLIGO -> preprocess_reasonligo source

let pretty_print_pascaligo source =
  let%bind ast = Parser.Pascaligo.parse_file source in
  let doc    = Parser_pascaligo.Pretty.print ast in
  let buffer = Buffer.create 131 in
  let width  =
    match Terminal_size.get_columns () with
      None -> 60
    | Some c -> c in
  let () = PPrint.ToBuffer.pretty 1.0 width buffer doc
  in Trace.ok buffer

let pretty_print_cameligo source =
  let%bind ast = Parser.Cameligo.parse_file source in
  let doc    = Parser_cameligo.Pretty.print ast in
  let buffer = Buffer.create 131 in
  let width  =
    match Terminal_size.get_columns () with
      None -> 60
    | Some c -> c in
  let () = PPrint.ToBuffer.pretty 1.0 width buffer doc
  in Trace.ok buffer

let pretty_print_reasonligo source =
  let%bind ast = Parser.Reasonligo.parse_file source in
  let doc    = Parser_reasonligo.Pretty.print ast in
  let buffer = Buffer.create 131 in
  let width  =
    match Terminal_size.get_columns () with
      None -> 60
    | Some c -> c in
  let () = PPrint.ToBuffer.pretty 1.0 width buffer doc
  in Trace.ok buffer

let pretty_print syntax source =
  let%bind v_syntax =
    syntax_to_variant syntax (Some source) in
  match v_syntax with
    PascaLIGO  -> trace parser_tracer @@ pretty_print_pascaligo  source
  | CameLIGO   -> trace parser_tracer @@ pretty_print_cameligo   source
  | ReasonLIGO -> trace parser_tracer @@ pretty_print_reasonligo source
