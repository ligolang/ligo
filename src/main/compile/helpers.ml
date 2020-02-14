open Trace

type s_syntax = Syntax_name of string
type v_syntax = PascaLIGO | CameLIGO | ReasonLIGO

let syntax_to_variant (Syntax_name syntax) source =
  match syntax, source with
    "auto", Some sf ->
      (match Filename.extension sf with
         ".ligo" | ".pligo" -> ok PascaLIGO
       | ".mligo"           -> ok CameLIGO
       | ".religo"          -> ok ReasonLIGO
       | _ -> simple_fail "Cannot auto-detect the syntax.\n\
                          Hint: Use -s <name of syntax>\n")
  | ("pascaligo" | "PascaLIGO"),   _ -> ok PascaLIGO
  | ("cameligo" | "CameLIGO"),     _ -> ok CameLIGO
  | ("reasonligo" | "ReasonLIGO"), _ -> ok ReasonLIGO
  | _ -> simple_fail "Invalid syntax name.\n\
                     Hint: Use \"pascaligo\", \"cameligo\" \
                     or \"reasonligo\".\n"

let parsify_pascaligo source =
  let%bind raw =
    trace (simple_error "parsing") @@
    Parser.Pascaligo.parse_file source in
  let%bind simplified =
    trace (simple_error "simplifying") @@
      Simplify.Pascaligo.simpl_program raw
  in ok simplified

let parsify_expression_pascaligo source =
  let%bind raw =
    trace (simple_error "parsing expression") @@
    Parser.Pascaligo.parse_expression source in
  let%bind simplified =
    trace (simple_error "simplifying expression") @@
    Simplify.Pascaligo.simpl_expression raw
  in ok simplified

let parsify_cameligo source =
  let%bind raw =
    trace (simple_error "parsing") @@
    Parser.Cameligo.parse_file source in
  let%bind simplified =
    trace (simple_error "simplifying") @@
    Simplify.Cameligo.simpl_program raw
  in ok simplified

let parsify_expression_cameligo source =
  let%bind raw =
    trace (simple_error "parsing expression") @@
    Parser.Cameligo.parse_expression source in
  let%bind simplified =
    trace (simple_error "simplifying expression") @@
    Simplify.Cameligo.simpl_expression raw
  in ok simplified

let parsify_reasonligo source =
  let%bind raw =
    trace (simple_error "parsing") @@
    Parser.Reasonligo.parse_file source in
  let%bind simplified =
    trace (simple_error "simplifying") @@
    Simplify.Cameligo.simpl_program raw
  in ok simplified

let parsify_expression_reasonligo source =
  let%bind raw =
    trace (simple_error "parsing expression") @@
    Parser.Reasonligo.parse_expression source in
  let%bind simplified =
    trace (simple_error "simplifying expression") @@
    Simplify.Cameligo.simpl_expression raw
  in ok simplified

let parsify syntax source =
  let%bind parsify =
    match syntax with
      PascaLIGO  -> ok parsify_pascaligo
    | CameLIGO   -> ok parsify_cameligo
    | ReasonLIGO -> ok parsify_reasonligo in
  let%bind parsified = parsify source in
  let%bind applied = Self_ast_simplified.all_program parsified
  in ok applied

let parsify_expression syntax source =
  let%bind parsify = match syntax with
    PascaLIGO  -> ok parsify_expression_pascaligo
  | CameLIGO   -> ok parsify_expression_cameligo
  | ReasonLIGO -> ok parsify_expression_reasonligo in
  let%bind parsified = parsify source in
  let%bind applied = Self_ast_simplified.all_expression parsified
  in ok applied

let parsify_string_reasonligo source =
  let%bind raw =
    trace (simple_error "parsing") @@
    Parser.Reasonligo.parse_string source in
  let%bind simplified =
    trace (simple_error "simplifying") @@
    Simplify.Cameligo.simpl_program raw
  in ok simplified

let parsify_string_pascaligo source =
  let%bind raw =
    trace (simple_error "parsing") @@
    Parser.Pascaligo.parse_string source in
  let%bind simplified =
    trace (simple_error "simplifying") @@
    Simplify.Pascaligo.simpl_program raw
  in ok simplified

let parsify_string_cameligo source =
  let%bind raw =
    trace (simple_error "parsing") @@
    Parser.Cameligo.parse_string source in
  let%bind simplified =
    trace (simple_error "simplifying") @@
    Simplify.Cameligo.simpl_program raw
  in ok simplified

let parsify_string syntax source =
  let%bind parsify =
    match syntax with
      PascaLIGO  -> ok parsify_string_pascaligo
    | CameLIGO   -> ok parsify_string_cameligo
    | ReasonLIGO -> ok parsify_string_reasonligo in
  let%bind parsified = parsify source in
  let%bind applied = Self_ast_simplified.all_program parsified
  in ok applied

let pretty_print_pascaligo source =
  let%bind ast = Parser.Pascaligo.parse_file source in
  let buffer = Buffer.create 59 in
  let state =
    Parser_pascaligo.ParserLog.mk_state
      ~offsets:true
      ~mode:`Byte
      ~buffer in
  Parser_pascaligo.ParserLog.pp_ast state ast;
  ok buffer

let pretty_print_cameligo source =
  let%bind ast = Parser.Cameligo.parse_file source in
  let buffer = Buffer.create 59 in
  let state = (* TODO: Should flow from the CLI *)
    Parser_cameligo.ParserLog.mk_state
      ~offsets:true
      ~mode:`Point
      ~buffer in
  Parser.Cameligo.ParserLog.pp_ast state ast;
  ok buffer

let pretty_print_reasonligo source =
  let%bind ast = Parser.Reasonligo.parse_file source in
  let buffer = Buffer.create 59 in
  let state = (* TODO: Should flow from the CLI *)
    Parser.Reasonligo.ParserLog.mk_state
      ~offsets:true
      ~mode:`Point
      ~buffer in
  Parser.Reasonligo.ParserLog.pp_ast state ast;
  ok buffer

let pretty_print syntax source =
  let%bind v_syntax =
    syntax_to_variant syntax (Some source) in
  match v_syntax with
    PascaLIGO  -> pretty_print_pascaligo  source
  | CameLIGO   -> pretty_print_cameligo   source
  | ReasonLIGO -> pretty_print_reasonligo source
