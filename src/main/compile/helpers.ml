open Trace

type s_syntax = Syntax_name of string
type v_syntax = Pascaligo | Cameligo

let syntax_to_variant : s_syntax -> string option -> v_syntax result =
  fun syntax source_filename ->
  let subr s n =
    String.sub s (String.length s - n) n in
  let endswith s suffix =
    let suffixlen = String.length suffix in
    (  String.length s >= suffixlen
       && String.equal (subr s suffixlen) suffix)
  in
  let (Syntax_name syntax) = syntax in
  match (syntax , source_filename) with
  | "auto" , Some sf when endswith sf ".ligo" -> ok Pascaligo
  | "auto" , Some sf when endswith sf ".mligo" -> ok Cameligo
  | "auto" , _ -> simple_fail "cannot auto-detect syntax, pleas use -s name_of_syntax"
  | "pascaligo" , _ -> ok Pascaligo
  | "cameligo" , _ -> ok Cameligo
  | _ -> simple_fail "unrecognized parser"

let parsify_pascaligo = fun source ->
  let%bind raw =
    trace (simple_error "parsing") @@
    Parser.Pascaligo.parse_file source in
  let%bind simplified =
    trace (simple_error "simplifying") @@
    Simplify.Pascaligo.simpl_program raw in
  ok simplified

let parsify_expression_pascaligo = fun source ->
  let%bind raw =
    trace (simple_error "parsing expression") @@
    Parser.Pascaligo.parse_expression source in
  let%bind simplified =
    trace (simple_error "simplifying expression") @@
    Simplify.Pascaligo.simpl_expression raw in
  ok simplified

let parsify_ligodity = fun source ->
  let%bind raw =
    trace (simple_error "parsing") @@
    Parser.Ligodity.parse_file source in
  let%bind simplified =
    trace (simple_error "simplifying") @@
    Simplify.Ligodity.simpl_program raw in
  ok simplified

let parsify_expression_ligodity = fun source ->
  let%bind raw =
    trace (simple_error "parsing expression") @@
    Parser.Ligodity.parse_expression source in
  let%bind simplified =
    trace (simple_error "simplifying expression") @@
    Simplify.Ligodity.simpl_expression raw in
  ok simplified

let parsify = fun (syntax : v_syntax) source_filename ->
  let%bind parsify = match syntax with
    | Pascaligo -> ok parsify_pascaligo
    | Cameligo -> ok parsify_ligodity
  in
  parsify source_filename

let parsify_expression = fun syntax source ->
  let%bind parsify = match syntax with
    | Pascaligo -> ok parsify_expression_pascaligo
    | Cameligo -> ok parsify_expression_ligodity
  in
  parsify source
