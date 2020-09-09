open Trace
open Main_errors

type s_syntax = Syntax_name of string
type s_dialect = Dialect_name of string
type v_syntax =
  | PascaLIGO of Tree_abstraction.Pascaligo.Decompiler.dialect option
  | CameLIGO | ReasonLIGO

let dialect_to_variant dialect =
  match dialect with
  | None -> ok None
  | Some (Dialect_name dialect) ->
     match dialect with
     | "terse" -> ok (Some Tree_abstraction.Pascaligo.Decompiler.Terse)
     | "verbose" -> ok (Some Tree_abstraction.Pascaligo.Decompiler.Verbose)
     | _ -> fail (`Main_invalid_dialect_name dialect)

let syntax_to_variant ?dialect (Syntax_name syntax) source =
  match syntax, source with
    "auto", Some sf ->
      (match Filename.extension sf with
         ".ligo" | ".pligo" ->
                    let%bind dialect = dialect_to_variant dialect in
                    ok (PascaLIGO dialect)
       | ".mligo"           -> ok CameLIGO
       | ".religo"          -> ok ReasonLIGO
       | ext                -> fail (syntax_auto_detection ext))
  | ("pascaligo" | "PascaLIGO"),   _ ->
     let%bind dialect = dialect_to_variant dialect in
     ok (PascaLIGO dialect)
  | ("cameligo" | "CameLIGO"),     _ -> ok CameLIGO
  | ("reasonligo" | "ReasonLIGO"), _ -> ok ReasonLIGO
  | _ -> fail (invalid_syntax syntax)

let specialise_and_print_pascaligo dialect program =
  let%bind cst = trace cit_pascaligo_tracer @@
    Tree_abstraction.Pascaligo.decompile_program ?dialect program in
  let%bind source = trace pretty_tracer @@
    Parser.Pascaligo.pretty_print cst
  in ok source

let specialise_and_print_expression_pascaligo dialect expression =
  let%bind cst = trace cit_pascaligo_tracer @@
    Tree_abstraction.Pascaligo.decompile_expression ?dialect expression in
  let%bind source = trace pretty_tracer @@
    Parser.Pascaligo.pretty_print_expression cst
  in ok source

let specialise_and_print_cameligo program =
  let%bind cst = trace cit_cameligo_tracer @@
    Tree_abstraction.Cameligo.decompile_program program in
  let%bind source = trace pretty_tracer @@
    Parser.Cameligo.pretty_print cst
  in ok source

let specialise_and_print_expression_cameligo expression =
  let%bind cst = trace cit_cameligo_tracer @@
    Tree_abstraction.Cameligo.decompile_expression expression in
  let%bind source = trace pretty_tracer @@
    Parser.Cameligo.pretty_print_expression cst
  in ok source

let specialise_and_print_reasonligo program =
  let%bind cst = trace cit_reasonligo_tracer @@
    Tree_abstraction.Reasonligo.decompile_program program in
  let%bind source = trace pretty_tracer @@
    Parser.Reasonligo.pretty_print cst
  in ok source

let specialise_and_print_expression_reasonligo expression =
  let%bind cst = trace cit_reasonligo_tracer @@
    Tree_abstraction.Reasonligo.decompile_expression expression in
  let%bind source = trace pretty_tracer @@
    Parser.Reasonligo.pretty_print_expression cst
  in ok source


let specialise_and_print syntax source : (Buffer.t, _) Trace.result =
  let specialise_and_print =
    match syntax with
      PascaLIGO dialect -> specialise_and_print_pascaligo dialect
    | CameLIGO   -> specialise_and_print_cameligo
    | ReasonLIGO -> specialise_and_print_reasonligo in
  specialise_and_print source

let specialise_and_print_expression syntax source =
  let specialise_and_print = match syntax with
    PascaLIGO dialect -> specialise_and_print_expression_pascaligo dialect
  | CameLIGO   -> specialise_and_print_expression_cameligo
  | ReasonLIGO -> specialise_and_print_expression_reasonligo in
  specialise_and_print source
