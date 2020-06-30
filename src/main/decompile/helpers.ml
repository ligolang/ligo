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

let specialise_and_print_pascaligo program =
  let%bind cst = trace cit_pascaligo_tracer @@
    Tree_abstraction.Pascaligo.decompile_program program in
  let%bind source = trace pretty_tracer @@
    Parser.Pascaligo.pretty_print cst
  in ok source

let specialise_and_print_expression_pascaligo expression =
  let%bind cst = trace cit_pascaligo_tracer @@
    Tree_abstraction.Pascaligo.decompile_expression expression in
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
  let%bind cst = trace cit_cameligo_tracer @@
    Tree_abstraction.Cameligo.decompile_program program in
  let%bind source = trace pretty_tracer @@
    Parser.Reasonligo.pretty_print cst
  in ok source

let specialise_and_print_expression_reasonligo expression =
  let%bind cst = trace cit_cameligo_tracer @@
    Tree_abstraction.Cameligo.decompile_expression expression in
  let%bind source = trace pretty_tracer @@
    Parser.Reasonligo.pretty_print_expression cst
  in ok source


let specialise_and_print syntax source : (Buffer.t, _) Trace.result =
  let%bind specialise_and_print =
    match syntax with
      PascaLIGO  -> ok specialise_and_print_pascaligo
    | CameLIGO   -> ok specialise_and_print_cameligo
    | ReasonLIGO -> ok specialise_and_print_reasonligo in
  let%bind source = specialise_and_print source in
  ok source

let specialise_and_print_expression syntax source =
  let%bind specialise_and_print = match syntax with
    PascaLIGO  -> ok specialise_and_print_expression_pascaligo
  | CameLIGO   -> ok specialise_and_print_expression_cameligo
  | ReasonLIGO -> ok specialise_and_print_expression_reasonligo in
  let%bind source = specialise_and_print source in
  ok source
