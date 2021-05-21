open Trace
open Main_errors

type s_syntax = Syntax_name of string
type s_dialect = Dialect_name of string

type v_syntax =
  | PascaLIGO of Tree_abstraction.Pascaligo.Decompiler.dialect option
  | CameLIGO
  | ReasonLIGO
  | JsLIGO

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
                    let* dialect = dialect_to_variant dialect in
                    ok (PascaLIGO dialect)
       | ".mligo"           -> ok CameLIGO
       | ".religo"          -> ok ReasonLIGO
       | ".jsligo"          -> ok JsLIGO
       | ext                -> fail (syntax_auto_detection ext))
  | ("pascaligo" | "PascaLIGO"),   _ ->
     let* dialect = dialect_to_variant dialect in
     ok (PascaLIGO dialect)
  | ("cameligo" | "CameLIGO"),     _ -> ok CameLIGO
  | ("reasonligo" | "ReasonLIGO"), _ -> ok ReasonLIGO
  | ("jsligo" | "JsLIGO"),         _ -> ok JsLIGO
  | _ -> fail (invalid_syntax syntax)

let specialise_and_print_pascaligo dialect m =
  let* ast = trace self_ast_imperative_tracer @@
    Self_ast_imperative.decompile_imperative m in
  let* cst = trace cit_pascaligo_tracer @@
    Tree_abstraction.Pascaligo.decompile_module ?dialect ast in
  let* source = trace pretty_tracer @@
    ok (Parsing.Pascaligo.pretty_print cst)
  in ok source

let specialise_and_print_expression_pascaligo dialect expression =
  let* ast = trace self_ast_imperative_tracer @@
    Self_ast_imperative.decompile_imperative_expression expression in
  let* cst = trace cit_pascaligo_tracer @@
    Tree_abstraction.Pascaligo.decompile_expression ?dialect ast in
  let* source = trace pretty_tracer @@
    ok (Parsing.Pascaligo.pretty_print_expression cst)
  in ok source

let specialise_and_print_cameligo m =
  let* cst = trace cit_cameligo_tracer @@
    Tree_abstraction.Cameligo.decompile_module m in
  let* source = trace pretty_tracer @@
    ok (Parsing.Cameligo.pretty_print cst)
  in ok source

let specialise_and_print_expression_cameligo expression =
  let* cst = trace cit_cameligo_tracer @@
    Tree_abstraction.Cameligo.decompile_expression expression in
  let* source = trace pretty_tracer @@
    ok (Parsing.Cameligo.pretty_print_expression cst)
  in ok source

let specialise_and_print_reasonligo m =
  let* cst = trace cit_reasonligo_tracer @@
    Tree_abstraction.Reasonligo.decompile_module m in
  let* source = trace pretty_tracer @@
    ok (Parsing.Reasonligo.pretty_print cst)
  in ok source

let specialise_and_print_expression_reasonligo expression =
  let* cst = trace cit_reasonligo_tracer @@
    Tree_abstraction.Reasonligo.decompile_expression expression in
  let* source = trace pretty_tracer @@
    ok (Parsing.Reasonligo.pretty_print_expression cst)
  in ok source

let specialise_and_print_jsligo m =
  let* ast = trace self_ast_imperative_tracer @@
    Self_ast_imperative.decompile_imperative m in
  let* cst = trace cit_jsligo_tracer @@
    Tree_abstraction.Jsligo.decompile_module ast in
  let* source = trace pretty_tracer @@
    ok (Parsing.Jsligo.pretty_print cst)
  in ok source

let specialise_and_print_expression_jsligo expression =
  let* ast = trace self_ast_imperative_tracer @@
    Self_ast_imperative.decompile_imperative_expression expression in
  let* cst = trace cit_jsligo_tracer @@
    Tree_abstraction.Jsligo.decompile_expression ast in
  let b = Buffer.create 100 in
  bind_fold_list (fun all x -> 
    let* source = trace pretty_tracer @@
    ok (Parsing.Jsligo.pretty_print_expression x) in
    Buffer.add_buffer all source; 
    ok @@ b
  ) b cst


let specialise_and_print syntax source : (Buffer.t, _) Trace.result =
  let specialise_and_print =
    match syntax with
      PascaLIGO dialect -> specialise_and_print_pascaligo dialect
    | CameLIGO   -> specialise_and_print_cameligo
    | ReasonLIGO -> specialise_and_print_reasonligo
    | JsLIGO     -> specialise_and_print_jsligo in
  specialise_and_print source

let specialise_and_print_expression syntax source =
  let specialise_and_print = match syntax with
    PascaLIGO dialect -> specialise_and_print_expression_pascaligo dialect
  | CameLIGO   -> specialise_and_print_expression_cameligo
  | ReasonLIGO -> specialise_and_print_expression_reasonligo
  | JsLIGO     -> specialise_and_print_expression_jsligo in
  specialise_and_print source
