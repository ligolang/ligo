let specialise_and_print_pascaligo p =
  let ast = Self_ast_imperative.decompile_imperative p in
  let cst = Tree_abstraction.Pascaligo.decompile_declarations ast in
  let source = Parsing.Pascaligo.pretty_print Parsing.Pascaligo.CST.{decl=cst ; eof = Lexing_pascaligo.Token.ghost_eof}
  in source

let specialise_and_print_expression_pascaligo expression =
  let ast = Self_ast_imperative.decompile_imperative_expression expression in
  let cst = Tree_abstraction.Pascaligo.decompile_expression ast in
  let source =(Parsing.Pascaligo.pretty_print_expression cst)
  in source

let specialise_and_print_cameligo m =
  let cst = Tree_abstraction.Cameligo.decompile_program m in
  let source = (Parsing.Cameligo.pretty_print cst)
  in source

let specialise_and_print_expression_cameligo expression =
  let cst = Tree_abstraction.Cameligo.decompile_expression expression in
  let source = (Parsing.Cameligo.pretty_print_expression cst)
  in source

let specialise_and_print_reasonligo m =
  let cst = Tree_abstraction.Reasonligo.decompile_program m in
  let source = (Parsing.Reasonligo.pretty_print cst)
  in source

let specialise_and_print_expression_reasonligo expression =
  let cst =
    Tree_abstraction.Reasonligo.decompile_expression expression in
  let source =
    (Parsing.Reasonligo.pretty_print_expression cst)
  in source

let specialise_and_print_jsligo m =
  let ast =
    Self_ast_imperative.decompile_imperative m in
  let cst =
    Tree_abstraction.Jsligo.decompile_program ast in
  let source =
    (Parsing.Jsligo.pretty_print cst)
  in source

let specialise_and_print_expression_jsligo expression =
  let ast =
    Self_ast_imperative.decompile_imperative_expression expression in
  let cst =
    Tree_abstraction.Jsligo.decompile_expression ast in
  let b = Buffer.create 100 in
  List.fold ~f:(fun all x ->
    let source =
    (Parsing.Jsligo.pretty_print_expression x) in
    Buffer.add_buffer all source;
    b
  ) ~init:b cst


let specialise_and_print (syntax : Syntax_types.t) source : Buffer.t =
  let specialise_and_print =
    match syntax with
      PascaLIGO  -> specialise_and_print_pascaligo
    | CameLIGO   -> specialise_and_print_cameligo
    | ReasonLIGO -> specialise_and_print_reasonligo
    | JsLIGO     -> specialise_and_print_jsligo in
  specialise_and_print source

let specialise_and_print_expression (syntax : Syntax_types.t) source =
  let specialise_and_print = match syntax with
    PascaLIGO  -> specialise_and_print_expression_pascaligo
  | CameLIGO   -> specialise_and_print_expression_cameligo
  | ReasonLIGO -> specialise_and_print_expression_reasonligo
  | JsLIGO     -> specialise_and_print_expression_jsligo in
  specialise_and_print source
