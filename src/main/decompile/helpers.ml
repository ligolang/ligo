let specialise_and_print_cameligo m =
  let cst = Tree_abstraction.Cameligo.decompile_program m in
  let source =
    Parsing.Cameligo.pretty_print Parsing.Cameligo.Pretty.default_environment cst
  in
  source


let specialise_and_print_expression_cameligo expression =
  let cst = Tree_abstraction.Cameligo.decompile_expression expression in
  let source =
    Parsing.Cameligo.pretty_print_expression
      Parsing.Cameligo.Pretty.default_environment
      cst
  in
  source


let specialise_and_print_jsligo m =
  let ast = Self_ast_imperative.decompile_imperative m in
  let cst = Tree_abstraction.Jsligo.decompile_program ast in
  let source =
    Parsing.Jsligo.pretty_print Parsing.Jsligo.Pretty.default_environment cst
  in
  source


let specialise_and_print_expression_jsligo expression =
  let ast = Self_ast_imperative.decompile_imperative_expression expression in
  let cst = Tree_abstraction.Jsligo.decompile_expression ast in
  let b = Buffer.create 100 in
  List.fold
    ~f:(fun all x ->
      let source =
        Parsing.Jsligo.pretty_print_expression Parsing.Jsligo.Pretty.default_environment x
      in
      Buffer.add_buffer all source;
      b)
    ~init:b
    cst


let specialise_and_print_pascaligo m =
  let ast = Self_ast_imperative.decompile_imperative m in
  let cst = Tree_abstraction.Pascaligo.decompile_program ast in
  let source =
    Parsing.Pascaligo.pretty_print Parsing.Pascaligo.Pretty.default_environment cst
  in
  source


let specialise_and_print_expression_pascaligo expression =
  let cst = Tree_abstraction.Pascaligo.decompile_expression expression in
  let source =
    Parsing.Pascaligo.pretty_print_expression
      Parsing.Pascaligo.Pretty.default_environment
      cst
  in
  source


let specialise_and_print (syntax : Syntax_types.t) source : Buffer.t =
  let specialise_and_print =
    match syntax with
    | CameLIGO -> specialise_and_print_cameligo
    | JsLIGO -> specialise_and_print_jsligo
    | PascaLIGO -> specialise_and_print_pascaligo
  in
  specialise_and_print source


let specialise_and_print_expression (syntax : Syntax_types.t) source =
  let specialise_and_print =
    match syntax with
    | CameLIGO -> specialise_and_print_expression_cameligo
    | JsLIGO -> specialise_and_print_expression_jsligo
    | PascaLIGO -> specialise_and_print_expression_pascaligo
  in
  specialise_and_print source
