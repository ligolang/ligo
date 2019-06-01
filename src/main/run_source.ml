open Trace

include struct
  open Ast_simplified

  let assert_entry_point_defined : program -> string -> unit result =
    fun program entry_point ->
      let aux : declaration -> bool = fun declaration ->
        match declaration with
        | Declaration_type _ -> false
        | Declaration_constant (name , _ , _) -> name = entry_point
      in
      trace_strong (simple_error "no entry-point with given name") @@
      Assert.assert_true @@ List.exists aux @@ List.map Location.unwrap program
end

include struct
  open Ast_typed
  open Combinators

  let get_entry_point_type : type_value -> (type_value * type_value) result = fun t ->
    let%bind (arg , result) =
      trace_strong (simple_error "entry-point doesn't have a function type") @@
      get_t_function t in
    let%bind (arg' , storage_param) =
      trace_strong (simple_error "entry-point doesn't have 2 parameters") @@
      get_t_pair arg in
    let%bind (ops , storage_result) =
      trace_strong (simple_error "entry-point doesn't have 2 results") @@
      get_t_pair result in
    let%bind () =
      trace_strong (simple_error "entry-point doesn't have a list of operation as first result") @@
      assert_t_list_operation ops in
    let%bind () =
      trace_strong (simple_error "entry-point doesn't identitcal type (storage) for second parameter and second result") @@
      assert_type_value_eq (storage_param , storage_result) in
    ok (arg' , storage_param)

  let get_entry_point : program -> string -> (type_value * type_value) result = fun p e ->
    let%bind declaration = get_declaration_by_name p e in
    match declaration with
    | Declaration_constant (d , _) -> get_entry_point_type d.annotated_expression.type_annotation

  let assert_valid_entry_point = fun p e ->
    let%bind _ = get_entry_point p e in
    ok ()
end

let transpile_value
    (e:Ast_typed.annotated_expression) : Mini_c.value result =
  let%bind f =
    let open Transpiler in
    let (f , _) = functionalize e in
    let%bind main = translate_main f in
    ok main
  in

  let input = Mini_c.Combinators.d_unit in
  let%bind r = Run_mini_c.run_entry f input in
  ok r

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

let parsify = fun syntax source ->
  let%bind parsify = match syntax with
    | "pascaligo" -> ok parsify_pascaligo
    | "cameligo" -> ok parsify_ligodity
    | _ -> simple_fail "unrecognized parser"
  in
  parsify source

let parsify_expression = fun syntax source ->
  let%bind parsify = match syntax with
    | "pascaligo" -> ok parsify_expression_pascaligo
    | "cameligo" -> ok parsify_expression_ligodity
    | _ -> simple_fail "unrecognized parser"
  in
  parsify source

let compile_contract_file : string -> string -> string -> string result = fun source entry_point syntax ->
  let%bind simplified = parsify syntax source in
  let%bind () =
    assert_entry_point_defined simplified entry_point in
  let%bind typed =
    trace (simple_error "typing") @@
    Typer.type_program simplified in
  let%bind mini_c =
    trace (simple_error "transpiling") @@
    Transpiler.translate_entry typed entry_point in
  let%bind michelson =
    trace (simple_error "compiling") @@
    Compiler.translate_contract mini_c in
  let str =
    Format.asprintf "%a" Michelson.pp_stripped michelson in
  ok str

let compile_contract_parameter : string -> string -> string -> string -> string result = fun source entry_point expression syntax ->
  let%bind (program , parameter_tv) =
    let%bind simplified = parsify syntax source in
    let%bind () =
      assert_entry_point_defined simplified entry_point in
    let%bind typed =
      trace (simple_error "typing file") @@
      Typer.type_program simplified in
    let%bind (param_ty , _) =
      get_entry_point typed entry_point in
    ok (typed , param_ty)
  in
  let%bind expr =
    let%bind typed =
      let%bind simplified = parsify_expression syntax expression in
      let env =
        let last_declaration = Location.unwrap List.(hd @@ rev program) in
        match last_declaration with
        | Declaration_constant (_ , (_ , post_env)) -> post_env
      in
      trace (simple_error "typing expression") @@
      Typer.type_expression env simplified in
    let%bind () =
      trace (simple_error "expression type doesn't match type parameter") @@
      Ast_typed.assert_type_value_eq (parameter_tv , typed.type_annotation) in
    let%bind mini_c =
      trace (simple_error "transpiling expression") @@
      transpile_value typed in
    let%bind michelson =
      trace (simple_error "compiling expression") @@
      Compiler.translate_value mini_c in
    let str =
      Format.asprintf "%a" Michelson.pp_stripped michelson in
    ok str
  in
  ok expr


let compile_contract_storage : string -> string -> string -> string -> string result = fun source entry_point expression syntax ->
  let%bind (program , storage_tv) =
    let%bind simplified = parsify syntax source in
    let%bind () =
      assert_entry_point_defined simplified entry_point in
    let%bind typed =
      trace (simple_error "typing file") @@
      Typer.type_program simplified in
    let%bind (_ , storage_ty) =
      get_entry_point typed entry_point in
    ok (typed , storage_ty)
  in
  let%bind expr =
    let%bind simplified = parsify_expression syntax expression in
    let%bind typed =
      let env =
        let last_declaration = Location.unwrap List.(hd @@ rev program) in
        match last_declaration with
        | Declaration_constant (_ , (_ , post_env)) -> post_env
      in
      trace (simple_error "typing expression") @@
      Typer.type_expression env simplified in
    let%bind () =
      trace (simple_error "expression type doesn't match type storage") @@
      Ast_typed.assert_type_value_eq (storage_tv , typed.type_annotation) in
    let%bind mini_c =
      trace (simple_error "transpiling expression") @@
      transpile_value typed in
    let%bind michelson =
      trace (simple_error "compiling expression") @@
      Compiler.translate_value mini_c in
    let str =
      Format.asprintf "%a" Michelson.pp_stripped michelson in
    ok str
  in
  ok expr

let type_file ?(debug_simplify = false) ?(debug_typed = false)
    syntax (path:string) : Ast_typed.program result =
  let%bind simpl = parsify syntax path in
  (if debug_simplify then
     Format.(printf "Simplified : %a\n%!" Ast_simplified.PP.program simpl)
  ) ;
  let%bind typed =
    trace (simple_error "typing") @@
    Typer.type_program simpl in
  (if debug_typed then (
      Format.(printf "Typed : %a\n%!" Ast_typed.PP.program typed)
    )) ;
  ok typed
