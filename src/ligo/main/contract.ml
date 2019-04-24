open Trace

include struct
  open Ast_simplified
  open Combinators

  let assert_entry_point_defined : program -> string -> unit result =
    fun program entry_point ->
      let aux : declaration -> bool = fun declaration ->
        match declaration with
        | Declaration_type _ -> false
        | Declaration_constant ne -> get_name ne = entry_point
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
    let (f, t) = functionalize e in
    let%bind main = translate_main f t in
    ok main
  in

  let input = Mini_c.Combinators.d_unit in
  let%bind r = Run_mini_c.run_entry f input in
  ok r

let compile_contract_file : string -> string -> string result = fun source entry_point ->
  let%bind raw =
    trace (simple_error "parsing") @@
    Parser.parse_file source in
  let%bind simplified =
    trace (simple_error "simplifying") @@
    Simplify.Pascaligo.simpl_program raw in
  let%bind () =
    assert_entry_point_defined simplified entry_point in
  let%bind typed =
    trace (simple_error "typing") @@
    Typer.type_program simplified in
  let%bind () =
    assert_valid_entry_point typed entry_point in
  let%bind mini_c =
    trace (simple_error "transpiling") @@
    Transpiler.translate_program typed in
  let%bind michelson =
    trace (simple_error "compiling") @@
    Compiler.translate_contract mini_c entry_point in
  let str =
    Format.asprintf "%a" Micheline.Michelson.pp_stripped michelson in
  ok str

let compile_contract_parameter : string -> string -> string -> string result = fun source entry_point expression ->
  let%bind (program , parameter_tv) =
    let%bind raw =
      trace (simple_error "parsing file") @@
      Parser.parse_file source in
    let%bind simplified =
      trace (simple_error "simplifying file") @@
      Simplify.Pascaligo.simpl_program raw in
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
    let%bind raw =
      trace (simple_error "parsing expression") @@
      Parser.parse_expression expression in
    let%bind simplified =
      trace (simple_error "simplifying expression") @@
      Simplify.Pascaligo.simpl_expression raw in
    let%bind typed =
      let env =
        let last_declaration = Location.unwrap List.(hd @@ rev program) in
        match last_declaration with
        | Declaration_constant (_ , env) -> env
      in
      trace (simple_error "typing expression") @@
      Typer.type_annotated_expression env simplified in
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
      Format.asprintf "%a" Micheline.Michelson.pp_stripped michelson in
    ok str
  in
  ok expr
