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

let transpile_value_literals 
    (e:Ast_typed.annotated_expression) : (Mini_c.value * _) result =
  let%bind (_ , ty) =
    let open Transpiler in
    let (f , _) = functionalize e in
    let%bind main = translate_main f e.location in
    ok main
  in
  let%bind lit = Run_typed.convert_to_literals e in
  ok (lit , snd ty)

let transpile_value
    (e:Ast_typed.annotated_expression) : (Mini_c.value * _) result =
  let%bind (f , ty) =
    let open Transpiler in
    let (f , _) = functionalize e in
    let%bind main = translate_main f e.location in
    ok main
  in

  let input = Mini_c.Combinators.d_unit in
  let%bind r = Run_mini_c.run_entry f ty input in
  ok (r , snd ty)

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

type s_syntax = Syntax_name of string
type v_syntax =  [`pascaligo | `cameligo ]

let syntax_to_variant : s_syntax -> string option -> v_syntax result =
  fun syntax source_filename ->
  let subr s n =
    String.sub s (String.length s - n) n in
  let endswith s suffix =
    let suffixlen = String.length suffix in
    (  String.length s >= suffixlen
       && String.equal (subr s suffixlen) suffix)
  in
  match syntax with
    Syntax_name syntax ->
    begin
      if String.equal syntax "auto" then
        begin
          match source_filename with
          | Some source_filename
            when endswith source_filename ".ligo"
            -> ok `pascaligo
          | Some source_filename
            when endswith source_filename ".mligo"
            -> ok `cameligo
          | _ -> simple_fail "cannot auto-detect syntax, pleas use -s name_of_syntax"
        end
      else if String.equal syntax "pascaligo" then ok `pascaligo
      else if String.equal syntax "cameligo" then ok `cameligo
      else simple_fail "unrecognized parser"
    end

let parsify = fun (syntax : v_syntax) source_filename ->
  let%bind parsify = match syntax with
    | `pascaligo -> ok parsify_pascaligo
    | `cameligo -> ok parsify_ligodity
  in
  parsify source_filename

let parsify_expression = fun syntax source ->
  let%bind parsify = match syntax with
    | `pascaligo -> ok parsify_expression_pascaligo
    | `cameligo -> ok parsify_expression_ligodity
  in
  parsify source

let compile_contract_file : string -> string -> s_syntax -> string result = fun source_filename entry_point syntax ->
  let%bind syntax = syntax_to_variant syntax (Some source_filename) in
  let%bind simplified = parsify syntax source_filename in
  let%bind () =
    assert_entry_point_defined simplified entry_point in
  let%bind typed =
    trace (simple_error "typing") @@
    Typer.type_program simplified in
  let%bind (mini_c , mini_c_ty) =
    trace (simple_error "transpiling") @@
    Transpiler.translate_entry typed entry_point in
  let%bind michelson =
    trace (simple_error "compiling") @@
    Compiler.translate_contract mini_c mini_c_ty in
  let str =
    Format.asprintf "%a" Michelson.pp_stripped michelson in
  ok str

let compile_contract_parameter : string -> string -> string -> s_syntax -> string result = fun source_filename entry_point expression syntax ->
  let%bind syntax = syntax_to_variant syntax (Some source_filename) in
  let%bind (program , parameter_tv) =
    let%bind simplified = parsify syntax source_filename in
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
    let%bind (mini_c , mini_c_ty) =
      trace (simple_error "transpiling expression") @@
      transpile_value typed in
    let%bind michelson =
      trace (simple_error "compiling expression") @@
      Compiler.translate_value mini_c mini_c_ty in
    let str =
      Format.asprintf "%a" Michelson.pp_stripped michelson in
    ok str
  in
  ok expr

(* Replace occurrences of E_map with E_big_map in the AST  *)
let rec transform_map_to_big_map (e: Ast_simplified.expression) : Ast_simplified.expression result = 
  let open Ast_simplified in
  match e.wrap_content with
  | E_tuple [fst;snd] ->
    let%bind tr_fst = transform_map_to_big_map fst in
    let new_tuple = Location.wrap (E_tuple [tr_fst;snd]) in
    ok @@ new_tuple
  | E_map lst ->
    let tr_map = Location.wrap (E_big_map lst) in
    ok @@ tr_map
  | _ -> fail @@ simple_error "can not replace map with big_map"

let compile_contract_storage ?(bigmap = false) source_filename entry_point expression syntax : string result =
  let%bind syntax = syntax_to_variant syntax (Some source_filename) in
  let%bind (program , storage_tv) =
    let%bind simplified = parsify syntax source_filename in
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
    let%bind simplified = if bigmap then transform_map_to_big_map simplified else ok @@ simplified in
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
    let%bind (mini_c , mini_c_ty) =
      trace (simple_error "transpiling expression") @@
      (if bigmap then transpile_value_literals typed else transpile_value typed) in
    let%bind michelson =
      trace (simple_error "compiling expression") @@
      Compiler.translate_value mini_c mini_c_ty in
    let str =
      Format.asprintf "%a" Michelson.pp_stripped michelson in
    ok str
  in
  ok expr

let type_file ?(debug_simplify = false) ?(debug_typed = false)
    syntax (source_filename:string) : Ast_typed.program result =
  let%bind simpl = parsify syntax source_filename in
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

let run_contract ?(bigmap = false) ?amount source_filename entry_point storage input syntax =
  let%bind syntax = syntax_to_variant syntax (Some source_filename) in
  let%bind typed =
    type_file syntax source_filename in
  let%bind storage_simpl =
    parsify_expression syntax storage in
  let%bind input_simpl =
    parsify_expression syntax input in
  let%bind input_simpl = if bigmap then transform_map_to_big_map input_simpl else ok @@ input_simpl in
  let options =
    let open Proto_alpha_utils.Memory_proto_alpha in
    let amount = Option.bind (fun amount -> Protocol.Alpha_context.Tez.of_string amount) amount in
    (make_options ?amount ()) in
  Run_simplified.run_simplityped ?input_to_value:(Some bigmap) ~options typed entry_point (Ast_simplified.e_pair storage_simpl input_simpl)

let run_function ?amount source_filename entry_point parameter syntax =
  let%bind syntax = syntax_to_variant syntax (Some source_filename) in
  let%bind typed =
    type_file syntax source_filename in
  let%bind parameter' =
    parsify_expression syntax parameter in
  let options =
    let open Proto_alpha_utils.Memory_proto_alpha in
    let amount = Option.bind (fun amount -> Protocol.Alpha_context.Tez.of_string amount) amount in
    (make_options ?amount ()) in
  Run_simplified.run_simplityped ~options typed entry_point parameter'

let evaluate_value ?amount source_filename entry_point syntax =
  let%bind syntax = syntax_to_variant syntax (Some source_filename) in
  let%bind typed =
    type_file syntax source_filename in
  let options =
    let open Proto_alpha_utils.Memory_proto_alpha in
    let amount = Option.bind (fun amount -> Protocol.Alpha_context.Tez.of_string amount) amount in
    (make_options ?amount ()) in
  Run_simplified.evaluate_simplityped ~options typed entry_point
