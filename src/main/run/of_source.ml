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
      trace_strong (simple_error "entry-point doesn't identical type (storage) for second parameter and second result") @@
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

(* open Tezos_utils *)

let compile_file_contract_parameter : string -> string -> string -> Compile.Helpers.s_syntax -> Michelson.t result =
  fun source_filename _entry_point expression syntax ->
  let%bind (program , state) = Compile.Of_source.type_file syntax source_filename in
  let env = Ast_typed.program_environment program in
  let%bind syntax = Compile.Helpers.syntax_to_variant syntax (Some source_filename) in
  let%bind simplified = Compile.Helpers.parsify_expression syntax expression in
  Of_simplified.compile_expression simplified ~env ~state

let compile_file_expression : string -> string -> string -> Compile.Helpers.s_syntax -> Michelson.t result =
  fun source_filename _entry_point expression syntax ->
  let%bind (program , state) = Compile.Of_source.type_file syntax source_filename in
  let env = Ast_typed.program_environment program in
  let%bind syntax = Compile.Helpers.syntax_to_variant syntax (Some source_filename) in
  let%bind simplified = Compile.Helpers.parsify_expression syntax expression in
  Of_simplified.compile_expression simplified ~env ~state

let compile_expression : string -> Typer.Solver.state -> Compile.Helpers.s_syntax -> Michelson.t result =
  fun expression state syntax ->
  let%bind syntax = Compile.Helpers.syntax_to_variant syntax None in
  let%bind simplified = Compile.Helpers.parsify_expression syntax expression in
  Of_simplified.compile_expression ~state simplified

let compile_file_contract_storage ~value : string -> string -> string -> Compile.Helpers.s_syntax -> Michelson.t result =
  fun source_filename _entry_point expression syntax ->
  let%bind (program , state) = Compile.Of_source.type_file syntax source_filename in
  let env = Ast_typed.program_environment program in
  let%bind syntax = Compile.Helpers.syntax_to_variant syntax (Some source_filename) in
  let%bind simplified = Compile.Helpers.parsify_expression syntax expression in
  Of_simplified.compile_expression ~value simplified ~env ~state

let compile_file_contract_args =
  fun ?value source_filename _entry_point storage parameter syntax ->
  let%bind (program , state) = Compile.Of_source.type_file syntax source_filename in
  let env = Ast_typed.program_environment program in
  let%bind syntax = Compile.Helpers.syntax_to_variant syntax (Some source_filename) in
  let%bind storage_simplified = Compile.Helpers.parsify_expression syntax storage in
  let%bind parameter_simplified = Compile.Helpers.parsify_expression syntax parameter in
  let args = Ast_simplified.e_pair storage_simplified parameter_simplified in
  Of_simplified.compile_expression ?value args ~env ~state

type dry_run_options =
  { amount : string ;
    sender : string option ;
    source : string option }

let make_dry_run_options (opts : dry_run_options) : Of_michelson.options result =
  let open Proto_alpha_utils.Trace in
  let open Proto_alpha_utils.Memory_proto_alpha in
  let open Protocol.Alpha_context in
  let%bind amount = match Tez.of_string opts.amount with
    | None -> simple_fail "invalid amount"
    | Some amount -> ok amount in
  let%bind sender =
    match opts.sender with
    | None -> ok None
    | Some sender ->
      let%bind sender =
        trace_alpha_tzresult
          (simple_error "invalid address")
          (Contract.of_b58check sender) in
      ok (Some sender) in
  let%bind source =
    match opts.source with
    | None -> ok None
    | Some source ->
      let%bind source =
        trace_alpha_tzresult
          (simple_error "invalid source address")
          (Contract.of_b58check source) in
      ok (Some source) in
  ok @@ make_options ~amount ?source:sender ?payer:source ()

let run_contract ~options ?storage_value source_filename entry_point storage parameter syntax =
  let%bind (program , state) = Compile.Of_source.type_file syntax source_filename in
  let () = Typer.Solver.discard_state state in
  let%bind code = Compile.Of_typed.compile_function_entry program entry_point in
  let%bind args = compile_file_contract_args ?value:storage_value source_filename entry_point storage parameter syntax in
  let%bind options = make_dry_run_options options in
  let%bind ex_value_ty = Of_michelson.run ~options code args in
  Compile.Of_simplified.uncompile_typed_program_entry_function_result program entry_point ex_value_ty

let run_function_entry ~options source_filename entry_point input syntax =
  let%bind (program , state) = Compile.Of_source.type_file syntax source_filename in
  let () = Typer.Solver.discard_state state in
  let%bind code = Compile.Of_typed.compile_function_entry program entry_point in
  let%bind args = compile_file_expression source_filename entry_point input syntax in
  let%bind options = make_dry_run_options options in
  let%bind ex_value_ty = Of_michelson.run ~options code args in
  Compile.Of_simplified.uncompile_typed_program_entry_function_result program entry_point ex_value_ty

let evaluate_entry ~options source_filename entry_point syntax =
  let%bind (program , state) = Compile.Of_source.type_file syntax source_filename in
  let () = Typer.Solver.discard_state state in
  let%bind code = Compile.Of_typed.compile_expression_as_function_entry program entry_point in
  let%bind options = make_dry_run_options options in
  let%bind ex_value_ty = Of_michelson.evaluate ~options code in
  Compile.Of_simplified.uncompile_typed_program_entry_expression_result program entry_point ex_value_ty

let evaluate_michelson expression syntax =
  let%bind code = Compile.Of_source.compile_expression_as_function expression syntax in
  Of_michelson.evaluate_michelson code
