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

let run_contract ?amount source_filename entry_point storage parameter syntax =
  let%bind program = Compile.Of_source.type_file syntax source_filename in
  let%bind code = Compile.Of_typed.compile_function_entry program entry_point in
  let%bind args = Compile.Of_source.compile_file_contract_args source_filename entry_point storage parameter syntax in
  let%bind ex_value_ty =
    let options =
      let open Proto_alpha_utils.Memory_proto_alpha in
      let amount = Option.bind (fun amount -> Protocol.Alpha_context.Tez.of_string amount) amount in
      (make_options ?amount ())
    in
    Of_michelson.run ~options code args
  in
  Compile.Of_simplified.uncompile_typed_program_entry_function_result program entry_point ex_value_ty

let run_function ?amount source_filename entry_point input syntax =
  let%bind program = Compile.Of_source.type_file syntax source_filename in
  let%bind code = Compile.Of_typed.compile_function_entry program entry_point in
  let%bind args = Compile.Of_source.compile_file_expression source_filename entry_point input syntax in
  let%bind ex_value_ty =
    let options =
      let open Proto_alpha_utils.Memory_proto_alpha in
      let amount = Option.bind (fun amount -> Protocol.Alpha_context.Tez.of_string amount) amount in
      (make_options ?amount ())
    in
    Of_michelson.run ~options code args
  in
  Compile.Of_simplified.uncompile_typed_program_entry_function_result program entry_point ex_value_ty

let evaluate ?amount source_filename entry_point syntax =
  let%bind program = Compile.Of_source.type_file syntax source_filename in
  let%bind code = Compile.Of_typed.compile_expression_as_function_entry program entry_point in
  let%bind input =
    let fake_input = Ast_simplified.e_unit () in
    Compile.Of_simplified.compile_expression fake_input
  in
  let%bind ex_value_ty =
    let options =
      let open Proto_alpha_utils.Memory_proto_alpha in
      let amount = Option.bind (fun amount -> Protocol.Alpha_context.Tez.of_string amount) amount in
      (make_options ?amount ())
    in
    Of_michelson.run ~options code input
  in
  Compile.Of_simplified.uncompile_typed_program_entry_expression_result program entry_point ex_value_ty
