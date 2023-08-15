open Simple_utils.Trace
open Test_helpers
open Main_errors

let wrap_test_w ~no_colour name f =
  try_with
    (fun ~raise ~catch ->
      let () = f ~raise () in
      List.iter ~f:(fun w ->
          Format.printf "%a\n" (Main_warnings.pp ~display_format:Dev ~no_colour) w)
      @@ catch.warnings ())
    (fun ~catch error ->
      let value = Error (test_err_tracer name error) in
      let format = Display.bind_format test_format Formatter.error_format in
      let disp = Simple_utils.Display.Displayable { value; format } in
      let s = Simple_utils.Display.convert ~display_format:Dev ~no_colour disp in
      List.iter ~f:(fun w ->
          Format.printf "%a\n" (Main_warnings.pp ~display_format:Dev ~no_colour) w)
      @@ catch.warnings ();
      Format.printf "%s\n" s;
      Stdlib.raise Alcotest.Test_error)


let test_case name test =
  let options = Test_helpers.options in
  let no_colour = options.tools.no_colour in
  Test (Alcotest.test_case name `Quick @@ fun () -> wrap_test_w ~no_colour name test)


let comp_file_assert ~raise f test syntax expected () =
  let options =
    let options = Test_helpers.options in
    let options = Compiler_options.set_syntax options syntax in
    Compiler_options.set_test_flag options test
  in
  let (core : Ast_core.program) = Test_helpers.core_file_unqualified ~raise f options in
  let got = Format.asprintf "%a" Ast_core.PP.program core in
  if not (String.equal got expected) then Stdlib.raise Alcotest.Test_error


let comp_file_ ~raise f test syntax () =
  let options =
    let options = Test_helpers.options in
    let options = Compiler_options.set_syntax options syntax in
    Compiler_options.set_test_flag options test
  in
  let (_ : Ast_core.program) = Test_helpers.core_file ~raise f options in
  ()


let type_file_ ~raise f test syntax () =
  let options =
    let options = Test_helpers.options in
    let options = Compiler_options.set_syntax options syntax in
    Compiler_options.set_test_flag options test
  in
  let (_ : Ast_typed.program) = Test_helpers.type_file ~raise f options in
  ()


let agg_file_ ~raise f test syntax () =
  let options =
    let options = Test_helpers.options in
    let options = Compiler_options.set_syntax options syntax in
    Compiler_options.set_test_flag options test
  in
  let prg = Test_helpers.type_file ~raise f options in
  let (_ : Ast_aggregated.program) =
    trace ~raise aggregation_tracer
    @@ Aggregation.compile_program (Ast_typed.e_a_unit ~loc ()) prg
  in
  ()


let mini_c_file_ ~raise f test syntax () =
  let options =
    let options = Test_helpers.options in
    let options = Compiler_options.set_syntax options syntax in
    Compiler_options.set_test_flag options test
  in
  let prg = Test_helpers.type_file ~raise f options in
  let ctxt, exp =
    trace ~raise aggregation_tracer
    @@ Aggregation.compile_program (Ast_typed.e_a_unit ~loc ()) prg
  in
  let ctxt, exp =
    trace ~raise self_ast_aggregated_tracer
    @@ Self_ast_aggregated.all_program ~options:options.middle_end (ctxt, exp)
  in
  let x = Ast_aggregated.context_apply ctxt exp in
  let x =
    trace ~raise self_ast_aggregated_tracer
    @@ Self_ast_aggregated.all_expression ~options:options.middle_end x
  in
  let x = Expansion.compile x in
  let (_ : Mini_c.expression) =
    trace ~raise spilling_tracer @@ Spilling.compile_expression x
  in
  ()


let type_file f =
  let f = "./contracts/" ^ f in
  test_case f (type_file_ f false None)


let type_tfile f =
  let f = "./contracts/" ^ f in
  test_case f (type_file_ f true None)


let type_lfile f =
  let f = "./lexer/" ^ f in
  test_case f (type_file_ f false None)


let comp_file f =
  let f = "./contracts/" ^ f in
  test_case f (comp_file_ f false None)


let comp_file_assert f expected =
  let f = "./contracts/" ^ f in
  test_case f (comp_file_assert f false None expected)


let aggregate_file f =
  let f = "./contracts/" ^ f in
  test_case f (agg_file_ f false None)


let mini_c_file f =
  let f = "./contracts/" ^ f in
  test_case f (mini_c_file_ f false None)


let compile_file_ f =
  let f = "./contracts/" ^ f in
  test_case f (Test_helpers.compile_main f)


let typed_prod =
  Test_helpers.test_suite
    "Ast-typed productions"
    [ type_file "build/D.mligo"
    ; type_file "build/instance/main.mligo"
    ; type_file "infer_fun_application.mligo"
    ; type_file "protocol_dalphanet.mligo"
    ; type_file "sequence.mligo"
    ; type_file "remove_recursion.mligo"
    ; type_file "attributes.jsligo"
    ; type_file "tuple_decl_pos.mligo"
      (* Check that decl_pos is not taken into account when "inferring" about tuples (including long tuples) *)
    ; type_file "modules_and_free_vars/simple.mligo"
    ; type_file "modules_and_free_vars/nested_modules.mligo"
    ; type_file "modules_and_free_vars/module_with_free_vars.mligo"
    ; type_file "modules_and_free_vars/nested_modules_with_free_vars.mligo"
    ; type_tfile "pattern_match4.jsligo"
    ; type_lfile "add_semi.jsligo"
    ; type_file "type_shadowing.mligo"
    ; type_file "type_vars_let_fun.mligo"
    ; type_file "export_newline.jsligo"
    ; type_file "ppx_helpers.mligo"
    ; type_file "contextual_expressions_typing.mligo"
    ; type_file "contextual_expressions_typing.jsligo"
    ]


let core_prod =
  Test_helpers.test_suite
    "Ast-core productions"
    [ comp_file "vars_consts/multiple_vars.jsligo"
    ; comp_file "letin.mligo"
    ; comp_file "double_ignored_variable.jsligo"
    ; comp_file "clauseblock.jsligo"
    ; comp_file "polymorphism/annotate.mligo"
    ; comp_file "deep_pattern_matching/list_pattern.mligo"
    ; comp_file_assert
        "core_abstraction/fun_type_var.mligo"
        "\n\
         const foo : ∀ a : * . list (a) -> list (a) =\n\
        \  Λ a ->  Λ b ->  fun ( xs : list (b)) : list (b) -> xs"
    ]


let agg_prod =
  Test_helpers.test_suite
    "Ast-aggregated productions"
    [ aggregate_file "aggregation/bug_alias.mligo"
    ; aggregate_file "aggregation/bug_alias2.mligo"
    ; aggregate_file "aggregation/bug_alias3.mligo"
    ; aggregate_file "aggregation/bug_alias4.mligo"
    ; aggregate_file "aggregation/bug_alias5.mligo"
    ; aggregate_file "aggregation/bug_alias6.mligo"
    ; aggregate_file "aggregation/bug_alias7.mligo"
    ; aggregate_file "aggregation/bug_alias8.mligo"
    ; aggregate_file "aggregation/bug_alias9.mligo"
    ; aggregate_file "aggregation/bug_alias10.mligo"
    ; aggregate_file "aggregation/bug_alias11.mligo"
    ; aggregate_file "aggregation/bug_alias12.mligo"
    ; aggregate_file "aggregation/bug_alias13.mligo"
    ]


let mini_c_prod =
  Test_helpers.test_suite
    "Mini-c productions"
    [ mini_c_file "build/D.mligo"
    ; mini_c_file "modules_env.mligo"
      (* Module being defined does not type with its own type *)
    ; mini_c_file "aggregation/nested_modules.mligo"
    ]


let contract_prod =
  Test_helpers.test_suite
    "Contract productions"
    [ (*                 Toplevel patterns                 *)
      compile_file_ "top_level_patterns/contracts/cameligo/nested_record.mligo"
    ; compile_file_ "top_level_patterns/contracts/cameligo/nested_tuple.mligo"
    ; compile_file_ "top_level_patterns/contracts/cameligo/record_tuple.mligo"
    ; compile_file_ "top_level_patterns/contracts/cameligo/record.mligo"
    ; compile_file_ "top_level_patterns/contracts/cameligo/ticket_record.mligo"
    ; compile_file_ "top_level_patterns/contracts/cameligo/ticket_tuple.mligo"
    ; compile_file_ "top_level_patterns/contracts/cameligo/tuple_record.mligo"
    ; compile_file_ "top_level_patterns/contracts/cameligo/tuple.mligo"
    ; compile_file_
        "top_level_patterns/contracts/cameligo/constr_tuple_destructuring.mligo"
    ; compile_file_
        "top_level_patterns/contracts/cameligo/constr_record_destructuring.mligo"
      (* ; compile_file_ "top_level_patterns/contracts/jsligo/nested_record.jsligo" *)
    ; compile_file_ "top_level_patterns/contracts/jsligo/nested_tuple.jsligo"
      (* ; compile_file_ "top_level_patterns/contracts/jsligo/record_tuple.jsligo" *)
      (* ; compile_file_ "top_level_patterns/contracts/jsligo/record.jsligo" *)
      (* ; compile_file_ "top_level_patterns/contracts/jsligo/ticket_record.jsligo" *)
    ; compile_file_ "top_level_patterns/contracts/jsligo/ticket_tuple.jsligo"
      (* ; compile_file_ "top_level_patterns/contracts/jsligo/tuple_record.jsligo" *)
    ; compile_file_ "top_level_patterns/contracts/jsligo/tuple.jsligo"
    ]
