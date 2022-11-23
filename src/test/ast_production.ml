open Simple_utils.Trace
open Test_helpers
open Main_errors

let wrap_test_w name f =
  try_with
    (fun ~raise ~catch ->
      let () = f ~raise () in
      List.iter ~f:(fun w ->
          Format.printf "%a\n" (Main_warnings.pp ~display_format:Dev) w)
      @@ catch.warnings ())
    (fun ~catch error ->
      let value = Error (test_err_tracer name error) in
      let format = Display.bind_format test_format Formatter.error_format in
      let disp = Simple_utils.Display.Displayable { value; format } in
      let s = Simple_utils.Display.convert ~display_format:Dev disp in
      List.iter ~f:(fun w ->
          Format.printf "%a\n" (Main_warnings.pp ~display_format:Dev) w)
      @@ catch.warnings ();
      Format.printf "%s\n" s;
      Stdlib.raise Alcotest.Test_error)


let test_case name test =
  Test (Alcotest.test_case name `Quick @@ fun () -> wrap_test_w name test)


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
  let (_ : Ast_typed.program) = Test_helpers.type_file ~raise f Env options in
  ()


let agg_file_ ~raise f test syntax () =
  let options =
    let options = Test_helpers.options in
    let options = Compiler_options.set_syntax options syntax in
    Compiler_options.set_test_flag options test
  in
  let prg = Test_helpers.type_file ~raise f Env options in
  let (_ : Ast_aggregated.program) =
    trace ~raise aggregation_tracer
    @@ Aggregation.compile_program (Ast_typed.e_a_unit ()) prg
  in
  ()


let mini_c_file_ ~raise f test syntax () =
  let options =
    let options = Test_helpers.options in
    let options = Compiler_options.set_syntax options syntax in
    Compiler_options.set_test_flag options test
  in
  let prg = Test_helpers.type_file ~raise f Env options in
  let ctxt, exp =
    trace ~raise aggregation_tracer
    @@ Aggregation.compile_program (Ast_typed.e_a_unit ()) prg
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


let lex_file f =
  let f = "./lexer/" ^ f in
  test_case f (type_file_ f false None)


let comp_file f =
  let f = "./contracts/" ^ f in
  test_case f (comp_file_ f false None)


let aggregate_file f =
  let f = "./contracts/" ^ f in
  test_case f (agg_file_ f false None)


let mini_c_file f =
  let f = "./contracts/" ^ f in
  test_case f (mini_c_file_ f false None)


let typed_prod =
  Test_helpers.test_suite
    "Ast-typed productions"
    [ type_file "type-alias.ligo"
    ; type_file "build/D.mligo"
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
    ; type_file "deep_pattern_matching/pm_test.religo"
    ; type_tfile "pattern_match4.jsligo"
    ; type_file "layout.pligo"
    ; lex_file "add_semi.jsligo" (* not sure about this one *)
    ]


let core_prod =
  Test_helpers.test_suite
    "Ast-core productions"
    [ comp_file "vars_consts/shadowing.ligo"
    ; comp_file "vars_consts/func_const_var.ligo"
    ; comp_file "vars_consts/func_same_const_var.ligo"
    ; comp_file "vars_consts/func_var_const.ligo"
    ; comp_file "vars_consts/var_loop.ligo"
    ; comp_file "vars_consts/multiple_vars.ligo"
    ; comp_file "vars_consts/multiple_vars.jsligo"
    ; comp_file "letin.mligo"
    ; comp_file "letin.religo"
    ; comp_file "type_puning.ligo"
    ; comp_file "polymorphism/annotate.mligo"
    ; comp_file "deep_pattern_matching/list_pattern.ligo"
    ; comp_file "deep_pattern_matching/list_pattern.mligo"
    ; comp_file "deep_pattern_matching/list_pattern.religo"
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
