open Test_helpers
open Prometheus
open Prometheus_format
open Lwt.Infix

exception Should_exit_good

let run_ligo args =
  Ligo_prim.Value_var.reset_counter ();
  Ligo_prim.Type_var.reset_counter ();
  Ligo_prim.Module_var.reset_counter ();
  Cli.reset_return ();
  let argv = "ligo" :: args in
  let result = Cli.run ~argv () in
  let exit_code = result in
  Format.eprintf "%i\n%!" exit_code;
  if exit_code <> 0 then raise Should_exit_good else ()


let get_registry_content registry =
  let registry_output_promise =
    CollectorRegistry.collect registry
    >>= fun collected -> Lwt.return @@ Fmt.to_to_string TextFormat_0_0_4.output collected
  in
  (* remove \n to permit .* to match *)
  Str.global_replace (Str.regexp "\n") " " (Lwt_main.run registry_output_promise)


let check_analytics expected_registry_content_regex expected_agg_registry_content_regex =
  Format.eprintf "%s\n%!" (get_registry_content Analytics.registry.collectorRegistry);
  let registry_analytics = get_registry_content Analytics.registry.collectorRegistry in
  Alcotest.(check bool)
    "Analytics matches expected regex"
    true
    (Str.string_match expected_registry_content_regex registry_analytics 0);
  Format.eprintf "%s\n%!" (get_registry_content Analytics.agg_registry.collectorRegistry);
  let agg_registry_analytics =
    get_registry_content Analytics.agg_registry.collectorRegistry
  in
  Alcotest.(check bool)
    "Analytics matches expected regex"
    true
    (Str.string_match expected_agg_registry_content_regex agg_registry_analytics 0);
  ()


let test_compile_contract ~raise:_ () =
  let expected_registry_output_regex =
    Str.regexp
      ".*ligo_compile_compilation_size{repository=\"[a-z0-9-]+\", version=\"\", \
       project=\"[0-9]+\", syntax=\"jsligo\", protocol=\"[a-z]+\"} [0-9.]+.*"
  in
  let expected_agg_registry_content_regex =
    Str.regexp
      ".*ligo_cli_command{user=\"[a-z0-9-]+\", repository=\"[a-z0-9-]+\", version=\"\", \
       command=\"compile_contract\"} \
       1.000000.*ligo_cli_execution_by_syntax_and_protocol{user=\"[a-z0-9-]+\", \
       repository=\"[a-z0-9-]+\", version=\"\", command=\"compile_contract\", \
       syntax=\"JsLIGO\", protocol=\"[a-z]+\"} 1.000000.*"
  in
  let _ = run_ligo [ "compile"; "contract"; "contracts/analytics_test.jsligo" ] in
  check_analytics expected_registry_output_regex expected_agg_registry_content_regex


let test_compile_expression ~raise:_ () =
  let expected_registry_output_regex = Str.regexp ".*" in
  let expected_agg_registry_content_regex =
    Str.regexp
      ".*ligo_cli_command{user=\"[a-z0-9-]+\", repository=\"[a-z0-9-]+\", version=\"\", \
       command=\"compile_expression\"} \
       1.*ligo_cli_execution_by_syntax_and_protocol{user=\"[a-z0-9-]+\", \
       repository=\"[a-z0-9-]+\", version=\"\", command=\"compile_expression\", \
       syntax=\"CameLIGO\", protocol=\"[a-z]+\"} 1.*"
  in
  let _ = run_ligo [ "compile"; "expression"; "cameligo"; "4n land 4n" ] in
  check_analytics expected_registry_output_regex expected_agg_registry_content_regex


let test_compile_parameter ~raise:_ () =
  let expected_registry_output_regex = Str.regexp ".*" in
  let expected_agg_registry_content_regex =
    Str.regexp
      ".*ligo_cli_command{user=\"[a-z0-9-]+\", repository=\"[a-z0-9-]+\", version=\"\", \
       command=\"compile_parameter\"} \
       1.*ligo_cli_execution_by_syntax_and_protocol{user=\"[a-z0-9-]+\", \
       repository=\"[a-z0-9-]+\", version=\"\", command=\"compile_parameter\", \
       syntax=\"CameLIGO\", protocol=\"[a-z]+\"} 1.*"
  in
  let _ =
    run_ligo
      [ "compile"; "parameter"; "contracts/module_contract_simple.mligo"; "Add 999" ]
  in
  check_analytics expected_registry_output_regex expected_agg_registry_content_regex


let test_compile_storage ~raise:_ () =
  let expected_registry_output_regex = Str.regexp ".*" in
  let expected_agg_registry_content_regex =
    Str.regexp
      ".*ligo_cli_command{user=\"[a-z0-9-]+\", repository=\"[a-z0-9-]+\", version=\"\", \
       command=\"compile_storage\"} \
       1.*ligo_cli_execution_by_syntax_and_protocol{user=\"[a-z0-9-]+\", \
       repository=\"[a-z0-9-]+\", version=\"\", command=\"compile_storage\", \
       syntax=\"CameLIGO\", protocol=\"[a-z]+\"} 1.*"
  in
  let _ = run_ligo [ "compile"; "storage"; "contracts/self_annotations.mligo"; "()" ] in
  check_analytics expected_registry_output_regex expected_agg_registry_content_regex


let test_run_dry_run ~raise:_ () =
  let expected_registry_output_regex = Str.regexp ".*" in
  let expected_agg_registry_content_regex =
    Str.regexp
      ".*ligo_cli_command{user=\"[a-z0-9-]+\", repository=\"[a-z0-9-]+\", version=\"\", \
       command=\"run_dry-run\"} \
       1.*ligo_cli_execution_by_syntax_and_protocol{user=\"[a-z0-9-]+\", \
       repository=\"[a-z0-9-]+\", version=\"\", command=\"run_dry-run\", \
       syntax=\"CameLIGO\", protocol=\"[a-z]+\"} 1.*"
  in
  let _ =
    run_ligo
      [ "run"; "dry-run"; "contracts/super-counter.mligo"; "test_param"; "test_storage" ]
  in
  check_analytics expected_registry_output_regex expected_agg_registry_content_regex


let test_run_evaluate_call ~raise:_ () =
  let expected_registry_output_regex = Str.regexp ".*" in
  let expected_agg_registry_content_regex =
    Str.regexp
      ".*ligo_cli_command{user=\"[a-z0-9-]+\", repository=\"[a-z0-9-]+\", version=\"\", \
       command=\"run_evaluate-call\"} \
       1.*ligo_cli_execution_by_syntax_and_protocol{user=\"[a-z0-9-]+\", \
       repository=\"[a-z0-9-]+\", version=\"\", command=\"run_evaluate-call\", \
       syntax=\"CameLIGO\", protocol=\"[a-z]+\"} 1.*"
  in
  let _ =
    run_ligo
      [ "run"; "evaluate-call"; "contracts/assert.mligo"; "with_error"; "(false, ())" ]
  in
  check_analytics expected_registry_output_regex expected_agg_registry_content_regex


(* fail  An internal error ocurred. Please, contact the developers.
hd. *)
(* let test_run_evaluate_expr ~raise:_ () =
  let expected_registry_output_regex = Str.regexp ".*" in
  let expected_agg_registry_content_regex =
    Str.regexp
      ".*ligo_cli_command{user=\"[a-z0-9-]+\", repository=\"[a-z0-9-]+\", version=\"\", command=\"run_evaluate-expr\"} \
       1.*ligo_cli_execution_by_syntax_and_protocol{user=\"[a-z0-9-]+\", \
       repository=\"[a-z0-9-]+\", version=\"\", command=\"run_evaluate-expr\", syntax=\"CameLIGO\", \
       protocol=\"[a-z]+\"} 1.*"
  in
  let _ = run_ligo [ "run"; "evaluate-expr"; "contracts/super-counter.mligo" ] in
  check_analytics expected_registry_output_regex expected_agg_registry_content_regex *)

let test_run_interpret ~raise:_ () =
  let expected_registry_output_regex = Str.regexp ".*" in
  let expected_agg_registry_content_regex =
    Str.regexp
      ".*ligo_cli_command{user=\"[a-z0-9-]+\", repository=\"[a-z0-9-]+\", version=\"\", \
       command=\"run_interpret\"} \
       1.*ligo_cli_execution_by_syntax_and_protocol{user=\"[a-z0-9-]+\", \
       repository=\"[a-z0-9-]+\", version=\"\", command=\"run_interpret\", \
       syntax=\"CameLIGO\", protocol=\"[a-z]+\"} 1.*"
  in
  let _ =
    run_ligo
      [ "run"
      ; "interpret"
      ; "t1 (Cons(1,2),Nil)"
      ; "--init-file"
      ; "contracts/deep_pattern_matching/pm_test.mligo"
      ]
  in
  check_analytics expected_registry_output_regex expected_agg_registry_content_regex


let test_run_test ~raise:_ () =
  let expected_registry_output_regex = Str.regexp ".*" in
  let expected_agg_registry_content_regex =
    Str.regexp
      ".*ligo_cli_command{user=\"[a-z0-9-]+\", repository=\"[a-z0-9-]+\", version=\"\", \
       command=\"run_test\"} \
       1.*ligo_cli_execution_by_syntax_and_protocol{user=\"[a-z0-9-]+\", \
       repository=\"[a-z0-9-]+\", version=\"\", command=\"run_test\", \
       syntax=\"CameLIGO\", protocol=\"[a-z]+\"} 1.*"
  in
  let _ = run_ligo [ "run"; "test"; "contracts/build/C1.mligo" ] in
  check_analytics expected_registry_output_regex expected_agg_registry_content_regex


let test_run_test_expr ~raise:_ () =
  let expected_registry_output_regex = Str.regexp ".*" in
  let expected_agg_registry_content_regex =
    Str.regexp
      ".*ligo_cli_command{user=\"[a-z0-9-]+\", repository=\"[a-z0-9-]+\", version=\"\", \
       command=\"run_test-expr\"} \
       1.*ligo_cli_execution_by_syntax_and_protocol{user=\"[a-z0-9-]+\", \
       repository=\"[a-z0-9-]+\", version=\"\", command=\"run_test-expr\", \
       syntax=\"CameLIGO\", protocol=\"[a-z]+\"} 1.*"
  in
  let _ = run_ligo [ "run"; "test-expr"; "cameligo"; "()" ] in
  check_analytics expected_registry_output_regex expected_agg_registry_content_regex


let test_info_get_scope ~raise:_ () =
  let expected_registry_output_regex = Str.regexp ".*" in
  let expected_agg_registry_content_regex =
    Str.regexp
      ".*ligo_cli_command{user=\"[a-z0-9-]+\", repository=\"[a-z0-9-]+\", version=\"\", \
       command=\"info_get-scope\"} \
       1.*ligo_cli_execution_by_syntax_and_protocol{user=\"[a-z0-9-]+\", \
       repository=\"[a-z0-9-]+\", version=\"\", command=\"info_get-scope\", \
       syntax=\"CameLIGO\", protocol=\"[a-z]+\"} 1.*"
  in
  let _ =
    run_ligo
      [ "info"
      ; "get-scope"
      ; "contracts/get_scope_tests/constant.mligo"
      ; "--format"
      ; "dev"
      ]
  in
  check_analytics expected_registry_output_regex expected_agg_registry_content_regex


let test_info_list_declaration ~raise:_ () =
  let expected_registry_output_regex = Str.regexp ".*" in
  let expected_agg_registry_content_regex =
    Str.regexp
      ".*ligo_cli_command{user=\"[a-z0-9-]+\", repository=\"[a-z0-9-]+\", version=\"\", \
       command=\"info_list-declarations\"} \
       1.*ligo_cli_execution_by_syntax_and_protocol{user=\"[a-z0-9-]+\", \
       repository=\"[a-z0-9-]+\", version=\"\", command=\"info_list-declarations\", \
       syntax=\"JsLIGO\", protocol=\"[a-z]+\"} 1.*"
  in
  let _ = run_ligo [ "info"; "list-declarations"; "contracts/list_declaration.jsligo" ] in
  check_analytics expected_registry_output_regex expected_agg_registry_content_regex


let test_info_measure_contract ~raise:_ () =
  let expected_registry_output_regex = Str.regexp ".*" in
  let expected_agg_registry_content_regex =
    Str.regexp
      ".*ligo_cli_command{user=\"[a-z0-9-]+\", repository=\"[a-z0-9-]+\", version=\"\", \
       command=\"info_measure-contract\"} \
       1.*ligo_cli_execution_by_syntax_and_protocol{user=\"[a-z0-9-]+\", \
       repository=\"[a-z0-9-]+\", version=\"\", command=\"info_measure-contract\", \
       syntax=\"JsLIGO\", protocol=\"[a-z]+\"} 1.*"
  in
  let _ = run_ligo [ "info"; "measure-contract"; "contracts/list_declaration.jsligo" ] in
  check_analytics expected_registry_output_regex expected_agg_registry_content_regex


let test_changelog ~raise:_ () =
  let expected_registry_output_regex = Str.regexp ".*" in
  let expected_agg_registry_content_regex =
    Str.regexp
      ".*ligo_cli_command{user=\"[a-z0-9-]+\", repository=\"[a-z0-9-]+\", version=\"\", \
       command=\"changelog\"} 1.*"
  in
  let _ = run_ligo [ "changelog" ] in
  check_analytics expected_registry_output_regex expected_agg_registry_content_regex


let test_print_pretty ~raise:_ () =
  let expected_registry_output_regex = Str.regexp ".*" in
  let expected_agg_registry_content_regex =
    Str.regexp
      ".*ligo_cli_command{user=\"[a-z0-9-]+\", repository=\"[a-z0-9-]+\", version=\"\", \
       command=\"print_pretty\"} \
       1.*ligo_cli_execution_by_syntax_and_protocol{user=\"[a-z0-9-]+\", \
       repository=\"[a-z0-9-]+\", version=\"\", command=\"print_pretty\", \
       syntax=\"CameLIGO\", protocol=\"[a-z]+\"} 1.*"
  in
  let _ = run_ligo [ "print"; "pretty"; "contracts/build/cycle_A.mligo" ] in
  check_analytics expected_registry_output_regex expected_agg_registry_content_regex


let test_print_preprocessed ~raise:_ () =
  let expected_registry_output_regex = Str.regexp ".*" in
  let expected_agg_registry_content_regex =
    Str.regexp
      ".*ligo_cli_command{user=\"[a-z0-9-]+\", repository=\"[a-z0-9-]+\", version=\"\", \
       command=\"print_preprocessed\"} \
       1.*ligo_cli_execution_by_syntax_and_protocol{user=\"[a-z0-9-]+\", \
       repository=\"[a-z0-9-]+\", version=\"\", command=\"print_preprocessed\", \
       syntax=\"CameLIGO\", protocol=\"[a-z]+\"} 1.*"
  in
  let _ = run_ligo [ "print"; "preprocessed"; "contracts/build/cycle_A.mligo" ] in
  check_analytics expected_registry_output_regex expected_agg_registry_content_regex


let test_print_cst ~raise:_ () =
  let expected_registry_output_regex = Str.regexp ".*" in
  let expected_agg_registry_content_regex =
    Str.regexp
      ".*ligo_cli_command{user=\"[a-z0-9-]+\", repository=\"[a-z0-9-]+\", version=\"\", \
       command=\"print_cst\"} \
       1.*ligo_cli_execution_by_syntax_and_protocol{user=\"[a-z0-9-]+\", \
       repository=\"[a-z0-9-]+\", version=\"\", command=\"print_cst\", \
       syntax=\"CameLIGO\", protocol=\"[a-z]+\"} 1.*"
  in
  let _ = run_ligo [ "print"; "cst"; "contracts/build/cycle_A.mligo" ] in
  check_analytics expected_registry_output_regex expected_agg_registry_content_regex


(* let test_print_ast_imperative ~raise:_ () =
  let expected_registry_output_regex = Str.regexp ".*" in
  let expected_agg_registry_content_regex =
    Str.regexp
      ".*ligo_cli_command{user=\"[a-z0-9-]+\", repository=\"[a-z0-9-]+\", version=\"\", command=\"print_ast-imperative\"} \
       1.*ligo_cli_execution_by_syntax_and_protocol{user=\"[a-z0-9-]+\", \
       repository=\"[a-z0-9-]+\", version=\"\", command=\"print_ast-imperative\", syntax=\"CameLIGO\", \
       protocol=\"[a-z]+\"} 1.*"
  in
  let _ = run_ligo [ "print"; "ast-imperative"; "contracts/build/cycle_A.mligo" ] in
  check_analytics expected_registry_output_regex expected_agg_registry_content_regex *)

let test_print_ast_core ~raise:_ () =
  let expected_registry_output_regex = Str.regexp ".*" in
  let expected_agg_registry_content_regex =
    Str.regexp
      ".*ligo_cli_command{user=\"[a-z0-9-]+\", repository=\"[a-z0-9-]+\", version=\"\", \
       command=\"print_ast-core\"} \
       1.*ligo_cli_execution_by_syntax_and_protocol{user=\"[a-z0-9-]+\", \
       repository=\"[a-z0-9-]+\", version=\"\", command=\"print_ast-core\", \
       syntax=\"CameLIGO\", protocol=\"[a-z]+\"} 1.*"
  in
  let _ = run_ligo [ "print"; "ast-core"; "contracts/build/cycle_A.mligo" ] in
  check_analytics expected_registry_output_regex expected_agg_registry_content_regex


(* test command fail
let test_print_ast_typed ~raise:_ () =
  let expected_registry_output_regex = Str.regexp ".*" in
  let expected_agg_registry_content_regex =
    Str.regexp
      ".*ligo_cli_command{user=\"[a-z0-9-]+\", repository=\"[a-z0-9-]+\", version=\"\", command=\"print_ast-typed\"} \
       1.*ligo_cli_execution_by_syntax_and_protocol{user=\"[a-z0-9-]+\", \
       repository=\"[a-z0-9-]+\", version=\"\", command=\"print_ast-typed\", syntax=\"CameLIGO\", \
       protocol=\"[a-z]+\"} 1.*"
  in
  let _ = run_ligo [ "print"; "ast-typed"; "contracts/build/cycle_A.mligo" ] in
  check_analytics expected_registry_output_regex expected_agg_registry_content_regex

let test_print_ast_aggregated ~raise:_ () =
  let expected_registry_output_regex = Str.regexp ".*" in
  let expected_agg_registry_content_regex =
    Str.regexp
      ".*ligo_cli_command{user=\"[a-z0-9-]+\", repository=\"[a-z0-9-]+\", version=\"\", command=\"print_ast-aggregated\"} \
       1.*ligo_cli_execution_by_syntax_and_protocol{user=\"[a-z0-9-]+\", \
       repository=\"[a-z0-9-]+\", version=\"\", command=\"print_ast-aggregated\", syntax=\"CameLIGO\", \
       protocol=\"[a-z]+\"} 1.*"
  in
  let _ = run_ligo [ "print"; "ast-aggregated"; "contracts/build/cycle_A.mligo" ] in
  check_analytics expected_registry_output_regex expected_agg_registry_content_regex

let test_print_ast_expanded ~raise:_ () =
  let expected_registry_output_regex = Str.regexp ".*" in
  let expected_agg_registry_content_regex =
    Str.regexp
      ".*ligo_cli_command{user=\"[a-z0-9-]+\", repository=\"[a-z0-9-]+\", version=\"\", command=\"print_ast-expanded\"} \
       1.*ligo_cli_execution_by_syntax_and_protocol{user=\"[a-z0-9-]+\", \
       repository=\"[a-z0-9-]+\", version=\"\", command=\"print_ast-expanded\", syntax=\"CameLIGO\", \
       protocol=\"[a-z]+\"} 1.*"
  in
  let _ = run_ligo [ "print"; "ast-expanded"; "contracts/build/cycle_A.mligo" ] in
  check_analytics expected_registry_output_regex expected_agg_registry_content_regex

let test_print_mini_c ~raise:_ () =
  let expected_registry_output_regex = Str.regexp ".*" in
  let expected_agg_registry_content_regex =
    Str.regexp
      ".*ligo_cli_command{user=\"[a-z0-9-]+\", repository=\"[a-z0-9-]+\", version=\"\", command=\"print_mini-c\"} \
       1.*ligo_cli_execution_by_syntax_and_protocol{user=\"[a-z0-9-]+\", \
       repository=\"[a-z0-9-]+\", version=\"\", command=\"print_mini-c\", syntax=\"CameLIGO\", \
       protocol=\"[a-z]+\"} 1.*"
  in
  let _ = run_ligo [ "print"; "mini-c"; "contracts/build/cycle_A.mligo" ] in
  check_analytics expected_registry_output_regex expected_agg_registry_content_regex *)

let test_print_dependency_graph ~raise:_ () =
  let expected_registry_output_regex = Str.regexp ".*" in
  let expected_agg_registry_content_regex =
    Str.regexp
      ".*ligo_cli_command{user=\"[a-z0-9-]+\", repository=\"[a-z0-9-]+\", version=\"\", \
       command=\"print_dependency-graph\"} \
       1.*ligo_cli_execution_by_syntax_and_protocol{user=\"[a-z0-9-]+\", \
       repository=\"[a-z0-9-]+\", version=\"\", command=\"print_dependency-graph\", \
       syntax=\"CameLIGO\", protocol=\"[a-z]+\"} 1.*"
  in
  let _ = run_ligo [ "print"; "dependency-graph"; "contracts/build/cycle_A.mligo" ] in
  check_analytics expected_registry_output_regex expected_agg_registry_content_regex


let test_transpile_contract ~raise:_ () =
  let expected_registry_output_regex = Str.regexp ".*" in
  let expected_agg_registry_content_regex =
    Str.regexp
      ".*ligo_cli_command{user=\"[a-z0-9-]+\", repository=\"[a-z0-9-]+\", version=\"\", \
       command=\"transpile_contract\"} 1.*ligo_cli_transpile{user=\"[a-z0-9-]+\", \
       repository=\"[a-z0-9-]+\", version=\"\", command=\"transpile_contract\", \
       old_syntax=\"PascaLIGO\", new_syntax=\"jsligo\"} 1.*"
  in
  let _ =
    run_ligo
      [ "transpile"; "contract"; "contracts/example.ligo"; "--to-syntax"; "jsligo" ]
  in
  check_analytics expected_registry_output_regex expected_agg_registry_content_regex


let test_init_contract ~raise:_ () =
  let expected_registry_output_regex = Str.regexp ".*" in
  let expected_agg_registry_content_regex =
    Str.regexp
      ".*ligo_cli_command{user=\"[a-z0-9-]+\", repository=\"[a-z0-9-]+\", version=\"\", \
       command=\"init_contract\"} 1.*ligo_cli_init{user=\"[a-z0-9-]+\", \
       repository=\"[a-z0-9-]+\", version=\"\", command=\"init_contract\", \
       template=\"advisor-cameligo\"} 1.*"
  in
  let _ = run_ligo [ "init"; "contract"; "--template"; "advisor-cameligo" ] in
  check_analytics expected_registry_output_regex expected_agg_registry_content_regex


let test_init_library ~raise:_ () =
  let expected_registry_output_regex = Str.regexp ".*" in
  let expected_agg_registry_content_regex =
    Str.regexp
      ".*ligo_cli_command{user=\"[a-z0-9-]+\", repository=\"[a-z0-9-]+\", version=\"\", \
       command=\"init_library\"} 1.*ligo_cli_init{user=\"[a-z0-9-]+\", \
       repository=\"[a-z0-9-]+\", version=\"\", command=\"init_library\", \
       template=\"ligo-fa\"} 1.*"
  in
  let _ = run_ligo [ "init"; "library"; "--template"; "ligo-fa" ] in
  check_analytics expected_registry_output_regex expected_agg_registry_content_regex


let main =
  test_suite
    "LIGO analytics tests"
    [ test "compile contract" test_compile_contract
    ; test "compile expression" test_compile_expression
    ; test "compile parameter" test_compile_parameter
    ; test "compile storage" test_compile_storage
    ; test "run dry-run" test_run_dry_run
    ; test "run evaluate-call" test_run_evaluate_call
      (* ; test "run evaluate-expr" test_run_evaluate_expr *)
    ; test "run interpret" test_run_interpret
    ; test "run test" test_run_test
    ; test "run test-expr " test_run_test_expr
    ; test "info get-scope " test_info_get_scope
    ; test "info list-declarations " test_info_list_declaration
    ; test "info measure-contract " test_info_measure_contract
      (* ; test "repl " test_repl_cst *)
    ; test "changelog" test_changelog
    ; test "print preprocessed" test_print_preprocessed
    ; test "print dependency_graph" test_print_dependency_graph
    ; test "print pretty" test_print_pretty
    ; test "print cst" test_print_cst
      (* ; test "print ast_imperative" test_print_ast_imperative *)
    ; test "print ast_core" test_print_ast_core
      (* ; test "print ast_typed" test_print_ast_typed
    ; test "print ast_aggregated" test_print_ast_aggregated
    ; test "print ast_expanded" test_print_ast_expanded
    ; test "print mini_c" test_print_mini_c *)
    ; test "transpile contract" test_transpile_contract
    ; test "init library" test_init_library
    ; test "init contract" test_init_contract
    ]
