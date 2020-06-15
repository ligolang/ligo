type all = 
[
 | `Main_invalid_syntax_name of string
 | `Main_invalid_extension of string
 | `Main_bad_michelson_parameter of Michelson.michelson
 | `Main_bad_michelson_storage of Michelson.michelson
 | `Main_bad_michelson of Michelson.michelson
 | `Main_gas_exhaustion
 | `Main_unparse_tracer of [ `Tezos_alpha_error of Proto_alpha_utils.Error_monad.error ] list
 | `Main_typecheck_contract_tracer of Michelson.michelson * [ `Tezos_alpha_error of Proto_alpha_utils.Error_monad.error ] list
 | `Main_typecheck_parameter
 | `Main_check_typed_arguments of Simple_utils.Runned_result.check_type * all
 | `Main_unknown_failwith_type
 | `Main_unknown
 | `Main_execution_failed of Runned_result.failwith
 | `Main_unparse_michelson_result of Proto_alpha_utils.Trace.tezos_alpha_error list
 | `Main_parse_payload of Proto_alpha_utils.Trace.tezos_alpha_error list
 | `Main_pack_payload of Proto_alpha_utils.Trace.tezos_alpha_error list
 | `Main_parse_michelson_input of Proto_alpha_utils.Trace.tezos_alpha_error list
 | `Main_parse_michelson_code of Proto_alpha_utils.Trace.tezos_alpha_error list
 | `Main_michelson_execution_error of Proto_alpha_utils.Trace.tezos_alpha_error list

 | `Main_parser of Parser.Errors.parser_error
 | `Main_self_ast_imperative of Self_ast_imperative.Errors.self_ast_imperative_error
 | `Main_imperative_to_sugar of Imperative_to_sugar.Errors.imperative_to_sugar_error
 | `Main_sugar_to_core of Sugar_to_core.Errors.sugar_to_core_error
 | `Main_cit_pascaligo of Concrete_to_imperative.Errors_pascaligo.abs_error
 | `Main_cit_cameligo of Concrete_to_imperative.Errors_cameligo.abs_error
 | `Main_typer of Typer.Errors.typer_error
 | `Main_interpreter of Interpreter.interpreter_error
 | `Main_self_ast_typed of Self_ast_typed.Errors.self_ast_typed_error
 | `Main_self_mini_c of Self_mini_c.Errors.self_mini_c_error
 | `Main_transpiler of Transpiler.Errors.transpiler_error
 | `Main_compiler of Compiler.Errors.compiler_error

 | `Main_uncompile_michelson of Compiler.Errors.compiler_error
 | `Main_uncompile_mini_c of Transpiler.Errors.transpiler_error
 | `Main_uncompile_typed of Typer.Errors.typer_error
 | `Main_entrypoint_not_a_function
 | `Main_entrypoint_not_found
 | `Main_invalid_amount of string
 | `Main_invalid_address of string
 | `Main_invalid_timestamp of string

 | `Test_err_tracer of string * all
 | `Test_run_tracer of string * all
 | `Test_expect_tracer of Ast_core.expression * Ast_core.expression
 | `Test_expect_n_tracer of int * all
 | `Test_expect_exp_tracer of Ast_core.expression * all
 | `Test_expect_eq_n_tracer of int * all
 | `Test_internal of string
 | `Test_md_file_tracer of string * string * string * string * all
 | `Test_bad_code_block of string
 | `Test_expected_to_fail
 | `Test_not_expected_to_fail
] 