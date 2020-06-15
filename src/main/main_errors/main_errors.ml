module Formatter = Formatter
module Types = Types
type all = Types.all

(* passes tracers *)

let parser_tracer (e:Parser.Errors.parser_error) : all = `Main_parser e
let cit_cameligo_tracer (e:Concrete_to_imperative.Errors_cameligo.abs_error) : all = `Main_cit_cameligo e
let cit_pascaligo_tracer (e:Concrete_to_imperative.Errors_pascaligo.abs_error) : all = `Main_cit_pascaligo e
let self_ast_imperative_tracer (e:Self_ast_imperative.Errors.self_ast_imperative_error) : all = `Main_self_ast_imperative e
let imperative_to_sugar_tracer (e:Imperative_to_sugar.Errors.imperative_to_sugar_error) : all = `Main_imperative_to_sugar e
let sugar_to_core_tracer (e:Sugar_to_core.Errors.sugar_to_core_error) : all = `Main_sugar_to_core e
let typer_tracer (e:Typer.Errors.typer_error) : all = `Main_typer e
let self_ast_typed_tracer (e:Self_ast_typed.Errors.self_ast_typed_error) : all = `Main_self_ast_typed e
let self_mini_c_tracer (e:Self_mini_c.Errors.self_mini_c_error) : all = `Main_self_mini_c e
let transpiler_tracer (e:Transpiler.Errors.transpiler_error) : all = `Main_transpiler e
let compiler_tracer (e:Compiler.Errors.compiler_error) : all = `Main_compiler e
let interpret_tracer (e:Interpreter.interpreter_error) : all = `Main_interpreter e

let uncompile_mini_c : Transpiler.Errors.transpiler_error -> all = fun e -> `Main_uncompile_mini_c e
let uncompile_typed : Typer.Errors.typer_error -> all = fun e -> `Main_uncompile_typed e
let uncompile_michelson : Compiler.Errors.compiler_error -> all = fun e -> `Main_uncompile_michelson e

(* top-level glue (in between passes) *)

let syntax_auto_detection extension : all = `Main_invalid_extension extension
let invalid_syntax syntax : all = `Main_invalid_syntax_name syntax

let entrypoint_not_a_function : all = `Main_entrypoint_not_a_function
let entrypoint_not_found : all = `Main_entrypoint_not_found

(* Michelson execution errors *)

let arguments_check_tracer ps err : all = `Main_check_typed_arguments (ps, err)
let unparse_tracer errs : all = `Main_unparse_tracer errs
let typecheck_contract_tracer c errs : all = `Main_typecheck_contract_tracer (c,errs)
let typecheck_parameters_tracer _ : all = `Main_typecheck_parameter

let bad_parameter c : all = `Main_bad_michelson_parameter c
let bad_storage   c : all = `Main_bad_michelson_storage c
let bad_contract  c : all = `Main_bad_michelson c
let gas_exhaustion : all = `Main_gas_exhaustion
let unknown : all = `Main_unknown

let unknown_failwith_type : all = `Main_unknown_failwith_type
let failwith fw : all = `Main_execution_failed fw

let unparsing_michelson_tracer err : all = `Main_unparse_michelson_result err
let parsing_payload_tracer err : all = `Main_parse_payload err
let packing_payload_tracer err : all = `Main_pack_payload err
let parsing_input_tracer err : all = `Main_parse_michelson_input err
let parsing_code_tracer err : all = `Main_parse_michelson_code err
let error_of_execution_tracer err : all = `Main_michelson_execution_error err

let invalid_amount s : all = `Main_invalid_amount s
let invalid_address s : all = `Main_invalid_address s
let invalid_timestamp s : all = `Main_invalid_timestamp s

(* test errors *)

let test_tracer name err : all = `Test_err_tracer (name,err)
let test_run_tracer entrypoint err : all = `Test_run_tracer (entrypoint,err)
let test_expect expected actual : all = `Test_expect_tracer (expected,actual)
let test_expect_n_tracer i err : all = `Test_expect_n_tracer (i,err)
let test_expect_exp_tracer e err : all = `Test_expect_exp_tracer (e,err)
let test_expect_eq_n_tracer i err : all = `Test_expect_eq_n_tracer (i,err)
let test_internal loc : all = `Test_internal loc
let test_md_file_tracer md_file s group prg err : all = `Test_md_file_tracer (md_file,s,group,prg,err) 
let test_code_block_arg arg : all = `Test_bad_code_block arg
let test_expected_to_fail : all = `Test_expected_to_fail
let test_not_expected_to_fail : all = `Test_not_expected_to_fail