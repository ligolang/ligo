module Formatter = Formatter
module Types = Types
type all = Types.all

(* passes tracers *)

let parser_tracer (e:Parser.Errors.parser_error) : all = `Main_parser e
let pretty_tracer (e:Parser.Errors.parser_error) : all = `Main_pretty e
let cit_cameligo_tracer (e:Tree_abstraction.Cameligo.Errors.abs_error) : all = `Main_cit_cameligo e
let cit_pascaligo_tracer (e:Tree_abstraction.Pascaligo.Errors.abs_error) : all = `Main_cit_pascaligo e
let cit_reasonligo_tracer (e:Tree_abstraction.Reasonligo.Errors.abs_error) : all = `Main_cit_reasonligo e
let self_ast_imperative_tracer (e:Self_ast_imperative.Errors.self_ast_imperative_error) : all = `Main_self_ast_imperative e
let purification_tracer (e:Purification.Errors.purification_error) : all = `Main_purification e
let depurification_tracer (e:Purification.Errors.purification_error) : all = `Main_depurification e
let desugaring_tracer (e:Desugaring.Errors.desugaring_error) : all = `Main_desugaring e
let sugaring_tracer (e:Desugaring.Errors.desugaring_error) : all = `Main_sugaring e
let typer_tracer (e:Typer.Errors.typer_error) : all = `Main_typer e
let self_ast_typed_tracer (e:Self_ast_typed.Errors.self_ast_typed_error) : all = `Main_self_ast_typed e
let self_mini_c_tracer (e:Self_mini_c.Errors.self_mini_c_error) : all = `Main_self_mini_c e
let spilling_tracer (e:Spilling.Errors.spilling_error) : all = `Main_spilling e
let stacking_tracer (e:Stacking.Errors.stacking_error) : all = `Main_stacking e
let interpret_tracer (e:Interpreter.interpreter_error) : all = `Main_interpreter e

let decompile_mini_c : Spilling.Errors.spilling_error -> all = fun e -> `Main_decompile_mini_c e
let decompile_typed : Typer.Errors.typer_error -> all = fun e -> `Main_decompile_typed e
let decompile_michelson : Stacking.Errors.stacking_error -> all = fun e -> `Main_decompile_michelson e

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

let unknown : all = `Main_unknown

let unknown_failwith_type : all = `Main_unknown_failwith_type
let failwith fw : all = `Main_execution_failed fw

let unparsing_michelson_tracer err : all = `Main_unparse_michelson_result err
let parsing_payload_tracer err : all = `Main_parse_payload err
let packing_payload_tracer err : all = `Main_pack_payload err
let parsing_input_tracer err : all = `Main_parse_michelson_input err
let parsing_code_tracer err : all = `Main_parse_michelson_code err
let error_of_execution_tracer err : all = `Main_michelson_execution_error err

let invalid_balance s : all = `Main_invalid_balance s
let invalid_amount s : all = `Main_invalid_amount s
let invalid_source s : all = `Main_invalid_source s
let invalid_sender s : all = `Main_invalid_sender s
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
