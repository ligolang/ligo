open Main_errors
open Trace
open Ast_typed

let compile : Ast_typed.program -> (Mini_c.program, _) result = fun p ->
  trace transpiler_tracer @@ Transpiler.transpile_program p

let compile_expression : expression -> (Mini_c.expression, _) result = fun e ->
  trace transpiler_tracer @@ Transpiler.transpile_annotated_expression e

let assert_equal_contract_type : Simple_utils.Runned_result.check_type -> string -> Ast_typed.program -> Ast_typed.expression -> (unit , _) result =
    fun c entry contract param ->
  let%bind entry_point = trace_option entrypoint_not_found (Ast_typed.get_entry contract entry) in
  trace (arguments_check_tracer c) (
    match entry_point.type_expression.type_content with
    | T_arrow {type1=args} -> (
        match args.type_content with
        | T_record m when LMap.cardinal m = 2 -> (
          let {field_type=param_exp;_} = LMap.find (Label "0") m in
          let {field_type=storage_exp;_} = LMap.find (Label "1") m in
            match c with
            | Check_parameter -> trace typer_tracer @@ Typer.assert_type_expression_eq (param_exp,   param.type_expression)
            | Check_storage   -> trace typer_tracer @@ Typer.assert_type_expression_eq (storage_exp, param.type_expression)
        )
        | _ -> fail @@ entrypoint_not_a_function )
    | _ -> fail @@ entrypoint_not_a_function
  )

let some_interpret x = trace interpret_tracer @@ Interpreter.eval x
