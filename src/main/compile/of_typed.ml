open Trace
open Ast_typed

let compile : Ast_typed.program -> Mini_c.program result = fun p ->
  Transpiler.transpile_program p

let compile_expression : expression -> Mini_c.expression result = fun e ->
  Transpiler.transpile_annotated_expression e

type check_type = Check_parameter | Check_storage
let assert_equal_contract_type : check_type -> string -> Ast_typed.program -> Ast_typed.expression -> unit result =
    fun c entry contract param -> Trace.trace (simple_info "Check argument type against contract type") (
  let%bind entry_point = Ast_typed.get_entry contract entry in
  match entry_point.type_expression.type_content with
  | T_arrow {type1=args} -> (
      match args.type_content with
      | T_record m when LMap.cardinal m = 2 -> (
        let param_exp = LMap.find (Label "0") m in
        let storage_exp = LMap.find (Label "1") m in
          match c with
          | Check_parameter -> assert_type_expression_eq (param_exp,   param.type_expression)
          | Check_storage   -> assert_type_expression_eq (storage_exp, param.type_expression)
      )
      | _ -> dummy_fail
  )
  | _ -> dummy_fail )

let pretty_print ppf program =
  Ast_typed.PP.program ppf program

let some_interpret = Interpreter.dummy
