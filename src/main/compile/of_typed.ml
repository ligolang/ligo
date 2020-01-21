open Trace
open Ast_typed

let compile : Ast_typed.program -> Mini_c.program result = fun p ->
  Transpiler.transpile_program p

let compile_expression : annotated_expression -> Mini_c.expression result = fun e ->
  Transpiler.transpile_annotated_expression e

type check_type = Check_parameter | Check_storage
let assert_equal_contract_type : check_type -> string -> Ast_typed.program -> Ast_typed.value -> unit result =
    fun c entry contract param -> Trace.trace (simple_info "Check argument type against contract type") (
  let%bind entry_point = Ast_typed.get_entry contract entry in
  match entry_point.type_annotation.type_value' with
  | T_arrow (args,_) -> (
      match args.type_value' with
      | T_tuple [param_exp;storage_exp] -> (
          match c with
          | Check_parameter -> assert_type_value_eq (param_exp,   param.type_annotation)
          | Check_storage   -> assert_type_value_eq (storage_exp, param.type_annotation)
      )
      | _ -> dummy_fail
  )
  | _ -> dummy_fail )

let pretty_print ppf program =
  Ast_typed.PP.program ppf program