open Trace
open Ast_typed
open Spilling
open Main_errors

module SMap = Map.Make(String)

let compile_with_modules ~raise ?(module_env = SMap.empty) : Ast_typed.module_fully_typed -> Mini_c.program * AST.type_expression SMap.t = fun p ->
  trace ~raise spilling_tracer @@ compile_module ~module_env:module_env p

let compile ~raise ?(module_env = SMap.empty) : Ast_typed.module_fully_typed -> Mini_c.program = fun p ->
  let mini_c,_ = compile_with_modules ~raise ~module_env:module_env p in
  mini_c

let compile_expression ~raise ?(module_env = SMap.empty) : expression -> Mini_c.expression = fun e ->
  trace ~raise spilling_tracer @@ compile_expression ~module_env:module_env e

let assert_equal_contract_type ~raise : Simple_utils.Runned_result.check_type -> string -> Ast_typed.module_fully_typed -> Ast_typed.expression -> unit  =
    fun c entry contract param ->
  let entry_point = trace_option ~raise entrypoint_not_found (Ast_typed.get_entry contract entry) in
  trace ~raise (arguments_check_tracer c) (
    fun ~raise ->
    match entry_point.type_expression.type_content with
    | T_arrow {type1=args} -> (
        match args.type_content with
        | T_record m when LMap.cardinal m.content = 2 -> (
          let {associated_type=param_exp;_} = LMap.find (Label "0") m.content in
          let {associated_type=storage_exp;_} = LMap.find (Label "1") m.content in
            match c with
            | Check_parameter -> trace ~raise checking_tracer @@ Checking.assert_type_expression_eq entry_point.location (param_exp, param.type_expression)
            | Check_storage   -> trace ~raise checking_tracer @@ Checking.assert_type_expression_eq entry_point.location (storage_exp, param.type_expression)
        )
        | _ -> raise.raise @@ entrypoint_not_a_function )
    | _ -> raise.raise @@ entrypoint_not_a_function
  )

let decompile_env e = Checking.decompile_env e
let list_declarations (m : Ast_typed.module') : string list =
  List.fold_left
    ~f:(fun prev el ->
      let open Location in
      match (el.wrap_content : Ast_typed.declaration) with
      | Declaration_constant {binder;_} -> (Var.to_name binder.wrap_content)::prev
      | _ -> prev)
    ~init:[] m

let list_type_declarations (m : Ast_typed.module') : string list =
  List.fold_left
    ~f:(fun prev el ->
      let open Location in
      match (el.wrap_content : Ast_typed.declaration) with
      | Declaration_type {type_binder;_} -> (Var.to_name type_binder)::prev
      | _ -> prev)
    ~init:[] m

let list_mod_declarations (m : Ast_typed.module') : string list =
  List.fold_left
    ~f:(fun prev el ->
      let open Location in
      match (el.wrap_content : Ast_typed.declaration) with
      | Declaration_module {module_binder;_} -> (module_binder)::prev
      | Module_alias {alias;_} -> (alias)::prev
      | _ -> prev)
    ~init:[] m
