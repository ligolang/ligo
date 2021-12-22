open Simple_utils.Trace
open Ast_typed
open Spilling
open Main_errors

module Var = Simple_utils.Var

module SMap = Map.Make(String)

let compile ~raise : Ast_typed.program -> Mini_c.program = fun p ->
  trace ~raise spilling_tracer @@ compile_program p

let compile_expression ~raise : expression -> Mini_c.expression = fun e ->
  trace ~raise spilling_tracer @@ compile_expression e

let compile_type ~raise : type_expression -> Mini_c.type_expression = fun e ->
  trace ~raise spilling_tracer @@ compile_type e

let assert_equal_contract_type ~raise : Simple_utils.Runned_result.check_type -> string -> Ast_typed.program -> Ast_typed.expression -> unit  =
    fun c entry contract param ->
  let entry_point = trace_option ~raise main_entrypoint_not_found (Ast_typed.get_entry contract entry) in
  trace ~raise (check_typed_arguments_tracer c) (
    fun ~raise ->
    match entry_point.type_expression.type_content with
    | T_arrow {type1=args;type2=_} -> (
        match args.type_content with
        | T_record m when LMap.cardinal m.content = 2 -> (
          let {associated_type=param_exp;_} = LMap.find (Label "0") m.content in
          let {associated_type=storage_exp;_} = LMap.find (Label "1") m.content in
            match c with
            | Check_parameter -> trace ~raise checking_tracer @@ Checking.assert_type_expression_eq entry_point.location (param_exp, param.type_expression)
            | Check_storage   -> trace ~raise checking_tracer @@ Checking.assert_type_expression_eq entry_point.location (storage_exp, param.type_expression)
        )
        | _ -> raise.raise @@ main_entrypoint_not_a_function )
    | _ -> raise.raise @@ main_entrypoint_not_a_function
  )

let rec get_views : Ast_typed.program -> (string * location) list = fun p ->
  let f : (string * location) list -> declaration_loc -> (string * location) list =
    fun acc {wrap_content=decl ; location=_ } ->
      match decl with
      | Declaration_constant { name=_ ; binder ; expr=_ ; attr } when attr.view -> (Var.to_name binder.wrap_content, binder.location)::acc
      | Declaration_module { module_binder=_ ; module_ ; module_attr=_} -> get_views module_ @ acc
      | _ -> acc
  in 
  List.fold ~init:[] ~f p

let list_declarations (m : Ast_typed.module') : string list =
  List.fold_left
    ~f:(fun prev el ->
      let open Simple_utils.Location in
      match (el.wrap_content : Ast_typed.declaration) with
      | Declaration_constant {binder;_} -> (Var.to_name binder.wrap_content)::prev
      | _ -> prev)
    ~init:[] m

let list_type_declarations (m : Ast_typed.module') : string list =
  List.fold_left
    ~f:(fun prev el ->
      let open Simple_utils.Location in
      match (el.wrap_content : Ast_typed.declaration) with
      | Declaration_type {type_binder;_} -> (Var.to_name type_binder)::prev
      | _ -> prev)
    ~init:[] m

let list_mod_declarations (m : Ast_typed.module') : string list =
  List.fold_left
    ~f:(fun prev el ->
      let open Simple_utils.Location in
      match (el.wrap_content : Ast_typed.declaration) with
      | Declaration_module {module_binder;_} -> (module_binder)::prev
      | Module_alias {alias;_} -> (alias)::prev
      | _ -> prev)
    ~init:[] m
