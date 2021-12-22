(* This file morph a module into a record for compilation to backends that doesn't support module 
  This lead to bad michelson code as accessing module is expensive. Consider namespacing the value instead *)
open Helpers
open Simple_utils.Trace
open Ast_typed

module Env = struct
  type t  = (expression_variable,type_expression) List.Assoc.t (* Use a fast lookup structure like Map *)
  let add = List.Assoc.add ~equal:equal_expression_variable
  let get  = List.Assoc.find ~equal:equal_expression_variable 
  let init ?env () =
    match env with None -> []
    | Some (env) ->
      let f e d = match Location.unwrap d with
        Ast_typed.Declaration_constant {name=_;binder;expr;attr=_}  -> add e binder expr.type_expression
      | _ -> e
      in
      Environment.fold ~f ~init:[] @@ env
end

let rec declaration_to_expression ~raise : Env.t -> declaration_loc list -> (string * expression) list = fun env decl ->
  let self = declaration_to_expression ~raise in
  match decl with
    [] -> []
  | (hd: declaration_loc) :: tl -> 
    match hd.wrap_content with
      Declaration_constant {name=_;binder;expr;attr=_} -> 
      let _,expr = fold_map_expression (peephole_expression' ~raise) env expr in
      let env = Env.add env binder expr.type_expression in
      let binder = Var.to_name binder.wrap_content in
      (binder,expr) :: self env tl
    | Declaration_type _ -> self env tl
    | Declaration_module {module_binder;module_;module_attr=_} ->
      let expr = module_to_record ~raise env module_ in
      let env = Env.add env
        (Location.wrap @@ Var.of_name module_binder) expr.type_expression in
      (module_binder,expr) :: self env tl
    | Module_alias {alias;binders} ->
      let let_binder = Location.wrap @@ Var.of_name alias in
      let (init,nexts) = binders in
      (match Env.get env (Location.wrap @@ Var.of_name init) with None -> raise.raise @@ Errors.corner_case "The module shouldn't type"
        | Some (record_type) ->
          let rhs = List.fold ~f:(fun record path -> 
            let record_type = Option.value_exn (get_t_record record.type_expression) in
            let type_expression = match (LMap.find_opt path record_type.content) with None -> raise.raise @@ Errors.corner_case "Module shouldn't type"
              | Some (r) -> r.associated_type in
            {expression_content = E_record_accessor {record;path}; type_expression;location=hd.location}) 
            ~init:(e_a_variable (Location.wrap @@ Var.of_name init) @@ record_type) @@ List.map ~f:(fun x -> Label x) nexts in
          let env = Env.add env let_binder rhs.type_expression in
          (alias, rhs) :: self env tl
      )
    
and module_to_record ~raise : Env.t -> module' -> expression = fun c m ->
  let lst = declaration_to_expression ~raise c m in
  let f = fun expr (binder,ex) ->
    let var = Location.wrap @@ Var.of_name binder in
    let attr = {inline=true; no_mutation=false;view=false;public=true} in
    let expr = fun e -> expr @@ 
      e_a_let_in var ex e attr
    in
    (expr, (Label binder, e_a_variable var ex.type_expression))
  in
  let expr, record = List.fold_map ~f ~init:(fun e -> e) lst in
  expr @@ e_a_record ~layout:L_comb @@ LMap.of_list record

and peephole_expression' ~raise : Env.t -> expression -> bool * Env.t * expression = fun e expr ->
  match expr.expression_content with
    E_mod_in {module_binder; rhs; let_result} ->
    let let_binder = Location.wrap @@ Var.of_name module_binder in
    let rhs = module_to_record ~raise e rhs in
    let c = Env.add e let_binder rhs.type_expression in
    true,c,{ expr with expression_content=E_let_in {let_binder; rhs;let_result; attr={inline=true;no_mutation=false;view=false;public=false}}}
  | E_mod_alias {alias;binders;result} ->
    let let_binder = Location.wrap @@ Var.of_name alias in
    let (init,nexts) = binders in
    (match Env.get e (Location.wrap @@ Var.of_name init) with None -> raise.raise @@ Errors.corner_case "The module shouldn't type"
      | Some (record_type) ->
        let rhs = List.fold ~f:(fun record path -> 
          let record_type = Option.value_exn (get_t_record record.type_expression) in
          let type_expression = match (LMap.find_opt path record_type.content) with None -> raise.raise @@ Errors.corner_case "Module shouldn't type"
            | Some (r) -> r.associated_type in
          { expr with expression_content = E_record_accessor {record;path}; type_expression}) 
          ~init:(e_a_variable (Location.wrap @@ Var.of_name init) @@ record_type) @@ List.map ~f:(fun x -> Label x) nexts in
        let attr = { inline = true;no_mutation=false;view=false;public=false } in
        let c = Env.add e let_binder rhs.type_expression in
        true,c,{expr with expression_content=E_let_in {let_binder;rhs;let_result=result;attr}}
    )
  | E_module_accessor {module_name; element} ->
    let module_var = Location.wrap @@ Var.of_name module_name in
    (match Env.get e module_var with None -> raise.raise @@ Errors.corner_case "The module shouldn't type"
      | Some (record_type) ->
      let module_var = e_a_variable module_var record_type in
      let rec aux (element : expression) = 
        (match element.expression_content with
          | E_module_accessor {module_name;element} -> (Label module_name) :: aux element
          | E_variable var -> [Label (Var.to_name @@ Location.unwrap var)]
          | E_record_accessor {record; path} -> aux record @ [path]
          | _ -> raise.raise @@ Errors.corner_case 
            @@ Format.asprintf "The parser shouldn't allowed this construct : %a" Ast_typed.PP.expression element
        )
      in
      let acces_list = aux element in
      let expr = List.fold ~f:(fun record path ->
        let record_type = Option.value_exn (get_t_record record.type_expression) in
        let type_expression = match (LMap.find_opt path record_type.content) with None -> raise.raise @@ Errors.corner_case "Module shouldn't type"
          | Some (r) -> r.associated_type in
        { expr with expression_content = E_record_accessor {record;path}; type_expression }) 
        ~init:module_var acces_list in
      true,e,expr
    )
  | E_type_inst _ -> raise.raise @@ Errors.corner_case "Monomorphisation should run before Module Morphing"
  | _ -> true,e,expr

let peephole_declaration ~raise : Env.t -> declaration_loc -> Env.t * declaration_loc = fun e m ->
  match m.wrap_content with
    Declaration_module {module_binder;module_;module_attr} ->
    let binder = Location.wrap @@ Var.of_name @@ module_binder in
    let expr = module_to_record ~raise e module_ in
    let attr = {inline=true; no_mutation=false;view=false;public=module_attr.public} in
    let e = Env.add e binder expr.type_expression in
    e,{ m with wrap_content=Declaration_constant {name=None;binder;expr;attr}}
  | Module_alias {alias;binders} ->
    let (init,nexts) = binders in 
    let binder = Location.wrap @@ Var.of_name @@ alias in
    (match Env.get e (Location.wrap @@ Var.of_name init) with None -> raise.raise @@ Errors.corner_case "The module shouldn't type"
    | Some (record_type) ->
      let expr = 
        let record_expr = e_a_variable (Location.wrap @@ Var.of_name init) (record_type) in
        List.fold ~f:(fun record path ->
          let record_type = Option.value_exn (get_t_record record.type_expression) in
          let type_expression = match (LMap.find_opt path record_type.content) with None -> raise.raise @@ Errors.corner_case "Module shouldn't type"
            | Some (r) -> r.associated_type in
          {record with expression_content = E_record_accessor {record;path}; type_expression}
          ) ~init:record_expr @@ List.map ~f:(fun s -> Label s) nexts 
      in
      let e = Env.add e binder expr.type_expression in
      let attr = {inline=true; no_mutation=false;view=false;public=true} in
      e,{ m with wrap_content = Declaration_constant {name=None;binder;expr;attr}}
    )
  | Declaration_constant {name;binder;expr;attr} ->
    let _,expr = fold_map_expression (peephole_expression' ~raise) e expr in
    let e = Env.add e binder expr.type_expression in
    e,{ m with wrap_content = Declaration_constant {name;binder;expr;attr}}

  | Declaration_type _ -> e,m

let peephole_module ~raise : Env.t -> module' -> Env.t * module' = fun e m ->
  List.fold_map ~f:(peephole_declaration ~raise) ~init:e m

let peephole_program ~raise : Environment.t -> program -> program = 
  fun env p -> snd @@ peephole_module ~raise (Env.init ~env ()) p

let peephole_expression ~raise : Environment.t -> expression -> expression =
  fun env e -> snd @@ fold_map_expression (peephole_expression' ~raise) (Env.init ~env ()) e