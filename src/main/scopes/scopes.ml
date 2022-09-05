open Types
open Misc

module Formatter = Formatter
module Api_helper = Api_helper

module ModVar = Ast_typed.ModuleVar

type sub_module = { type_env : tenv  ; bindings : bindings_map }

let scopes : with_types:bool -> options:Compiler_options.middle_end -> Ast_core.module_ -> (def_map * scopes) = fun ~with_types ~options core_prg ->
  let make_v_def = make_v_def ~with_types in

  let rec find_scopes' = fun (all_defs,env,scopes,lastloc) (partials:sub_module) (e : Ast_core.expression) ->
    let bindings = partials.bindings in
    match e.expression_content with
    | E_let_in { let_result ; attr= { hidden = true ; _ } ; _ } -> (
      find_scopes' (all_defs,env,scopes,let_result.location) partials let_result
    )
    | E_let_in { let_binder = {var ; ascr ; attributes=_} ; rhs ; let_result ; attr=_} -> (
      let (all_defs,_, scopes) = find_scopes' (all_defs,env,scopes,e.location) partials rhs in
      let def = make_v_def bindings var ?core_type:ascr (Ast_core.ValueVar.get_location var) rhs.location in
      let env = add_shadowing_def (get_binder_name var) def env in
      let all_defs = merge_defs env all_defs in
      find_scopes' (all_defs,env,scopes,let_result.location) partials let_result
    )
    | E_type_in { type_binder; rhs ; let_result } -> (
      let def = make_t_def (get_type_binder_name type_binder) e.location rhs in
      let env = add_shadowing_def (get_type_binder_name type_binder) def env in
      let all_defs = merge_defs env all_defs in
      find_scopes' (all_defs,env,scopes,let_result.location) partials let_result
    )
    | E_mod_in { module_binder; rhs; let_result } -> (
      let range = ModVar.get_location module_binder in
      match rhs.wrap_content with
      | M_struct decls ->
        let (mod_top_env, mod_inner_env, scopes, _) = List.fold_left ~f:declaration ~init:(Def_map.empty, Def_map.empty, scopes, partials) decls in
        let mod_env = merge_defs mod_top_env mod_inner_env in
        let def = make_m_def ~range ~body_range:rhs.location (get_mod_binder_name module_binder) mod_env in
        let env = add_shadowing_def (get_mod_binder_name module_binder) def env in
        let all_defs = merge_defs env all_defs in
        find_scopes' (all_defs,env,scopes,let_result.location) partials let_result
      | M_variable m -> 
        let alias = [get_mod_binder_name m] in
        let def = make_m_alias_def ~range ~body_range:(ModVar.get_location m) (get_mod_binder_name module_binder) alias in
        let env = update_module_reference [m] env in
        let env = add_shadowing_def (get_mod_binder_name module_binder) def env in
        let all_defs = merge_defs env all_defs in
        find_scopes' (all_defs,env,scopes,let_result.location) partials let_result
      | M_module_path m ->
        let path,mods,body_range = List.fold ~init:([],[],Location.dummy) ~f:(fun (path,mods,loc) m -> 
          path@[get_mod_binder_name m], mods@[m], Location.cover loc (ModVar.get_location m)) 
          (List.Ne.to_list m) in
        let def = make_m_alias_def ~range ~body_range (get_mod_binder_name module_binder) path in
        let env = update_module_reference mods env in
        let env = add_shadowing_def (get_mod_binder_name module_binder) def env in
        let all_defs = merge_defs env all_defs in
        find_scopes' (all_defs,env,scopes,let_result.location) partials let_result
    )
    | E_recursive { fun_name ; fun_type ; lambda = { binder = {var;ascr=input_type; attributes=_} ; output_type = _ ; result ; _ } } -> (
      let env =
        let def = make_v_def bindings fun_name ~core_type:fun_type (Ast_typed.ValueVar.get_location fun_name) result.location in
        add_shadowing_def (get_binder_name fun_name) def env
      in
      let env =
        let def = make_v_def bindings var ?core_type:input_type (Ast_typed.ValueVar.get_location var) result.location in
        add_shadowing_def (get_binder_name var) def env
      in
      let all_defs = merge_defs env all_defs in
      find_scopes' (all_defs,env,scopes,result.location) partials result
    )
    | E_lambda { binder={var;ascr=input_type; attributes=_} ; output_type = _ ; result } -> (
      let env =
        let def = make_v_def bindings var ?core_type:input_type (Ast_typed.ValueVar.get_location var) result.location in
        add_shadowing_def (get_binder_name var) def env
      in
      let all_defs = merge_defs env all_defs in
      find_scopes' (all_defs,env,scopes,result.location) partials result
    )
    | E_type_abstraction { type_binder; result } -> (
      let def = make_t_def (get_type_binder_name type_binder) e.location (Ast_core.t_variable type_binder ()) in
      let env = add_shadowing_def (get_type_binder_name type_binder) def env in
      let all_defs = merge_defs env all_defs in
      find_scopes' (all_defs,env,scopes,result.location) partials result
    )
    | E_matching {matchee; cases} -> (
      let (all_defs,_,scopes) = find_scopes' (all_defs,env,scopes,matchee.location) partials matchee in
      let aux = fun (all_defs,scopes) ({pattern;body}: (Ast_core.expression,_) Ast_core.match_case) ->
        let aux env (p: _ Ast_core.pattern) =
          match p.wrap_content with
          | Ast_core.P_var binder ->
            let loc = Ast_core.ValueVar.get_location binder.var in
            let proj_def = make_v_def bindings binder.var loc loc in
            add_shadowing_def (get_binder_name binder.var) proj_def env
          | _ -> env
        in
        let env = Stage_common.Helpers.fold_pattern aux env pattern in
        let (all_defs,_,scopes) = find_scopes' (all_defs,env,scopes,body.location) partials body in
        let all_defs = merge_defs env all_defs in
        (all_defs,scopes)
      in
      let (all_defs,scopes) = List.fold_left ~f:aux ~init:(all_defs,scopes) cases in
      (all_defs,env,scopes)
    )
    | E_record emap -> (
      let aux = fun (all_defs,scopes) (exp:Ast_core.expression) ->
        let (all_defs,_,scopes) = find_scopes' (all_defs,env,scopes,exp.location) partials exp in
        (all_defs,scopes)
      in
      let (all_defs,scopes) = List.fold_left ~f:aux ~init:(all_defs,scopes) (Ast_core.LMap.to_list emap) in
      (all_defs,env,scopes)
    )
    | E_record_update { record ; update ; _ } -> (
      (*TODO: here record has a virtual location, check this out.. not normal *)
      let (all_defs,_,scopes) = find_scopes' (all_defs,env,scopes,record.location) partials record in
      find_scopes' (all_defs,env,scopes,update.location) partials update
    )
    | E_constant { arguments ; _ } -> (
      let aux = fun (all_defs,scopes) (exp:Ast_core.expression) ->
        let (all_defs,_,scopes) = find_scopes' (all_defs,env,scopes,exp.location) partials exp in
        (all_defs,scopes)
      in
      let (all_defs,scopes) = List.fold_left ~f:aux ~init:(all_defs,scopes) arguments in
      (all_defs,env,scopes)
    )
    | E_application { lamb ; args } -> (
      let (all_defs,_,scopes) = find_scopes' (all_defs,env,scopes,lamb.location) partials lamb in
      find_scopes' (all_defs,env,scopes,args.location) partials args
    )
    | E_ascription { anno_expr=e;_ } | E_record_accessor { record=e;_ } | E_constructor { element=e;_ } -> (
      find_scopes' (all_defs,env,scopes,e.location) partials e
    )
    | E_module_accessor { module_path ; element } -> (
      let env = update_module_reference module_path env in
      let env = add_module_element_reference module_path element env in
      let all_defs = merge_defs env all_defs in
      let scopes = add_scope (lastloc, env) scopes in
      (all_defs,env,scopes)
    )
    | E_variable x -> (
      let env = add_reference x env in
      let all_defs = merge_defs env all_defs in
      let scopes = add_scope (lastloc, env) scopes in
      (all_defs,env,scopes)
    )
    | E_literal _ | E_raw_code _ -> (
      let scopes = add_scope (lastloc, env) scopes in
      (all_defs,env,scopes)
    )
    | E_assign { binder ; expression ; _ } -> (
      (* QUESTION: in case of e_assign I dont think a shadowing def needs to be added ?? *)
      let def = make_v_def bindings binder.var ?core_type:binder.ascr (Ast_typed.ValueVar.get_location binder.var) expression.location in
      let env = add_shadowing_def (get_binder_name binder.var) def env in
      let all_defs = merge_defs env all_defs in
      find_scopes' (all_defs,env,scopes,expression.location) partials expression
    )
  
  and find_scopes (top_lvl_defs,scopes,loc) bindings e =
    let (defs,_,scopes) = find_scopes' (top_lvl_defs,top_lvl_defs,scopes,loc) bindings e in
    (defs,scopes)

  and declaration = fun (top_def_map, inner_def_map, scopes, partials) (decl : Ast_core.declaration) ->
    let typed_prg =
      (*
        if --with-types optional flag is enabled, we try typing the declaration
        to build a partial Ast_typed program.
        if a declaration do not type, we will still try to type the next one
      *)
      if with_types then 
        Simple_utils.Trace.to_option 
          (Checking.type_declaration ~options ~env:partials.type_env decl)
      else 
        None
    in
    let partials = match typed_prg with
      | Some (decl') ->
        let bindings = extract_variable_types partials.bindings decl'.wrap_content in
        let type_env = Environment.add_declaration decl' partials.type_env in
        { type_env ; bindings }
      | None -> partials
    in
    match decl.wrap_content with
    | Declaration_constant { attr = { hidden = true ; _ } ; _ } -> (
      ( top_def_map, inner_def_map, scopes , partials )
    )
    | Declaration_constant { binder= { var ; ascr ; attributes=_ } ; expr ; _ } -> (
      let (new_inner_def_map,scopes) = find_scopes (top_def_map,scopes,decl.location) partials expr in
      let inner_def_map = merge_defs new_inner_def_map inner_def_map in
      let def = make_v_def partials.bindings var ?core_type:ascr (Ast_core.ValueVar.get_location var) expr.location in
      let top_def_map = add_shadowing_def (get_binder_name var) def top_def_map in
      ( top_def_map, inner_def_map, scopes , partials )
    )
    | Declaration_type {type_attr={hidden = true; _} ; _} -> (
      ( top_def_map, inner_def_map, scopes, partials )
    )
    | Declaration_type {type_binder; type_expr ; type_attr=_} -> (
      let def = make_t_def (get_type_binder_name type_binder) decl.location type_expr in
      let top_def_map = add_shadowing_def (get_type_binder_name type_binder) def top_def_map in
      ( top_def_map, inner_def_map, scopes, partials )
    )
    | Declaration_module {module_attr={hidden = true; _} ; _} -> (
      ( top_def_map, inner_def_map, scopes, partials )
    )
    | Declaration_module {module_binder; module_ ; module_attr=_} -> (
      let range = ModVar.get_location module_binder in
      match module_.wrap_content with
      | M_struct m ->
        let (mod_top_env, mod_inner_env, scopes, _) = List.fold_left ~f:declaration ~init:(Def_map.empty, Def_map.empty, scopes, partials) m in
        let mod_env = merge_defs mod_top_env mod_inner_env in
        let def = make_m_def ~range ~body_range:module_.location (get_mod_binder_name module_binder) mod_env in
        let top_def_map = add_shadowing_def (get_mod_binder_name module_binder) def top_def_map in
        ( top_def_map, inner_def_map, scopes, partials )
      | M_variable m -> 
        let alias = [get_mod_binder_name m] in
        let top_def_map = update_module_reference [m] top_def_map in
        let def = make_m_alias_def ~range ~body_range:(ModVar.get_location m) (get_mod_binder_name module_binder) alias in
        let top_def_map = add_shadowing_def (get_mod_binder_name module_binder) def top_def_map in
        ( top_def_map, inner_def_map, scopes, partials )
      | M_module_path m ->
        let path,mods,body_range = List.fold ~init:([],[],Location.dummy) ~f:(fun (path,mods,loc) m -> 
          path@[get_mod_binder_name m], mods@[m], Location.cover loc (ModVar.get_location m)) 
          (List.Ne.to_list m) in
        let top_def_map = update_module_reference mods top_def_map in
        let def = make_m_alias_def ~range ~body_range (get_mod_binder_name module_binder) path in
        let top_def_map = add_shadowing_def (get_mod_binder_name module_binder) def top_def_map in
        ( top_def_map, inner_def_map, scopes, partials )
    )
  in

  let main ~(options:Compiler_options.middle_end) core_prg =
    let init = { type_env = options.init_env ; bindings = Bindings_map.empty } in
    let (top,inner,scopes,_) = List.fold_left ~f:declaration ~init:(Def_map.empty, Def_map.empty, [], init) core_prg in
    let defs = merge_defs top inner in 
    defs, scopes
  in
  let () = Misc.reset_counter () in
  main ~options core_prg

  (* TODO: nested module name B2 or A.B.2 ?? *)
  (* TODO: update schema.json after modifying code *)
  (* TODO: check the body range of module defs *)

  (* TODO: Fix nested module var references *)
  (* TODO: Fix top module var used inside nested module update reference *)