open Types
open Misc

module Formatter = Formatter
module Api_helper = Api_helper

module ModVar = Ast_typed.ModuleVar

type sub_module = { type_env : tenv  ; bindings : bindings_map }

let scopes : with_types:bool -> options:Compiler_options.middle_end -> Ast_core.module_ -> (def_map * scopes) = fun ~with_types ~options core_prg ->
  let make_v_def = make_v_def ~with_types in

  let rec find_scopes' = fun (all_defs,env,scopes,lastloc) (bindings:bindings_map) (e : Ast_core.expression) ->
    match e.expression_content with
    | E_let_in { let_result ; attr= { hidden = true ; _ } ; _ } -> (
      find_scopes' (all_defs,env,scopes,let_result.location) bindings let_result
    )
    | E_let_in { let_binder = {var ; ascr ; attributes=_} ; rhs ; let_result ; attr=_} -> (
      let (all_defs,_, scopes) = find_scopes' (all_defs,env,scopes,e.location) bindings rhs in
      let def = make_v_def bindings var ?core_type:ascr (Ast_core.ValueVar.get_location var) rhs.location in
      let env = add_shadowing_def (get_binder_name var) def env in
      let all_defs = merge_defs env all_defs in
      find_scopes' (all_defs,env,scopes,let_result.location) bindings let_result
    )
    | E_type_in { type_binder; rhs ; let_result } -> (
      let def = make_t_def (get_type_binder_name type_binder) e.location rhs in
      let env = add_shadowing_def (get_type_binder_name type_binder) def env in
      let all_defs = merge_defs env all_defs in
      find_scopes' (all_defs,env,scopes,let_result.location) bindings let_result
    )
    | E_mod_in { module_binder; rhs; let_result } -> (
      match rhs.wrap_content with
      | M_struct m ->
        let (new_outer_def_map,scopes) = module_expr ~options ~env ~scopes rhs in
        let def = make_m_def ~range:e.location ~body_range:rhs.location (get_mod_binder_name module_binder) new_outer_def_map in
        let env = add_shadowing_def (get_mod_binder_name module_binder) def env in
        let all_defs = merge_defs env all_defs in
        find_scopes' (all_defs,env,scopes,let_result.location) bindings let_result
      | M_variable m -> 
        let alias = [get_mod_binder_name m] in
        let def = make_m_alias_def ~range:e.location ~body_range:(ModVar.get_location m) (get_mod_binder_name module_binder) alias in
        let env = add_shadowing_def (get_mod_binder_name module_binder) def env in
        let all_defs = merge_defs env all_defs in
        find_scopes' (all_defs,env,scopes,let_result.location) bindings let_result
      | M_module_path m ->
        let path,body_range = List.fold ~init:([],Location.dummy) ~f:(fun (path,loc) m -> 
          path@[get_mod_binder_name m], Location.cover loc (ModVar.get_location m)) 
          (List.Ne.to_list m) in
        let def = make_m_alias_def ~range:e.location ~body_range (get_mod_binder_name module_binder) path in
        let env = add_shadowing_def (get_mod_binder_name module_binder) def env in
        let all_defs = merge_defs env all_defs in
        find_scopes' (all_defs,env,scopes,let_result.location) bindings let_result
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
      find_scopes' (all_defs,env,scopes,result.location) bindings result
    )
    | E_lambda { binder={var;ascr=input_type; attributes=_} ; output_type = _ ; result } -> (
      let env =
        let def = make_v_def bindings var ?core_type:input_type (Ast_typed.ValueVar.get_location var) result.location in
        add_shadowing_def (get_binder_name var) def env
      in
      let all_defs = merge_defs env all_defs in
      find_scopes' (all_defs,env,scopes,result.location) bindings result
    )
    | E_type_abstraction { type_binder; result } -> (
      let def = make_t_def (get_type_binder_name type_binder) e.location (Ast_core.t_variable type_binder ()) in
      let env = add_shadowing_def (get_type_binder_name type_binder) def env in
      let all_defs = merge_defs env all_defs in
      find_scopes' (all_defs,env,scopes,result.location) bindings result
    )
    | E_matching {matchee; cases} -> (
      let (all_defs,_,scopes) = find_scopes' (all_defs,env,scopes,matchee.location) bindings matchee in
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
        let (all_defs,_,scopes) = find_scopes' (all_defs,env,scopes,body.location) bindings body in
        let all_defs = merge_defs env all_defs in
        (all_defs,scopes)
      in
      let (all_defs,scopes) = List.fold_left ~f:aux ~init:(all_defs,scopes) cases in
      (all_defs,env,scopes)
    )
    | E_record emap -> (
      let aux = fun (all_defs,scopes) (exp:Ast_core.expression) ->
        let (all_defs,_,scopes) = find_scopes' (all_defs,env,scopes,exp.location) bindings exp in
        (all_defs,scopes)
      in
      let (all_defs,scopes) = List.fold_left ~f:aux ~init:(all_defs,scopes) (Ast_core.LMap.to_list emap) in
      (all_defs,env,scopes)
    )
    | E_record_update { record ; update ; _ } -> (
      (*TODO: here record has a virtual location, check this out.. not normal *)
      let (all_defs,_,scopes) = find_scopes' (all_defs,env,scopes,record.location) bindings record in
      find_scopes' (all_defs,env,scopes,update.location) bindings update
    )
    | E_constant { arguments ; _ } -> (
      let aux = fun (all_defs,scopes) (exp:Ast_core.expression) ->
        let (all_defs,_,scopes) = find_scopes' (all_defs,env,scopes,exp.location) bindings exp in
        (all_defs,scopes)
      in
      let (all_defs,scopes) = List.fold_left ~f:aux ~init:(all_defs,scopes) arguments in
      (all_defs,env,scopes)
    )
    | E_application { lamb ; args } -> (
      let (all_defs,_,scopes) = find_scopes' (all_defs,env,scopes,lamb.location) bindings lamb in
      find_scopes' (all_defs,env,scopes,args.location) bindings args
    )
    | E_ascription { anno_expr=e;_ } | E_record_accessor { record=e;_ } | E_constructor { element=e;_ } -> (
      find_scopes' (all_defs,env,scopes,e.location) bindings e
    )
    | E_module_accessor _ -> (
      (* ??? *)
      (* TODOREWORK we should update all_defs so that references to variable accessed are taken into account *)
      (* M.x -> M (resolved module) x (var) *)
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
      (* in case of e_assign I dont think a shadowing def needs to be added ?? *)
      let def = make_v_def bindings binder.var ?core_type:binder.ascr (Ast_typed.ValueVar.get_location binder.var) expression.location in
      let env = add_shadowing_def (get_binder_name binder.var) def env in
      let all_defs = merge_defs env all_defs in
      find_scopes' (all_defs,env,scopes,expression.location) bindings expression
    )
  
  (* What is a scope ? *)

  and find_scopes (top_lvl_defs,scopes,loc) bindings e =
    let (defs,_,scopes) = find_scopes' (top_lvl_defs,top_lvl_defs,scopes,loc) bindings e in
    (defs,scopes)

  and module_expr ~options ~env ~scopes (me: Ast_core.module_expr) =
    match me.wrap_content with
    | M_struct decls -> declaration ~options decls
    | M_module_path path -> (
      let aux (env:def_map) binder : def_map =
        match Def_map.find_opt (get_mod_binder_name binder) env with
        | Some (Module m) -> failwith "TODO"
        | _ -> env
      in
      let def_map = List.fold_left ~f:aux ~init:env (List.Ne.to_list path) in
      (def_map,scopes)
    )
    | M_variable mv -> (
      let env_opt = Def_map.find_opt (get_mod_binder_name mv) env in
      let def_map = match env_opt with
        | Some (Module m) -> failwith "TODO"
        | _ -> Def_map.empty
      in
      (def_map,scopes)
    )

(* Module Alias module A = B.C -> M_variable A, M_path  [B ; C]
   Module Defn module A = struct ... end -> M_variable A, M_struct ...  *)

  and declaration ~options core_prg =
    let compile_declaration ~raise env decl () = Checking.type_declaration ~raise ~options ~env decl in
    let aux = fun (top_def_map,inner_def_map,scopes,partials) (decl : Ast_core.declaration) ->
      let typed_prg =
        (*
          if --with-types optional flag is enabled, we try typing the declaration
          to build a partial Ast_typed program.
          if a declaration do not type, we will still try to type the next one
        *)
        if with_types then Simple_utils.Trace.to_option (compile_declaration partials.type_env decl ())
        else None
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
        let (new_inner_def_map,scopes) = find_scopes (top_def_map,scopes,decl.location) partials.bindings expr in
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
        (* TODO: differentiate between module defn & module alias *)
        let (new_outer_def_map,scopes) = module_expr ~options ~env:top_def_map ~scopes module_ in
        let def = make_m_def ~range:decl.location ~body_range:module_.location (get_mod_binder_name module_binder) new_outer_def_map in
        let top_def_map = Def_map.add (get_mod_binder_name module_binder) def top_def_map in
        ( top_def_map, inner_def_map, scopes, partials )
      )
    in
    let init = { type_env = options.init_env ; bindings = Bindings_map.empty } in
    let (top,inner,scopes,_) = List.fold_left ~f:aux ~init:(Def_map.empty, Def_map.empty, [], init) core_prg in
    let defs = merge_defs top inner in 
    defs, scopes
  in
  let () = Misc.reset_counter () in
  declaration ~options core_prg

  (* TODO: nested module name B2 or A.B.2 ?? *)
  (* TODO: update schema.json after modifying code *)