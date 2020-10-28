open Trace
open Types
open Misc

module Formatter = Formatter

type tstate = Typer_common.Errors.typer_error Typer.O'.typer_state
type tenv = Ast_typed.environment
type sub_program = { program : Ast_core.program  ; bindings : bindings_map }

let scopes : with_types:bool -> Ast_typed.environment -> Ast_core.program -> ((def_map * scopes), Main_errors.all) result = fun ~with_types init_env core_prg ->
  let make_v_def_from_core = make_v_def_from_core ~with_types  in
  let make_v_def_option_type = make_v_def_option_type ~with_types in

  let rec find_scopes' = fun (i,all_defs,env,scopes,lastloc) (bindings:bindings_map) (e : Ast_core.expression) ->
    match e.content with
    | E_let_in { let_binder = {var ; ascr} ; rhs ; let_result } -> (
      let (i,all_defs,_, scopes) = find_scopes' (i,all_defs,env,scopes,e.location) bindings rhs in
      let def = make_v_def_option_type bindings var ascr var.location rhs.location in
      let (i,env) = add_shadowing_def (i,var.wrap_content) def env in
      let all_defs = merge_defs env all_defs in
      find_scopes' (i,all_defs,env,scopes,let_result.location) bindings let_result
    )
    | E_recursive { fun_name ; fun_type ; lambda = { result ; _ } } -> (
      let def = make_v_def_option_type bindings fun_name (Some fun_type) fun_name.location result.location in
      let (i,env) = add_shadowing_def (i,fun_name.wrap_content) def env in
      find_scopes' (i,all_defs,env,scopes,result.location) bindings result
    )
    | E_lambda { binder={var;ascr=input_type} ; output_type = _ ; result } -> (
      let def = make_v_def_option_type bindings var input_type var.location result.location in
      let (i,env) = add_shadowing_def (i,var.wrap_content) def env in
      let all_defs = merge_defs env all_defs in
      find_scopes' (i,all_defs,env,scopes,result.location) bindings result
    )
    | E_matching {matchee; cases} -> (
      let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,matchee.location) bindings matchee in
      match cases with
      | Match_list { match_nil ; match_cons = { hd ; tl ; body }} -> (
        let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,match_nil.location) bindings match_nil in
        let all_defs = merge_defs env all_defs in
        let hd_def = make_v_def_from_core bindings hd hd.location hd.location in
        let tl_def = make_v_def_from_core bindings tl tl.location tl.location in
        let (i,env) = add_shadowing_def (i,hd.wrap_content) hd_def env in
        let (i,env) = add_shadowing_def (i,tl.wrap_content) tl_def env in
        let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,body.location) bindings body in
        let all_defs = merge_defs env all_defs in
        (i,all_defs,env,scopes)
      )
      | Match_option { match_none ; match_some = {opt ; body } } -> (
        let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,match_none.location) bindings match_none in
        let all_defs = merge_defs env all_defs in
        let def = make_v_def_from_core bindings opt opt.location opt.location in
        let (i,env) = add_shadowing_def (i,opt.wrap_content) def env in
        let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,body.location) bindings body in
        let all_defs = merge_defs env all_defs in
        (i,all_defs,env,scopes)
      )
      | Match_variant lst -> (
        let aux = fun (i,all_defs,scopes) ({constructor=_;proj;body}:Ast_core.match_variant) ->
          let proj_def = make_v_def_from_core bindings proj proj.location proj.location in
          let (i,env) = add_shadowing_def (i,proj.wrap_content) proj_def env in
          let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,body.location) bindings body in
          let all_defs = merge_defs env all_defs in
          (i,all_defs,scopes)
        in
        let (i,all_defs,scopes) = List.fold_left aux (i,all_defs,scopes) lst in
        (i,all_defs,env,scopes)
      )
    )
    | E_record emap -> (
      let aux = fun (i,all_defs,scopes) (exp:Ast_core.expression) ->
        let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,exp.location) bindings exp in
        (i,all_defs,scopes)
      in
      let (i,all_defs,scopes) = List.fold_left aux (i,all_defs,scopes) (Ast_core.LMap.to_list emap) in
      (i,all_defs,env,scopes)
    )
    | E_record_update { record ; update ; _ } -> (
      (*TODO: here record has a virtual location, check this out.. not normal *)
      let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,record.location) bindings record in
      find_scopes' (i,all_defs,env,scopes,update.location) bindings update
    )
    | E_constant { arguments ; _ } -> (
      let aux = fun (i,all_defs,scopes) (exp:Ast_core.expression) ->
        let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,exp.location) bindings exp in
        (i,all_defs,scopes)
      in
      let (i,all_defs,scopes) = List.fold_left aux (i,all_defs,scopes) arguments in
      (i,all_defs,env,scopes)
    )
    | E_application { lamb ; args } -> (
      let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,lamb.location) bindings lamb in
      find_scopes' (i,all_defs,env,scopes,args.location) bindings args
    )
    | E_ascription { anno_expr=e;_ } | E_record_accessor { record=e;_ } | E_constructor { element=e;_ } -> (
      find_scopes' (i,all_defs,env,scopes,e.location) bindings e
    )
    | E_literal _ | E_raw_code _ | E_variable _ -> (
      let scopes = add_scope (lastloc, env) scopes in
      (i,all_defs,env,scopes)
    )
  in
  let find_scopes (i,top_lvl_defs,scopes,loc) bindings  e =
    let (i,defs,_,scopes) = find_scopes' (i,top_lvl_defs,top_lvl_defs,scopes,loc) bindings e in
    (i,defs,scopes) in

  let aux = fun (i,top_def_map,inner_def_map,scopes,sub_prg) (x : Ast_core.declaration Location.wrap) ->
    let program = List.append sub_prg.program [x] in
    let typed_prg = if with_types then Trace.to_option @@ Compile.Of_core.compile ~init_env Env program else None in
    let bindings = match typed_prg with
      | Some (typed_prg,_,_) -> extract_variable_types sub_prg.bindings typed_prg
      | None -> sub_prg.bindings
    in
    let sub_prg' = { program ; bindings } in
    match x.wrap_content with
    | Declaration_constant { binder= { var ; ascr } ; expr ; _ } -> (
      let (i,new_inner_def_map,scopes) = find_scopes (i,top_def_map,scopes,x.location) bindings expr in
      let inner_def_map = merge_defs new_inner_def_map inner_def_map in
      let def = make_v_def_option_type bindings var ascr var.location expr.location in
      let (i,top_def_map) = add_shadowing_def (i,var.wrap_content) def top_def_map in
      ( i, top_def_map, inner_def_map, scopes , sub_prg' )
    )
    | Declaration_type {type_binder; type_expr} -> (
      let def = make_t_def (get_binder_name type_binder) x type_expr in
      let (i,top_def_map) = add_shadowing_def (i,type_binder) def top_def_map in
      ( i, top_def_map, inner_def_map, scopes, sub_prg' )
    )
  in

  let init = { program = [] ; bindings = Bindings_map.empty } in
  let (_,top_d,inner_d,s,_) = List.fold_left aux (0, Def_map.empty, Def_map.empty, [], init) core_prg in
  let d = Def_map.union (fun _ outter _ -> Some outter) top_d inner_d in
  ok (d,s)
