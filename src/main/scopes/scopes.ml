open Trace
open Types
open Misc

module Formatter = Formatter

let scopes : with_types:bool -> string -> string -> ((def_map * scopes), Main_errors.all) result = fun ~with_types source_file syntax ->
  let make_v_def_from_core = make_v_def_from_core ~with_types source_file syntax in
  let make_v_def_option_type = make_v_def_option_type ~with_types source_file syntax in
  let make_v_def_ppx_type = make_v_def_ppx_type ~with_types source_file syntax in 

  let rec find_scopes' = fun (i,all_defs,env,scopes,lastloc) (e : Ast_core.expression) ->
    match e.content with
    | E_let_in { let_binder = ({wrap_content=fn;location=fn_loc},_) ; rhs ; let_result } -> (
      let (i,all_defs,_, scopes) = find_scopes' (i,all_defs,env,scopes,e.location) rhs in
      let (i,env) = add_shadowing_def (i,fn) (make_v_def_from_core fn rhs fn_loc rhs.location) env in
      let all_defs = merge_defs env all_defs in
      find_scopes' (i,all_defs,env,scopes,let_result.location) let_result
    )
    | E_recursive { fun_name={wrap_content=fn;location=fn_loc} ;  fun_type ; lambda = { result;_ } } -> (
      (* Note:
          It is not entirely true that 'fun_name' is in 'result' scope; because only tail calls are allowed
      *)
      let def = make_v_def_option_type fn (Some fun_type) fn_loc result.location in
      let (i,env) = add_shadowing_def (i,fn) def env in
      find_scopes' (i,all_defs,env,scopes,result.location) result
    )
    | E_lambda { binder={wrap_content=fun_name;location=fn_loc} ; input_type ; output_type = _ ; result } -> (
      let (i,env) = add_shadowing_def (i,fun_name) (make_v_def_option_type fun_name input_type fn_loc result.location) env in
      let all_defs = merge_defs env all_defs in
      find_scopes' (i,all_defs,env,scopes,result.location) result
    )
    | E_matching {matchee; cases} -> (
      let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,matchee.location) matchee in
      match cases with
      | Match_list { match_nil ; match_cons = ({wrap_content=hd;location=hd_loc} , {wrap_content=tl;location=tl_loc} , match_cons) } -> (
        let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,match_nil.location) match_nil in
        let all_defs = merge_defs env all_defs in

        let list_f = fun (t:Ast_typed.type_expression) -> match Ast_typed.get_t_list t with
          | None -> failwith "Could not get the type of a list"
          | Some t -> t in
        let hd_def = make_v_def_ppx_type hd list_f matchee hd_loc hd_loc in
        let tl_def = make_v_def_from_core tl matchee tl_loc tl_loc in

        let (i,env) = add_shadowing_def (i,hd) hd_def env in
        let (i,env) = add_shadowing_def (i,tl) tl_def env in

        let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,match_cons.location) match_cons in
        let all_defs = merge_defs env all_defs in
        (i,all_defs,env,scopes)
      )
      | Match_option { match_none ; match_some = ({wrap_content=some;location=some_loc} , match_some) } -> (
        let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,match_none.location) match_none in
        let all_defs = merge_defs env all_defs in

        let tl_def = make_v_def_from_core some matchee some_loc some_loc in
        let (i,env) = add_shadowing_def (i,some) tl_def env in

        let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,match_some.location) match_some in
        let all_defs = merge_defs env all_defs in
        (i,all_defs,env,scopes)
      )
      | Match_variant lst -> (
        let aux = fun (i,all_defs,scopes) ((c,(proj:Ast_core.expression_variable)),(match_variant:Ast_core.expression)) ->
          let proj_f = fun (t:Ast_typed.type_expression) -> match Ast_typed.get_t_sum t with
            | Some t -> (Ast_typed.CMap.find (Ast_typed.Environment.convert_constructor' c) t).ctor_type
            | None -> failwith "Could not get the inner type of a constructor" in

          let proj_def = make_v_def_ppx_type proj.wrap_content proj_f matchee proj.location proj.location in
          let (i,env) = add_shadowing_def (i,proj.wrap_content) proj_def env in
          let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,match_variant.location) match_variant in
          let all_defs = merge_defs env all_defs in
          (i,all_defs,scopes)
        in
        let (i,all_defs,scopes) = List.fold_left aux (i,all_defs,scopes) lst in
        (i,all_defs,env,scopes)
      )
    )
    | E_record emap -> (
      let aux = fun (i,all_defs,scopes) (exp:Ast_core.expression) -> 
        let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,exp.location) exp in
        (i,all_defs,scopes)
      in
      let (i,all_defs,scopes) = List.fold_left aux (i,all_defs,scopes) (Ast_core.LMap.to_list emap) in
      (i,all_defs,env,scopes)
    )
    | E_record_update { record ; update ; _ } -> (
      (*TODO: here record has a virtual location, check this out.. not normal *)
      let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,record.location) record in
      find_scopes' (i,all_defs,env,scopes,update.location) update
    )
    | E_constant { arguments ; _ } -> (
      let aux = fun (i,all_defs,scopes) (exp:Ast_core.expression) -> 
        let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,exp.location) exp in
        (i,all_defs,scopes)
      in
      let (i,all_defs,scopes) = List.fold_left aux (i,all_defs,scopes) arguments in
      (i,all_defs,env,scopes)
    )
    | E_application { lamb ; args } -> (
      let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,lamb.location) lamb in
      find_scopes' (i,all_defs,env,scopes,args.location) args
    )
    | E_ascription { anno_expr=e;_ } | E_record_accessor { record=e;_ } | E_constructor { element=e;_ } -> (
      find_scopes' (i,all_defs,env,scopes,e.location) e
    )
    | E_literal _ | E_raw_code _ | E_variable _ -> (
      let scopes = add_scope (lastloc, env) scopes in
      (i,all_defs,env,scopes)
    )
  in
  let find_scopes (i,top_lvl_defs,scopes,loc) e =
    let (i,defs,_,scopes) = find_scopes' (i,top_lvl_defs,top_lvl_defs,scopes,loc) e in 
    (i,defs,scopes) in

  let aux = fun (i,top_def_map,inner_def_map,scopes) (x : Ast_core.declaration Location.wrap) ->
    match x.wrap_content with
    | Declaration_constant ({wrap_content=v;location=v_loc} , _o , _i, e) ->
      let (i,new_inner_def_map,scopes) = find_scopes (i,top_def_map,scopes,x.location) e in
      let inner_def_map = merge_defs new_inner_def_map inner_def_map in
      let def = make_v_def_from_core v e v_loc e.location in
      let (i,top_def_map) = add_shadowing_def (i,v) def top_def_map in
      ( i, top_def_map, inner_def_map, scopes )

    | Declaration_type (tv, te) ->
      let def = make_t_def (get_binder_name tv) x te in
      let (i,top_def_map) = add_shadowing_def (i,tv) def top_def_map in
      ( i, top_def_map, inner_def_map, scopes )

  in

  let%bind (core_prg : Ast_core.program) = Compile.Utils.to_core source_file syntax in
  let (_,top_d,inner_d,s) = List.fold_left aux (0, Def_map.empty ,Def_map.empty, []) core_prg in
  let d = Def_map.union (fun _ outter _ -> Some outter) top_d inner_d in
  ok (d,s)