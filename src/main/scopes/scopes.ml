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
    | E_let_in { let_binder = (fn,_) ; rhs ; let_result } -> (
      match rhs.content with
      | E_recursive { fun_name ;  fun_type ; lambda = { result;_ } } -> (
        (* Note:
            It is not entirely true that 'fun_name' is in 'result' scope; because only tail calls are allowed
        *)
        let def = make_v_def_option_type fun_name (Some fun_type) e.location e.location in
        let (i,env) = add_shadowing_def (i,fun_name) def env in
        find_scopes' (i,all_defs,env,scopes,result.location) result
      )
      | _ -> (
        (*TODO : n needs location and should be  used bellow in union with rhs *)
        let (i,all_defs,_, scopes) = find_scopes' (i,all_defs,env,scopes,e.location) rhs in
        let (i,env) = add_shadowing_def (i,fn) (make_v_def_from_core fn rhs rhs.location rhs.location) env in
        let all_defs = merge_defs env all_defs in
        find_scopes' (i,all_defs,env,scopes,let_result.location) let_result
      )
    )
    | E_lambda { binder ; input_type ; output_type = _ ; result } -> (
      let (i,env) = add_shadowing_def (i,binder) (make_v_def_option_type binder input_type result.location result.location) env in
      let all_defs = merge_defs env all_defs in
      find_scopes' (i,all_defs,env,scopes,result.location) result
    )
    | E_matching {matchee; cases} -> (
      let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,matchee.location) matchee in
      match cases with
      | Match_list { match_nil ; match_cons = (hd , tl , match_cons) } -> (
        let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,match_nil.location) match_nil in
        let all_defs = merge_defs env all_defs in
        (* TODO hd and tl needs location and should be used bellow instead of match_cons .. *)

        let list_f = fun (t:Ast_typed.type_expression) -> match Ast_typed.get_t_list t with
          | None -> failwith "Could not get the type of a list"
          | Some t -> t in
        let hd_def = make_v_def_ppx_type hd list_f matchee match_cons.location match_cons.location in
        let tl_def = make_v_def_from_core tl matchee match_cons.location match_cons.location in

        let (i,env) = add_shadowing_def (i,hd) hd_def env in
        let (i,env) = add_shadowing_def (i,tl) tl_def env in

        let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,match_cons.location) match_cons in
        let all_defs = merge_defs env all_defs in
        (i,all_defs,env,scopes)
      )
      | Match_option { match_none ; match_some = (some , match_some) } -> (
        let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,match_none.location) match_none in
        let all_defs = merge_defs env all_defs in
        (* TODO some needs location and should be used bellow instead of match_some .. *)

        let tl_def = make_v_def_from_core some matchee match_some.location match_some.location in
        let (i,env) = add_shadowing_def (i,some) tl_def env in

        let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,match_some.location) match_some in
        let all_defs = merge_defs env all_defs in
        (i,all_defs,env,scopes)
      )
      | Match_variant lst -> (
        let aux = fun (i,all_defs,scopes) ((c,proj),(match_variant:Ast_core.expression)) ->
          let proj_f = fun (t:Ast_typed.type_expression) -> match Ast_typed.get_t_sum t with
            | Some t -> (Ast_typed.CMap.find (Ast_typed.Environment.convert_constructor' c) t).ctor_type
            | None -> failwith "Could not get the inner type of a constructor" in

          (* TODO proj needs location and should be used bellow instead of match_variant .. *)
          let proj_def = make_v_def_ppx_type proj proj_f matchee match_variant.location match_variant.location in
          let (i,env) = add_shadowing_def (i,proj) proj_def env in
          let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,match_variant.location) match_variant in
          let all_defs = merge_defs env all_defs in
          (i,all_defs,scopes)
        in
        let (i,all_defs,scopes) = List.fold_left aux (i,all_defs,scopes) lst in
        (i,all_defs,env,scopes)
      )
    )
    | E_ascription { anno_expr ; _ } -> find_scopes' (i,all_defs,env,scopes,anno_expr.location) anno_expr
    | _ ->
      let scopes = add_scope (lastloc, env) scopes in
      (i,all_defs,env,scopes)
  in
  let find_scopes (i,top_lvl_defs,scopes,loc) e =
    let (i,defs,_,scopes) = find_scopes' (i,top_lvl_defs,top_lvl_defs,scopes,loc) e in 
    (i,defs,scopes) in

  let aux = fun (i,top_def_map,inner_def_map,scopes) (x : Ast_core.declaration Location.wrap) ->
    match x.wrap_content with
    | Declaration_constant (v , _o , _i, e) ->
      let (i,inner_def_map,scopes) = find_scopes (i,top_def_map,scopes,x.location) e in
      let def = make_v_def_from_core v e x.location e.location in
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