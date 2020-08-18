module Errors = Errors
module I = Ast_imperative
module O = Ast_sugar
open Trace

let compare_var = Location.compare_content ~compare:Var.compare

let rec add_to_end (expression: O.expression) to_add =
  match expression.expression_content with
  | O.E_let_in lt -> 
    let lt = {lt with let_result = add_to_end lt.let_result to_add} in
    {expression with expression_content = O.E_let_in lt}
  | O.E_sequence seq -> 
    let seq = {seq with expr2 = add_to_end seq.expr2 to_add} in
    {expression with expression_content = O.E_sequence seq}
  | _ -> O.e_sequence expression to_add

let repair_mutable_variable_in_matching (match_body : O.expression) (element_names : O.expression_variable list) (env : I.expression_variable) =
  let%bind ((dv,fv),mb) = Self_ast_sugar.fold_map_expression
    (* TODO : these should use Variables sets *)
    (fun (decl_var,free_var : O.expression_variable list * O.expression_variable list) (ass_exp : O.expression) ->
      match ass_exp.expression_content with
        | E_let_in {let_binder;mut=false;rhs;let_result} ->
          let (name,_) = let_binder in
          ok (true,(name::decl_var, free_var),O.e_let_in let_binder false false rhs let_result)
        | E_let_in {let_binder;mut=true; rhs;let_result} ->
          let (name,_) = let_binder in
          if List.mem ~compare:compare_var name decl_var then
            ok (true,(decl_var, free_var), O.e_let_in let_binder false false rhs let_result)
          else(
            let free_var = if (List.mem ~compare:compare_var name free_var) then free_var else name::free_var in
            let expr = O.e_let_in (env,O.t_wildcard ()) false false (O.e_update (O.e_variable env) [O.Access_record (Var.to_name name.wrap_content)] (O.e_variable name)) let_result in
            ok (true,(decl_var, free_var), O.e_let_in let_binder false  false rhs expr)
          )
        | E_constant {cons_name=C_MAP_FOLD;arguments= _}
        | E_constant {cons_name=C_SET_FOLD;arguments= _}
        | E_constant {cons_name=C_LIST_FOLD;arguments= _} 
        | E_cond _
        | E_matching _ -> ok @@ (false, (decl_var,free_var),ass_exp)
      | E_constant _
      | E_skip
      | E_literal _ | E_variable _
      | E_application _ | E_lambda _| E_recursive _ | E_raw_code _
      | E_constructor _ | E_record _| E_accessor _|E_update _
      | E_ascription _  | E_sequence _ | E_tuple _
      | E_map _ | E_big_map _ |E_list _ | E_set _
       -> ok (true, (decl_var, free_var),ass_exp)
    )
      (element_names,[])
      match_body in
  ok @@ ((dv,fv),mb)

and repair_mutable_variable_in_loops (for_body : O.expression) (element_names : O.expression_variable list) (env : O.expression_variable) =
  let%bind ((dv,fv),fb) = Self_ast_sugar.fold_map_expression
    (* TODO : these should use Variables sets *)
    (fun (decl_var,free_var : O.expression_variable list * O.expression_variable list) (ass_exp : O.expression) ->
      (* Format.printf "debug: dv:%a; fv:%a; expr:%a \n%!" 
        (I.PP.list_sep_d I.PP.expression_variable) decl_var
        (I.PP.list_sep_d I.PP.expression_variable) decl_var
        O.PP.expression ass_exp
      ;*)
      match ass_exp.expression_content with
        | E_let_in {let_binder;mut=false;} ->
          let (name,_) = let_binder in
          ok (true,(name::decl_var, free_var),ass_exp)
        | E_let_in {let_binder;mut=true; rhs;let_result} ->
          let (name,_) = let_binder in
          if List.mem ~compare:compare_var name decl_var then
            ok (true,(decl_var, free_var), O.e_let_in let_binder false false rhs let_result)
          else(
            let free_var =
              if (List.mem ~compare:compare_var name free_var)
              then free_var
              else name::free_var in
            let expr = O.e_let_in (env,O.t_wildcard ()) false false (
              O.e_update (O.e_variable env) [O.Access_tuple Z.zero; O.Access_record (Var.to_name name.wrap_content)] (O.e_variable name)
              )
              let_result in
            ok (true,(decl_var, free_var), O.e_let_in let_binder false  false rhs expr)
          )
        | E_constant {cons_name=C_MAP_FOLD;arguments= _}
        | E_constant {cons_name=C_SET_FOLD;arguments= _}
        | E_constant {cons_name=C_LIST_FOLD;arguments= _} 
        | E_cond _
        | E_matching _ -> ok @@ (false,(decl_var,free_var),ass_exp)
      | E_constant _
      | E_skip
      | E_literal _ | E_variable _
      | E_application _ | E_lambda _| E_recursive _ | E_raw_code _
      | E_constructor _ | E_record _| E_accessor _| E_update _
      | E_ascription _  | E_sequence _ | E_tuple _
      | E_map _ | E_big_map _ |E_list _ | E_set _
       -> ok (true, (decl_var, free_var),ass_exp)
    )
      (element_names,[])
      for_body in
  ok @@ ((dv,fv),fb)

and store_mutable_variable (free_vars : I.expression_variable list) =
  if (List.length free_vars == 0) then
    O.e_unit ()
  else
    let aux (var:I.expression_variable) = (O.Label (Var.to_name var.wrap_content), O.e_variable var) in
    O.e_record @@ O.LMap.of_list (List.map aux free_vars)
 
and restore_mutable_variable (expr : O.expression->O.expression) (free_vars : O.expression_variable list) (env : O.expression_variable) =
  let aux (f: O.expression -> O.expression) (ev: O.expression_variable) =
    fun expr -> f (O.e_let_in (ev,O.t_wildcard ()) true false (O.e_accessor (O.e_variable env) [O.Access_record (Var.to_name ev.wrap_content)]) expr)
  in
  let ef = List.fold_left aux (fun e -> e) free_vars in
  fun e -> match e with 
    | None -> expr (ef (O.e_skip ()))
    | Some e -> expr (ef e)


let rec compile_type_expression : I.type_expression -> (O.type_expression,Errors.purification_error) result =
  fun te ->
  let return tc = ok @@ O.make_t ~loc:te.location tc in
  match te.type_content with
    | I.T_sum sum -> 
      let%bind sum = 
        Stage_common.Helpers.bind_map_lmap (fun (({associated_type = v; decl_pos ; _}:I.row_element)) ->
          let%bind v = compile_type_expression v in
          let content : O.row_element = {associated_type = v ; michelson_annotation = None ; decl_pos } in
          ok @@ content
        ) sum
      in
      return @@ O.T_sum sum
    | I.T_record record -> 
      let%bind record = 
        Stage_common.Helpers.bind_map_lmap (fun (({associated_type = v; decl_pos ; _}:I.row_element)) ->
          let%bind v = compile_type_expression v in
          let content : O.row_element = {associated_type = v; michelson_annotation = None ; decl_pos} in
          ok @@ content
        ) record
      in
      return @@ O.T_record record
    | I.T_tuple tuple ->
      let%bind tuple = bind_map_list compile_type_expression tuple in
      return @@ O.T_tuple tuple
    | I.T_arrow {type1;type2} ->
      let%bind type1 = compile_type_expression type1 in
      let%bind type2 = compile_type_expression type2 in
      return @@ T_arrow {type1;type2}
    | I.T_variable type_variable -> return @@ T_variable type_variable 
    | I.T_wildcard               -> return @@ T_wildcard
    | I.T_constant (TC_michelson_or, [l;r]) ->
      let%bind (l, l_ann) = trace_option (Errors.corner_case "not an annotated type") @@ I.get_t_annoted l in
      let%bind (r, r_ann) = trace_option (Errors.corner_case "not an annotated type") @@ I.get_t_annoted r in
      let%bind (l,r) = bind_map_pair compile_type_expression (l,r) in
      let sum : (O.label * O.row_element) list = [
        (O.Label "M_left" , {associated_type = l ; michelson_annotation = Some l_ann ; decl_pos = 0}); 
        (O.Label "M_right", {associated_type = r ; michelson_annotation = Some r_ann ; decl_pos = 1}); ]
      in
      return @@ O.T_sum (O.LMap.of_list sum)
    | I.T_constant (TC_michelson_pair, [l;r]) ->
      let%bind (l, l_ann) = trace_option (Errors.corner_case "not an annotated type") @@ I.get_t_annoted l in
      let%bind (r, r_ann) = trace_option (Errors.corner_case "not an annotated type") @@ I.get_t_annoted r in
      let%bind (l,r) = bind_map_pair compile_type_expression (l,r) in
      let sum : (O.label * O.row_element) list = [
        (O.Label "0" , {associated_type = l ; michelson_annotation = Some l_ann ; decl_pos = 0}); 
        (O.Label "1", {associated_type = r ; michelson_annotation = Some r_ann ; decl_pos = 0}); ]
      in
      return @@ O.T_record (O.LMap.of_list sum)
    | I.T_constant (type_constant, lst) ->
      let%bind lst = bind_map_list compile_type_expression lst in
      return @@ T_constant (type_constant, lst)
    | I.T_annoted (ty, _) -> compile_type_expression ty

let rec compile_expression : I.expression -> (O.expression , _) result =
  fun e ->
  let%bind e = compile_expression' e in
  ok @@ e None

and compile_expression' : I.expression -> (O.expression option -> O.expression, Errors.purification_error) result =
  fun e ->
  let return expr = ok @@ function
    | None -> expr 
    | Some e -> O.e_sequence expr e   
  in
  let loc = e.location in
  match e.expression_content with
    | I.E_literal literal   -> return @@ O.e_literal ~loc literal
    | I.E_constant {cons_name;arguments} -> 
      let%bind arguments = bind_map_list compile_expression arguments in
      return @@ O.e_constant ~loc cons_name arguments
    | I.E_variable name     -> return @@ O.e_variable ~loc name
    | I.E_application {lamb;args} -> 
      let%bind lamb = compile_expression lamb in
      let%bind args = compile_expression args in
      return @@ O.e_application ~loc lamb args
    | I.E_lambda lambda ->
      let%bind lambda = compile_lambda lambda in
      return @@ O.make_e ~loc (O.E_lambda lambda)
    | I.E_recursive {fun_name;fun_type;lambda} ->
      let%bind fun_type = compile_type_expression fun_type in
      let%bind lambda = compile_lambda lambda in
      return @@ O.e_recursive ~loc fun_name fun_type lambda
    | I.E_let_in {let_binder;inline;rhs;let_result} ->
      let%bind binder = compile_binder let_binder in
      let%bind rhs = compile_expression rhs in
      let%bind let_result = compile_expression let_result in
      return @@ O.e_let_in ~loc binder false inline rhs let_result
    | I.E_raw_code {language;code} ->
      let%bind code = compile_expression code in
      return @@ O.e_raw_code ~loc language code 
    | I.E_constructor {constructor;element} ->
      let%bind element = compile_expression element in
      return @@ O.e_constructor ~loc constructor element
    | I.E_matching m ->
      let%bind m = compile_matching m loc in
      ok @@ m 
    | I.E_record record ->
      let record = I.LMap.to_kv_list record in
      let%bind record = 
        bind_map_list (fun (k,v) ->
          let%bind v = compile_expression v in
          ok @@ (k,v)
        ) record
      in
      return @@ O.e_record ~loc (O.LMap.of_list record)
    | I.E_accessor {record;path} ->
      let%bind record = compile_expression record in
      let%bind path   = compile_path path in
      return @@ O.e_accessor ~loc record path
    | I.E_update {record;path;update} ->
      let%bind record = compile_expression record in
      let%bind path   = compile_path path in
      let%bind update = compile_expression update in
      return @@ O.e_update ~loc record path update
    | I.E_map map ->
      let%bind map = bind_map_list (
        bind_map_pair compile_expression
      ) map
      in
      return @@ O.e_map ~loc map
    | I.E_big_map big_map ->
      let%bind big_map = bind_map_list (
        bind_map_pair compile_expression
      ) big_map
      in
      return @@ O.e_big_map ~loc big_map
    | I.E_list lst ->
      let%bind lst = bind_map_list compile_expression lst in
      return @@ O.e_list ~loc lst
    | I.E_set set ->
      let%bind set = bind_map_list compile_expression set in
      return @@ O.e_set ~loc set 
    | I.E_ascription {anno_expr; type_annotation} ->
      let%bind anno_expr = compile_expression anno_expr in
      let%bind type_annotation = compile_type_expression type_annotation in
      return @@ O.e_annotation ~loc anno_expr type_annotation
    | I.E_cond {condition;then_clause;else_clause} ->
      let%bind condition    = compile_expression condition in
      let%bind then_clause' = compile_expression then_clause in
      let%bind else_clause' = compile_expression else_clause in
      let env = Location.wrap (Var.fresh ~name:"env" ()) in
      let%bind ((_,free_vars_true), then_clause) = repair_mutable_variable_in_matching then_clause' [] env in
      let%bind ((_,free_vars_false), else_clause) = repair_mutable_variable_in_matching else_clause' [] env in
      let then_clause  = add_to_end then_clause (O.e_variable env) in
      let else_clause = add_to_end else_clause (O.e_variable env) in

      let free_vars = List.sort_uniq compare_var @@ free_vars_true @ free_vars_false in
      if (List.length free_vars != 0) then 
        let cond_expr  = O.e_cond condition then_clause else_clause in
        let return_expr = fun expr ->
          O.e_let_in (env,O.t_wildcard ()) false false (store_mutable_variable free_vars) @@
          O.e_let_in (env,O.t_wildcard ()) false false cond_expr @@
          expr 
        in
        ok @@ restore_mutable_variable return_expr free_vars env
      else
        return @@ O.e_cond ~loc condition then_clause' else_clause'
    | I.E_sequence {expr1; expr2} ->
      let%bind expr1 = compile_expression' expr1 in
      let%bind expr2 = compile_expression' expr2 in
      ok @@ fun e -> (match e with 
        | None ->  expr1 (Some (expr2 None))
        | Some e -> expr1 (Some (expr2 (Some e)))
        )
    | I.E_skip -> return @@ O.e_skip ~loc ()
    | I.E_tuple tuple ->
      let%bind tuple = bind_map_list compile_expression tuple in
      return @@ O.e_tuple ~loc tuple
    | I.E_assign {variable; access_path; expression} ->
      let%bind access_path = compile_path access_path in
      let%bind expression = compile_expression expression in
      let rhs = match access_path with
        [] -> expression
      | _  -> O.e_update ~loc (O.e_variable ~loc variable) access_path expression in
      ok @@ fun expr -> (match expr with 
       | None   -> O.e_let_in ~loc (variable,O.t_wildcard ()) true false rhs (O.e_skip ())
       | Some e -> O.e_let_in ~loc (variable,O.t_wildcard ()) true false rhs e
      )
    | I.E_for f -> 
      let%bind f = compile_for f in
      ok @@ f
    | I.E_for_each fe -> 
      let%bind fe = compile_for_each fe in
      ok @@ fe
    | I.E_while w ->
      let%bind w = compile_while w in
      ok @@ w

and compile_binder : _ I.binder -> _ result = fun (var, ty) ->
  let%bind ty = compile_type_expression ty in
  ok @@ (var,ty)

and compile_path : I.access list -> (O.access list, Errors.purification_error) result =
  fun path ->
  let aux a = match a with
    | I.Access_record s -> ok @@ O.Access_record s
    | I.Access_tuple  i -> ok @@ O.Access_tuple  i
    | I.Access_map e ->
      let%bind e = compile_expression e in
      ok @@ O.Access_map e
  in
  bind_map_list aux path

and compile_lambda : I.lambda -> (O.lambda, _) result =
  fun {binder;result}->
    let%bind binder = compile_binder binder in
    let%bind result = compile_expression result in
    ok @@ O.{binder;result}

and compile_matching : I.matching -> Location.t -> (O.expression option -> O.expression, Errors.purification_error) result =
  fun {matchee;cases} loc ->
  let return expr = ok @@ function
    | None -> expr 
    | Some e -> O.e_sequence expr e   
  in
  let%bind matchee = compile_expression matchee in
  match cases with 
    | I.Match_option {match_none;match_some} ->
      let%bind match_none' = compile_expression match_none in
      let (n,expr) = match_some in
      let%bind expr' = compile_expression expr in
      let env = Location.wrap (Var.fresh ~name:"env" ()) in
      let%bind ((_,free_vars_none), match_none) = repair_mutable_variable_in_matching match_none' [] env in
      let%bind ((_,free_vars_some), expr) = repair_mutable_variable_in_matching expr' [n] env in
      let match_none = add_to_end match_none (O.e_variable env) in
      let expr       = add_to_end expr (O.e_variable env) in
      let free_vars = List.sort_uniq compare_var @@ free_vars_none @ free_vars_some in
      if (List.length free_vars != 0) then
        let match_expr  = O.e_matching matchee (O.Match_option {match_none; match_some=(n,expr)}) in
        let return_expr = fun expr ->
          O.e_let_in (env,O.t_wildcard ()) false false (store_mutable_variable free_vars) @@
          O.e_let_in (env,O.t_wildcard ()) false false match_expr @@
          expr 
        in
        ok @@ restore_mutable_variable return_expr free_vars env
      else
        return @@ O.e_matching ~loc matchee @@ O.Match_option {match_none=match_none'; match_some=(n,expr')}
    | I.Match_list {match_nil;match_cons} ->
      let%bind match_nil' = compile_expression match_nil in
      let (hd,tl,expr) = match_cons in
      let%bind expr' = compile_expression expr in
      let env = Location.wrap (Var.fresh ~name:"name" ()) in
      let%bind ((_,free_vars_nil), match_nil) = repair_mutable_variable_in_matching match_nil' [] env in
      let%bind ((_,free_vars_cons), expr) = repair_mutable_variable_in_matching expr' [hd;tl] env in
      let match_nil = add_to_end match_nil (O.e_variable env) in
      let expr      = add_to_end expr (O.e_variable env) in
      let free_vars = List.sort_uniq compare_var @@ free_vars_nil @ free_vars_cons in
      if (List.length free_vars != 0) then
        let match_expr  = O.e_matching matchee (O.Match_list {match_nil; match_cons=(hd,tl,expr)}) in
        let return_expr = fun expr ->
          O.e_let_in (env,O.t_wildcard ()) false false (store_mutable_variable free_vars) @@
          O.e_let_in (env,O.t_wildcard ()) false false match_expr @@
          expr 
        in
        ok @@ restore_mutable_variable return_expr free_vars env
      else
        return @@ O.e_matching ~loc matchee @@ O.Match_list {match_nil=match_nil'; match_cons=(hd,tl,expr')}
    | I.Match_variant lst ->
      let env = Location.wrap (Var.fresh ~name:"env" ()) in
      let aux fv ((c,n),expr) =
        let%bind expr = compile_expression expr in
        let%bind ((_,free_vars), case_clause) = repair_mutable_variable_in_matching expr [n] env in
        let case_clause'= expr in
        let case_clause = add_to_end case_clause (O.e_variable env) in
        ok (free_vars::fv,((c,n), case_clause, case_clause')) in
      let%bind (fv,cases) = bind_fold_map_list aux [] lst in
      let free_vars = List.sort_uniq compare_var @@ List.concat fv in
      if (List.length free_vars == 0) then (
        let cases = List.map (fun case -> let (a,_,b) = case in (a,b)) cases in
        return @@ O.e_matching ~loc matchee @@ O.Match_variant cases
      ) else (
        let cases = List.map (fun case -> let (a,b,_) = case in (a,b)) cases in
        let match_expr = O.e_matching matchee @@ O.Match_variant cases in
        let return_expr = fun expr ->
          O.e_let_in (env,O.t_wildcard ()) false false (store_mutable_variable free_vars) @@
          O.e_let_in (env,O.t_wildcard ()) false false match_expr @@
          expr 
        in
        ok @@ restore_mutable_variable return_expr free_vars env
      )
    | I.Match_record (binders,expr) ->
      let%bind expr = compile_expression expr in
      let aux (a,b,c) =
        let%bind (b,c) = compile_binder (b,c) in
        ok @@ (a,b,c) in
      let%bind binders = bind_map_list aux binders in
      return @@ O.e_matching ~loc matchee @@ O.Match_record (binders,expr)
    | I.Match_tuple (binders,expr) ->
      let%bind expr = compile_expression expr in
      let%bind binders = bind_map_list compile_binder binders in
      return @@ O.e_matching ~loc matchee @@ O.Match_tuple (binders,expr)
    | I.Match_variable (binder,expr) ->
      let%bind expr   = compile_expression expr in
      let%bind binder = compile_binder binder in
      return @@ O.e_matching ~loc matchee @@ O.Match_variable (binder,expr)
 
and compile_while I.{condition;body} =
  let env_rec = Location.wrap @@ Var.fresh ~name:"env_rec" () in
  let binder  = Location.wrap @@ Var.fresh ~name:"binder"  () in

  let%bind cond = compile_expression condition in
  let ctrl = 
    (O.e_variable binder)
  in

  let%bind for_body = compile_expression body in
  let%bind ((_,captured_name_list),for_body) = repair_mutable_variable_in_loops for_body [] binder in
  let for_body = add_to_end for_body ctrl in

  let aux name expr=
    O.e_let_in (name,O.t_wildcard ()) false false (O.e_accessor (O.e_variable binder) [Access_tuple Z.zero; Access_record (Var.to_name name.wrap_content)]) expr
  in
  let init_rec = O.e_tuple [store_mutable_variable @@ captured_name_list] in
  let restore = fun expr -> List.fold_right aux captured_name_list expr in
  let continue_expr = O.e_constant C_FOLD_CONTINUE [for_body] in
  let stop_expr = O.e_constant C_FOLD_STOP [O.e_variable binder] in
  let aux_func = 
    O.e_lambda (binder, O.t_wildcard ()) @@ 
    restore @@
    O.e_cond cond continue_expr stop_expr in
  let loop = O.e_constant C_FOLD_WHILE [aux_func; O.e_variable env_rec] in
  let let_binder = (env_rec,O.t_wildcard ()) in
  let return_expr = fun expr -> 
    O.e_let_in let_binder false false init_rec @@
    O.e_let_in let_binder false false loop @@
    O.e_let_in let_binder false false (O.e_accessor (O.e_variable env_rec) [Access_tuple Z.zero]) @@
    expr
  in
  ok @@ restore_mutable_variable return_expr captured_name_list env_rec 


and compile_for I.{binder;start;final;increment;body} =
  let env_rec = Location.wrap @@ Var.fresh ~name:"env_rec" () in
  (*Make the cond and the step *)
  let cond = I.e_annotation (I.e_constant C_LE [I.e_variable binder ; final]) (I.t_bool ()) in
  let%bind cond = compile_expression cond in
  let%bind step = compile_expression increment in
  let continue_expr = O.e_constant C_FOLD_CONTINUE [(O.e_variable env_rec)] in
  let ctrl = 
    O.e_let_in (binder,O.t_int ()) false false (O.e_constant C_ADD [ O.e_variable binder ; step ]) @@
    O.e_let_in (env_rec, O.t_wildcard ()) false false (O.e_update (O.e_variable env_rec) [Access_tuple Z.one] @@ O.e_variable binder)@@
    continue_expr
  in
  (* Modify the body loop*)
  let%bind body = compile_expression body in
  let%bind ((_,captured_name_list),for_body) = repair_mutable_variable_in_loops body [binder] env_rec in
  let for_body = add_to_end for_body ctrl in

  let aux name expr=
    O.e_let_in (name,O.t_wildcard ()) false false (O.e_accessor (O.e_variable env_rec) [Access_tuple Z.zero; Access_record (Var.to_name name.wrap_content)]) expr
  in

  (* restores the initial value of the free_var*)
  let restore = fun expr -> List.fold_right aux captured_name_list expr in

  (*Prep the lambda for the fold*)
  let stop_expr = O.e_constant C_FOLD_STOP [O.e_variable env_rec] in
  let aux_func = O.e_lambda (env_rec,O.t_wildcard ()) @@ 
                 O.e_let_in (binder,O.t_int ()) false false (O.e_accessor (O.e_variable env_rec) [Access_tuple Z.one]) @@
                 O.e_cond cond (restore for_body) (stop_expr) in

  (* Make the fold_while en precharge the vakye *)
  let loop = O.e_constant C_FOLD_WHILE [aux_func; O.e_variable env_rec] in
  let init_rec = O.e_pair (store_mutable_variable captured_name_list) @@ O.e_variable binder in
  
  let%bind start = compile_expression start in
  let let_binder = (env_rec,O.t_wildcard ()) in
  let return_expr = fun expr -> 
    O.e_let_in (binder, O.t_int ()) false false start @@
    O.e_let_in let_binder false false init_rec @@
    O.e_let_in let_binder false false loop @@
    O.e_let_in let_binder false false (O.e_accessor (O.e_variable env_rec) [Access_tuple Z.zero]) @@
    expr
  in
  ok @@ restore_mutable_variable return_expr captured_name_list env_rec 

and compile_for_each I.{binder;collection;collection_type; body} =
  let env_rec = Location.wrap @@ Var.fresh ~name:"env_rec" () in
  let args    = Location.wrap @@ Var.fresh ~name:"args" () in

  let%bind element_names = ok @@ match snd binder with
    | Some v -> [fst binder;v]
    | None -> [fst binder] 
  in
  
  let%bind body = compile_expression body in
  let%bind ((_,free_vars), body) = repair_mutable_variable_in_loops body element_names args in
  let for_body = add_to_end body @@ (O.e_accessor (O.e_variable args) [Access_tuple Z.zero]) in

  let init_record = store_mutable_variable free_vars in
  let%bind collect = compile_expression collection in
  let aux name expr=
    O.e_let_in (name,O.t_wildcard ()) false false (O.e_accessor (O.e_variable args) [Access_tuple Z.zero; Access_record (Var.to_name name.wrap_content)]) expr
  in
  let restore = fun expr -> List.fold_right aux free_vars expr in
  let restore = match collection_type with
    | Map -> (match snd binder with 
      | Some v -> fun expr -> restore (O.e_let_in (fst binder, O.t_wildcard ()) false false (O.e_accessor (O.e_variable args) [Access_tuple Z.one; Access_tuple Z.zero]) 
                                    (O.e_let_in (v, O.t_wildcard ()) false false (O.e_accessor (O.e_variable args) [Access_tuple Z.one; Access_tuple Z.one]) expr))
      | None -> fun expr -> restore (O.e_let_in (fst binder, O.t_wildcard ()) false false (O.e_accessor (O.e_variable args) [Access_tuple Z.one; Access_tuple Z.zero]) expr) 
    )
    | _ -> fun expr -> restore (O.e_let_in (fst binder, O.t_wildcard ()) false false (O.e_accessor (O.e_variable args) [Access_tuple Z.one]) expr)
  in
  let lambda = O.e_lambda (args,O.t_wildcard ()) (restore for_body) in
  let%bind op_name = match collection_type with
   | Map -> ok @@ O.C_MAP_FOLD | Set -> ok @@ O.C_SET_FOLD | List -> ok @@ O.C_LIST_FOLD 
  in
  let fold = fun expr -> 
    O.e_let_in (env_rec,O.t_wildcard ()) false false (O.e_constant op_name [lambda; collect ; init_record]) expr
  in
  ok @@ restore_mutable_variable fold free_vars env_rec

let compile_declaration : I.declaration Location.wrap -> _ =
  fun {wrap_content=declaration;location} ->
  let return decl = ok @@ Location.wrap ~loc:location decl in
  match declaration with 
  | I.Declaration_constant (n, te, inline, expr) ->
    let%bind expr = compile_expression expr in
    let%bind te   = compile_type_expression te in
    return @@ O.Declaration_constant (n, te, inline, expr)
  | I.Declaration_type (n, te) ->
    let%bind te = compile_type_expression te in
    return @@ O.Declaration_type (n,te)

let compile_program : I.program -> (O.program , Errors.purification_error) result =
  fun p ->
  bind_map_list compile_declaration p
