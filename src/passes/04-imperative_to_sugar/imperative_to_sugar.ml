module Errors = Errors
module I = Ast_imperative
module O = Ast_sugar
open Trace

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
          if List.mem name decl_var then 
            ok (true,(decl_var, free_var), O.e_let_in let_binder false false rhs let_result)
          else(
            let free_var = if (List.mem name free_var) then free_var else name::free_var in
            let expr = O.e_let_in (env,None) false false (O.e_update (O.e_variable env) [O.Access_record (Var.to_name name)] (O.e_variable name)) let_result in
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
          if List.mem name decl_var then 
            ok (true,(decl_var, free_var), O.e_let_in let_binder false false rhs let_result)
          else(
            let free_var = if (List.mem name free_var) then free_var else name::free_var in
            let expr = O.e_let_in (env,None) false false (
              O.e_update (O.e_variable env) [O.Access_tuple Z.zero; O.Access_record (Var.to_name name)] (O.e_variable name)
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
    let aux var = (O.Label (Var.to_name var), O.e_variable var) in
    O.e_record @@ O.LMap.of_list (List.map aux free_vars)
 
and restore_mutable_variable (expr : O.expression->O.expression) (free_vars : O.expression_variable list) (env : O.expression_variable) =
  let aux (f: O.expression -> O.expression) (ev: O.expression_variable) =
    fun expr -> f (O.e_let_in (ev,None) true false (O.e_accessor (O.e_variable env) [O.Access_record (Var.to_name ev)]) expr)
  in
  let ef = List.fold_left aux (fun e -> e) free_vars in
  fun e -> match e with 
    | None -> expr (ef (O.e_skip ()))
    | Some e -> expr (ef e)


let rec compile_type_expression : I.type_expression -> (O.type_expression,Errors.imperative_to_sugar_error) result =
  fun te ->
  let return tc = ok @@ O.make_t ~loc:te.location tc in
  match te.type_content with
    | I.T_sum sum -> 
      let sum = I.CMap.to_kv_list sum in
      let%bind sum = 
        bind_map_list (fun (k,({ctor_type = v; ctor_decl_pos ; _}:I.ctor_content)) ->
          let%bind v = compile_type_expression v in
          let content : O.ctor_content = {ctor_type = v ; michelson_annotation = None ; ctor_decl_pos } in
          ok @@ (k,content)
        ) sum
      in
      return @@ O.T_sum (O.CMap.of_list sum)
    | I.T_record record -> 
      let record = I.LMap.to_kv_list record in
      let%bind record = 
        bind_map_list (fun (k, ({field_type = v; field_decl_pos ; _}:I.field_content)) ->
          let%bind v = compile_type_expression v in
          let content : O.field_content = {field_type = v; michelson_annotation = None ; field_decl_pos} in
          ok @@ (k,content)
        ) record
      in
      return @@ O.T_record (O.LMap.of_list record)
    | I.T_tuple tuple ->
      let%bind tuple = bind_map_list compile_type_expression tuple in
      return @@ O.T_tuple tuple
    | I.T_arrow {type1;type2} ->
      let%bind type1 = compile_type_expression type1 in
      let%bind type2 = compile_type_expression type2 in
      return @@ T_arrow {type1;type2}
    | I.T_variable type_variable -> return @@ T_variable type_variable 
    | I.T_constant type_constant -> return @@ T_constant type_constant
    | I.T_operator (TC_michelson_or, [l;r]) ->
      let%bind (l, l_ann) = trace_option (Errors.corner_case "not an annotated type") @@ I.get_t_annoted l in
      let%bind (r, r_ann) = trace_option (Errors.corner_case "not an annotated type") @@ I.get_t_annoted r in
      let%bind (l,r) = bind_map_pair compile_type_expression (l,r) in
      let sum : (O.constructor' * O.ctor_content) list = [
        (O.Constructor "M_left" , {ctor_type = l ; michelson_annotation = Some l_ann ; ctor_decl_pos = 0}); 
        (O.Constructor "M_right", {ctor_type = r ; michelson_annotation = Some r_ann ; ctor_decl_pos = 1}); ]
      in
      return @@ O.T_sum (O.CMap.of_list sum)
    | I.T_operator (TC_michelson_pair, [l;r]) ->
      let%bind (l, l_ann) = trace_option (Errors.corner_case "not an annotated type") @@ I.get_t_annoted l in
      let%bind (r, r_ann) = trace_option (Errors.corner_case "not an annotated type") @@ I.get_t_annoted r in
      let%bind (l,r) = bind_map_pair compile_type_expression (l,r) in
      let sum : (O.label * O.field_content) list = [
        (O.Label "0" , {field_type = l ; michelson_annotation = Some l_ann ; field_decl_pos = 0}); 
        (O.Label "1", {field_type = r ; michelson_annotation = Some r_ann ; field_decl_pos = 0}); ]
      in
      return @@ O.T_record (O.LMap.of_list sum)
    | I.T_operator (type_operator, lst) ->
      let%bind lst = bind_map_list compile_type_expression lst in
      return @@ T_operator (type_operator, lst)
    | I.T_annoted (ty, _) -> compile_type_expression ty

let rec compile_expression : I.expression -> (O.expression , _) result =
  fun e ->
  let%bind e = compile_expression' e in
  ok @@ e None

and compile_expression' : I.expression -> (O.expression option -> O.expression, Errors.imperative_to_sugar_error) result =
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
      let (binder,ty_opt) = let_binder in
      let%bind ty_opt = bind_map_option compile_type_expression ty_opt in
      let%bind rhs = compile_expression rhs in
      let%bind let_result = compile_expression let_result in
      return @@ O.e_let_in ~loc (binder,ty_opt) false inline rhs let_result
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
      let env = Var.fresh () in
      let%bind ((_,free_vars_true), then_clause) = repair_mutable_variable_in_matching then_clause' [] env in
      let%bind ((_,free_vars_false), else_clause) = repair_mutable_variable_in_matching else_clause' [] env in
      let then_clause  = add_to_end then_clause (O.e_variable env) in
      let else_clause = add_to_end else_clause (O.e_variable env) in

      let free_vars = List.sort_uniq Var.compare @@ free_vars_true @ free_vars_false in
      if (List.length free_vars != 0) then 
        let cond_expr  = O.e_cond condition then_clause else_clause in
        let return_expr = fun expr ->
          O.e_let_in (env,None) false false (store_mutable_variable free_vars) @@
          O.e_let_in (env,None) false false cond_expr @@
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
      let rhs = O.e_update ~loc (O.e_variable ~loc variable) access_path expression in
      ok @@ fun expr -> (match expr with 
       | None -> O.e_let_in ~loc (variable,None) true false rhs (O.e_skip ())
       | Some e -> O.e_let_in ~loc (variable, None) true false rhs e
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

and compile_path : I.access list -> (O.access list, Errors.imperative_to_sugar_error) result =
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
  fun {binder;input_type;output_type;result}->
    let%bind input_type = bind_map_option compile_type_expression input_type in
    let%bind output_type = bind_map_option compile_type_expression output_type in
    let%bind result = compile_expression result in
    ok @@ O.{binder;input_type;output_type;result}

and compile_matching : I.matching -> Location.t -> (O.expression option -> O.expression, Errors.imperative_to_sugar_error) result =
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
      let env = Var.fresh () in
      let%bind ((_,free_vars_none), match_none) = repair_mutable_variable_in_matching match_none' [] env in
      let%bind ((_,free_vars_some), expr) = repair_mutable_variable_in_matching expr' [n] env in
      let match_none = add_to_end match_none (O.e_variable env) in
      let expr       = add_to_end expr (O.e_variable env) in
      let free_vars = List.sort_uniq Var.compare @@ free_vars_none @ free_vars_some in
      if (List.length free_vars != 0) then
        let match_expr  = O.e_matching matchee (O.Match_option {match_none; match_some=(n,expr)}) in
        let return_expr = fun expr ->
          O.e_let_in (env,None) false false (store_mutable_variable free_vars) @@
          O.e_let_in (env,None) false false match_expr @@
          expr 
        in
        ok @@ restore_mutable_variable return_expr free_vars env
      else
        return @@ O.e_matching ~loc matchee @@ O.Match_option {match_none=match_none'; match_some=(n,expr')}
    | I.Match_list {match_nil;match_cons} ->
      let%bind match_nil' = compile_expression match_nil in
      let (hd,tl,expr) = match_cons in
      let%bind expr' = compile_expression expr in
      let env = Var.fresh () in
      let%bind ((_,free_vars_nil), match_nil) = repair_mutable_variable_in_matching match_nil' [] env in
      let%bind ((_,free_vars_cons), expr) = repair_mutable_variable_in_matching expr' [hd;tl] env in
      let match_nil = add_to_end match_nil (O.e_variable env) in
      let expr      = add_to_end expr (O.e_variable env) in
      let free_vars = List.sort_uniq Var.compare @@ free_vars_nil @ free_vars_cons in
      if (List.length free_vars != 0) then
        let match_expr  = O.e_matching matchee (O.Match_list {match_nil; match_cons=(hd,tl,expr)}) in
        let return_expr = fun expr ->
          O.e_let_in (env,None) false false (store_mutable_variable free_vars) @@
          O.e_let_in (env,None) false false match_expr @@
          expr 
        in
        ok @@ restore_mutable_variable return_expr free_vars env
      else
        return @@ O.e_matching ~loc matchee @@ O.Match_list {match_nil=match_nil'; match_cons=(hd,tl,expr')}
    | I.Match_variant lst ->
      let env = Var.fresh () in
      let aux fv ((c,n),expr) =
        let%bind expr = compile_expression expr in
        let%bind ((_,free_vars), case_clause) = repair_mutable_variable_in_matching expr [n] env in
        let case_clause'= expr in
        let case_clause = add_to_end case_clause (O.e_variable env) in
        ok (free_vars::fv,((c,n), case_clause, case_clause')) in
      let%bind (fv,cases) = bind_fold_map_list aux [] lst in
      let free_vars = List.sort_uniq Var.compare @@ List.concat fv in
      if (List.length free_vars == 0) then (
        let cases = List.map (fun case -> let (a,_,b) = case in (a,b)) cases in
        return @@ O.e_matching ~loc matchee @@ O.Match_variant cases
      ) else (
        let cases = List.map (fun case -> let (a,b,_) = case in (a,b)) cases in
        let match_expr = O.e_matching matchee @@ O.Match_variant cases in
        let return_expr = fun expr ->
          O.e_let_in (env,None) false false (store_mutable_variable free_vars) @@
          O.e_let_in (env,None) false false match_expr @@
          expr 
        in
        ok @@ restore_mutable_variable return_expr free_vars env
      )
    | I.Match_record (lst,ty_opt,expr) ->
      let%bind expr = compile_expression expr in
      let%bind ty_opt = bind_map_option (bind_map_list compile_type_expression) ty_opt in
      return @@ O.e_matching ~loc matchee @@ O.Match_record (lst,ty_opt,expr)
    | I.Match_tuple (lst,ty_opt,expr) ->
      let%bind expr = compile_expression expr in
      let%bind ty_opt = bind_map_option (bind_map_list compile_type_expression) ty_opt in
      return @@ O.e_matching ~loc matchee @@ O.Match_tuple (lst,ty_opt,expr)
    | I.Match_variable (lst,ty_opt,expr) ->
      let%bind expr = compile_expression expr in
      let%bind ty_opt = bind_map_option compile_type_expression ty_opt in
      return @@ O.e_matching ~loc matchee @@ O.Match_variable (lst,ty_opt,expr)
 
and compile_while I.{condition;body} =
  let env_rec = Var.fresh () in
  let binder  = Var.fresh () in

  let%bind cond = compile_expression condition in
  let ctrl = 
    (O.e_variable binder)
  in

  let%bind for_body = compile_expression body in
  let%bind ((_,captured_name_list),for_body) = repair_mutable_variable_in_loops for_body [] binder in
  let for_body = add_to_end for_body ctrl in

  let aux name expr=
    O.e_let_in (name,None) false false (O.e_accessor (O.e_variable binder) [Access_tuple Z.zero; Access_record (Var.to_name name)]) expr
  in
  let init_rec = O.e_tuple [store_mutable_variable @@ captured_name_list] in
  let restore = fun expr -> List.fold_right aux captured_name_list expr in
  let continue_expr = O.e_constant C_FOLD_CONTINUE [for_body] in
  let stop_expr = O.e_constant C_FOLD_STOP [O.e_variable binder] in
  let aux_func = 
    O.e_lambda binder None None @@ 
    restore @@
    O.e_cond cond continue_expr stop_expr in
  let loop = O.e_constant C_FOLD_WHILE [aux_func; O.e_variable env_rec] in
  let let_binder = (env_rec,None) in
  let return_expr = fun expr -> 
    O.e_let_in let_binder false false init_rec @@
    O.e_let_in let_binder false false loop @@
    O.e_let_in let_binder false false (O.e_accessor (O.e_variable env_rec) [Access_tuple Z.zero]) @@
    expr
  in
  ok @@ restore_mutable_variable return_expr captured_name_list env_rec 


and compile_for I.{binder;start;final;increment;body} =
  let env_rec = Var.fresh () in
  (*Make the cond and the step *)
  let cond = I.e_annotation (I.e_constant C_LE [I.e_variable binder ; final]) (I.t_bool ()) in
  let%bind cond = compile_expression cond in
  let%bind step = compile_expression increment in
  let continue_expr = O.e_constant C_FOLD_CONTINUE [(O.e_variable env_rec)] in
  let ctrl = 
    O.e_let_in (binder,Some (O.t_int ())) false false (O.e_constant C_ADD [ O.e_variable binder ; step ]) @@
    O.e_let_in (env_rec, None) false false (O.e_update (O.e_variable env_rec) [Access_tuple Z.one] @@ O.e_variable binder)@@
    continue_expr
  in
  (* Modify the body loop*)
  let%bind body = compile_expression body in
  let%bind ((_,captured_name_list),for_body) = repair_mutable_variable_in_loops body [binder] env_rec in
  let for_body = add_to_end for_body ctrl in

  let aux name expr=
    O.e_let_in (name,None) false false (O.e_accessor (O.e_variable env_rec) [Access_tuple Z.zero; Access_record (Var.to_name name)]) expr
  in

  (* restores the initial value of the free_var*)
  let restore = fun expr -> List.fold_right aux captured_name_list expr in

  (*Prep the lambda for the fold*)
  let stop_expr = O.e_constant C_FOLD_STOP [O.e_variable env_rec] in
  let aux_func = O.e_lambda env_rec None None @@ 
                 O.e_let_in (binder,Some (O.t_int ())) false false (O.e_accessor (O.e_variable env_rec) [Access_tuple Z.one]) @@
                 O.e_cond cond (restore for_body) (stop_expr) in

  (* Make the fold_while en precharge the vakye *)
  let loop = O.e_constant C_FOLD_WHILE [aux_func; O.e_variable env_rec] in
  let init_rec = O.e_pair (store_mutable_variable captured_name_list) @@ O.e_variable binder in
  
  let%bind start = compile_expression start in
  let let_binder = (env_rec,None) in
  let return_expr = fun expr -> 
    O.e_let_in (binder, Some (O.t_int ())) false false start @@
    O.e_let_in let_binder false false init_rec @@
    O.e_let_in let_binder false false loop @@
    O.e_let_in let_binder false false (O.e_accessor (O.e_variable env_rec) [Access_tuple Z.zero]) @@
    expr
  in
  ok @@ restore_mutable_variable return_expr captured_name_list env_rec 

and compile_for_each I.{binder;collection;collection_type; body} =
  let env_rec = Var.fresh () in
  let args = Var.fresh () in

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
    O.e_let_in (name,None) false false (O.e_accessor (O.e_variable args) [Access_tuple Z.zero; Access_record (Var.to_name name)]) expr
  in
  let restore = fun expr -> List.fold_right aux free_vars expr in
  let restore = match collection_type with
    | Map -> (match snd binder with 
      | Some v -> fun expr -> restore (O.e_let_in (fst binder, None) false false (O.e_accessor (O.e_variable args) [Access_tuple Z.one; Access_tuple Z.zero]) 
                                    (O.e_let_in (v, None) false false (O.e_accessor (O.e_variable args) [Access_tuple Z.one; Access_tuple Z.one]) expr))
      | None -> fun expr -> restore (O.e_let_in (fst binder, None) false false (O.e_accessor (O.e_variable args) [Access_tuple Z.one; Access_tuple Z.zero]) expr) 
    )
    | _ -> fun expr -> restore (O.e_let_in (fst binder, None) false false (O.e_accessor (O.e_variable args) [Access_tuple Z.one]) expr)
  in
  let lambda = O.e_lambda args None None (restore for_body) in
  let%bind op_name = match collection_type with
   | Map -> ok @@ O.C_MAP_FOLD | Set -> ok @@ O.C_SET_FOLD | List -> ok @@ O.C_LIST_FOLD 
  in
  let fold = fun expr -> 
    O.e_let_in (env_rec,None) false false (O.e_constant op_name [lambda; collect ; init_record]) expr
  in
  ok @@ restore_mutable_variable fold free_vars env_rec

let compile_declaration : I.declaration Location.wrap -> _ =
  fun {wrap_content=declaration;location} ->
  let return decl = ok @@ Location.wrap ~loc:location decl in
  match declaration with 
  | I.Declaration_constant (n, te_opt, inline, expr) ->
    let%bind expr = compile_expression expr in
    let%bind te_opt = bind_map_option compile_type_expression te_opt in
    return @@ O.Declaration_constant (n, te_opt, inline, expr)
  | I.Declaration_type (n, te) ->
    let%bind te = compile_type_expression te in
    return @@ O.Declaration_type (n,te)

let compile_program : I.program -> (O.program , Errors.imperative_to_sugar_error) result =
  fun p ->
  bind_map_list compile_declaration p

(* uncompiling *)
let rec uncompile_type_expression : O.type_expression -> (I.type_expression , Errors.imperative_to_sugar_error) result =
  fun te ->
  let return te = ok @@ I.make_t te in
  match te.type_content with
    | O.T_sum sum -> 
      (* This type sum could be a michelson_or as well, we could use is_michelson_or *)
      let sum = I.CMap.to_kv_list sum in
      let%bind sum = 
        bind_map_list (fun (k,v) ->
          let {ctor_type;ctor_decl_pos;_} : O.ctor_content = v in
          let%bind v = uncompile_type_expression ctor_type in
          ok @@ (k,({ctor_type=v; ctor_decl_pos}: I.ctor_content))
        ) sum
      in
      return @@ I.T_sum (O.CMap.of_list sum)
    | O.T_record record -> 
      let record = I.LMap.to_kv_list record in
      let%bind record = 
        bind_map_list (fun (k,v) ->
          let {field_type;field_decl_pos} : O.field_content = v in
          let%bind v = uncompile_type_expression field_type in
          ok @@ (k,({field_type=v;field_decl_pos}:I.field_content))
        ) record
      in
      return @@ I.T_record (O.LMap.of_list record)
    | O.T_tuple tuple ->
      let%bind tuple = bind_map_list uncompile_type_expression tuple in
      return @@ I.T_tuple tuple
    | O.T_arrow {type1;type2} ->
      let%bind type1 = uncompile_type_expression type1 in
      let%bind type2 = uncompile_type_expression type2 in
      return @@ T_arrow {type1;type2}
    | O.T_variable type_variable -> return @@ T_variable type_variable 
    | O.T_constant type_constant -> return @@ T_constant type_constant
    | O.T_operator (type_operator, lst) ->
      let%bind lst = bind_map_list uncompile_type_expression lst in
      return @@ T_operator (type_operator, lst)

let rec uncompile_expression : O.expression -> (I.expression , Errors.imperative_to_sugar_error) result =
  fun e ->
  let return expr = ok @@ I.make_e ~loc:e.location expr in
  match e.expression_content with 
    O.E_literal lit -> return @@ I.E_literal lit
  | O.E_constant {cons_name;arguments} -> 
    let%bind arguments = bind_map_list uncompile_expression arguments in
    return @@ I.E_constant {cons_name;arguments}
  | O.E_variable name     -> return @@ I.E_variable name
  | O.E_application {lamb; args} -> 
    let%bind lamb = uncompile_expression lamb in
    let%bind args = uncompile_expression args in
    return @@ I.E_application {lamb; args}
  | O.E_lambda lambda ->
    let%bind lambda = uncompile_lambda lambda in
    return @@ I.E_lambda lambda
  | O.E_recursive {fun_name;fun_type;lambda} ->
    let%bind fun_type = uncompile_type_expression fun_type in
    let%bind lambda = uncompile_lambda lambda in
    return @@ I.E_recursive {fun_name;fun_type;lambda}
  | O.E_let_in {let_binder;inline;rhs;let_result} ->
    let (binder,ty_opt) = let_binder in
    let%bind ty_opt = bind_map_option uncompile_type_expression ty_opt in
    let%bind rhs = uncompile_expression rhs in
    let%bind let_result = uncompile_expression let_result in
    return @@ I.E_let_in {let_binder=(binder,ty_opt);inline;rhs;let_result}
  | O.E_raw_code {language;code} ->
    let%bind code  = uncompile_expression code in
    return @@ I.E_raw_code {language;code} 
  | O.E_constructor {constructor;element} ->
    let%bind element = uncompile_expression element in
    return @@ I.E_constructor {constructor;element}
  | O.E_matching {matchee; cases} ->
    let%bind matchee = uncompile_expression matchee in
    let%bind cases   = uncompile_matching cases in
    return @@ I.E_matching {matchee;cases}
  | O.E_record record ->
    let record = I.LMap.to_kv_list record in
    let%bind record = 
      bind_map_list (fun (k,v) ->
        let%bind v = uncompile_expression v in
        ok @@ (k,v)
      ) record
    in
    return @@ I.E_record (O.LMap.of_list record)
  | O.E_accessor {record;path} ->
    let%bind record = uncompile_expression record in
    let%bind path = uncompile_path path in
    return @@ I.E_accessor {record;path}
  | O.E_update {record;path;update} ->
    let%bind record = uncompile_expression record in
    let%bind path = uncompile_path path in
    let%bind update = uncompile_expression update in
    return @@ I.E_update {record;path;update}
  | O.E_tuple tuple ->
    let%bind tuple = bind_map_list uncompile_expression tuple in
    return @@ I.E_tuple tuple
  | O.E_map map ->
    let%bind map = bind_map_list (
      bind_map_pair uncompile_expression
    ) map
    in
    return @@ I.E_map map
  | O.E_big_map big_map ->
    let%bind big_map = bind_map_list (
      bind_map_pair uncompile_expression
    ) big_map
    in
    return @@ I.E_big_map big_map
  | O.E_list lst ->
    let%bind lst = bind_map_list uncompile_expression lst in
    return @@ I.E_list lst
  | O.E_set set ->
    let%bind set = bind_map_list uncompile_expression set in
    return @@ I.E_set set 
  | O.E_ascription {anno_expr; type_annotation} ->
    let%bind anno_expr = uncompile_expression anno_expr in
    let%bind type_annotation = uncompile_type_expression type_annotation in
    return @@ I.E_ascription {anno_expr; type_annotation}
  | O.E_cond {condition;then_clause;else_clause} ->
    let%bind condition   = uncompile_expression condition in
    let%bind then_clause = uncompile_expression then_clause in
    let%bind else_clause = uncompile_expression else_clause in
    return @@ I.E_cond {condition; then_clause; else_clause}
  | O.E_sequence {expr1; expr2} ->
    let%bind expr1 = uncompile_expression expr1 in
    let%bind expr2 = uncompile_expression expr2 in
    return @@ I.E_sequence {expr1; expr2}
  | O.E_skip -> return @@ I.E_skip

and uncompile_path : O.access list -> (I.access list, Errors.imperative_to_sugar_error) result =
  fun path -> let aux a = match a with
    | O.Access_record s -> ok @@ I.Access_record s
    | O.Access_tuple  i -> ok @@ I.Access_tuple  i
    | O.Access_map e ->
      let%bind e = uncompile_expression e in
      ok @@ I.Access_map e
  in
  bind_map_list aux path

and uncompile_lambda : O.lambda -> (I.lambda , Errors.imperative_to_sugar_error) result =
  fun {binder;input_type;output_type;result}->
    let%bind input_type = bind_map_option uncompile_type_expression input_type in
    let%bind output_type = bind_map_option uncompile_type_expression output_type in
    let%bind result = uncompile_expression result in
    ok @@ I.{binder;input_type;output_type;result}
and uncompile_matching : O.matching_expr -> (I.matching_expr , Errors.imperative_to_sugar_error) result =
  fun m -> 
  match m with 
    | O.Match_list {match_nil;match_cons} ->
      let%bind match_nil = uncompile_expression match_nil in
      let (hd,tl,expr) = match_cons in
      let%bind expr = uncompile_expression expr in
      ok @@ I.Match_list {match_nil; match_cons=(hd,tl,expr)}
    | O.Match_option {match_none;match_some} ->
      let%bind match_none = uncompile_expression match_none in
      let (n,expr) = match_some in
      let%bind expr = uncompile_expression expr in
      ok @@ I.Match_option {match_none; match_some=(n,expr)}
    | O.Match_variant lst ->
      let%bind lst = bind_map_list (
        fun ((c,n),expr) ->
          let%bind expr = uncompile_expression expr in
          ok @@ ((c,n),expr)
      ) lst 
      in
      ok @@ I.Match_variant lst
    | O.Match_record (lst,ty_opt,expr) ->
      let%bind expr = uncompile_expression expr in
      let%bind ty_opt = bind_map_option (bind_map_list uncompile_type_expression) ty_opt in
      ok @@ I.Match_record (lst,ty_opt,expr)
    | O.Match_tuple (lst,ty_opt,expr) ->
      let%bind expr = uncompile_expression expr in
      let%bind ty_opt = bind_map_option (bind_map_list uncompile_type_expression) ty_opt in
      ok @@ I.Match_tuple (lst,ty_opt,expr)
    | O.Match_variable (lst,ty_opt,expr) ->
      let%bind expr = uncompile_expression expr in
      let%bind ty_opt = bind_map_option uncompile_type_expression ty_opt in
      ok @@ I.Match_variable (lst,ty_opt,expr)
