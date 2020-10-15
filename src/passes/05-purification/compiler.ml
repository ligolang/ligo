module Errors = Errors
module I = Ast_imperative
module O = Ast_sugar
open Trace
open Stage_common.Maps

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
          ok (true,(let_binder.var::decl_var, free_var),O.e_let_in let_binder false [] rhs let_result)
        | E_let_in {let_binder;mut=true; rhs;let_result} ->
          let name = let_binder.var in
          if List.mem ~compare:compare_var name decl_var then
            ok (true,(decl_var, free_var), O.e_let_in let_binder false [] rhs let_result)
          else(
            let free_var = if (List.mem ~compare:compare_var name free_var) then free_var else name::free_var in
            let expr = O.e_let_in_ez env false [] (O.e_update (O.e_variable env) [O.Access_record (Var.to_name name.wrap_content)] (O.e_variable name)) let_result in
            ok (true,(decl_var, free_var), O.e_let_in let_binder false [] rhs expr)
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
          let {var;ascr=_} : _ O.binder = let_binder in
          ok (true,(var::decl_var, free_var),ass_exp)
        | E_let_in {let_binder;mut=true; rhs;let_result} ->
          let name = let_binder.var in
          if List.mem ~compare:compare_var name decl_var then
            ok (true,(decl_var, free_var), O.e_let_in let_binder false [] rhs let_result)
          else(
            let free_var =
              if (List.mem ~compare:compare_var name free_var)
              then free_var
              else name::free_var in
            let expr = O.e_let_in_ez env false [] (
              O.e_update (O.e_variable env) [O.Access_tuple Z.zero; O.Access_record (Var.to_name name.wrap_content)] (O.e_variable name)
              )
              let_result in
            ok (true,(decl_var, free_var), O.e_let_in let_binder false  [] rhs expr)
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
    fun expr -> f (O.e_let_in_ez ev true [] (O.e_accessor (O.e_variable env) [O.Access_record (Var.to_name ev.wrap_content)]) expr)
  in
  let ef = List.fold_left aux (fun e -> e) free_vars in
  fun e -> match e with
    | None -> expr (ef (O.e_skip ()))
    | Some e -> expr (ef e)


let rec compile_type_expression : I.type_expression -> (O.type_expression,Errors.purification_error) result =
  fun te ->
  let self = compile_type_expression in
  let return tc = ok @@ O.make_t ~loc:te.location tc in
  match te.type_content with
    | I.T_sum sum ->
      let%bind sum = rows self sum in
      return @@ O.T_sum sum
    | I.T_record record ->
      let%bind record = rows self record in
      return @@ O.T_record record
    | I.T_tuple tuple ->
      let%bind tuple = bind_map_list self tuple in
      return @@ O.T_tuple tuple
    | I.T_arrow arr ->
      let%bind arr = arrow self arr in
      return @@ T_arrow arr
    | I.T_variable type_variable -> return @@ T_variable type_variable
    | I.T_constant {type_constant=TC_michelson_or; arguments=[l;r]} ->
      let%bind (l, l_ann) = trace_option (Errors.corner_case "not an annotated type") @@ I.get_t_annoted l in
      let%bind (r, r_ann) = trace_option (Errors.corner_case "not an annotated type") @@ I.get_t_annoted r in
      let%bind (l,r) = bind_map_pair compile_type_expression (l,r) in
      let sum : (O.label * _ O.row_element) list = [
        (O.Label "M_left" , {associated_type = l ; attributes = [ "annot:"^l_ann ] ; decl_pos = 0});
        (O.Label "M_right", {associated_type = r ; attributes = [ "annot:"^r_ann ] ; decl_pos = 1}); ]
      in
      return @@ O.T_sum { fields = O.LMap.of_list sum ; attributes = [] }
    | I.T_constant {type_constant=TC_michelson_pair; arguments=[l;r]} ->
      let%bind (l, l_ann) = trace_option (Errors.corner_case "not an annotated type") @@ I.get_t_annoted l in
      let%bind (r, r_ann) = trace_option (Errors.corner_case "not an annotated type") @@ I.get_t_annoted r in
      let%bind (l,r) = bind_map_pair compile_type_expression (l,r) in
      let sum : (O.label * _ O.row_element) list = [
        (O.Label "0", {associated_type = l ; attributes = [ "annot:"^l_ann ] ; decl_pos = 0});
        (O.Label "1", {associated_type = r ; attributes = [ "annot:"^r_ann ] ; decl_pos = 0}); ]
      in
      return @@ O.T_record { fields = (O.LMap.of_list sum) ; attributes = [] }
    | I.T_constant tc ->
      let%bind tc = type_operator self tc in
      return @@ T_constant tc
    | I.T_annoted (ty, _) -> self ty

let rec compile_expression : I.expression -> (O.expression , _) result =
  fun e ->
  let%bind e = compile_expression' e in
  ok @@ e None

and compile_expression' : I.expression -> (O.expression option -> O.expression, Errors.purification_error) result =
  fun e ->
  let self = compile_expression in
  let self_type = compile_type_expression in
  let return' expr = ok @@ function
    | None -> expr
    | Some e -> O.e_sequence expr e
  in
  let return expr = return' @@ O.make_e ~loc:e.location expr in
  match e.expression_content with
    | I.E_literal literal   -> return @@ O.E_literal literal
    | I.E_constant {cons_name;arguments} ->
      let%bind arguments = bind_map_list compile_expression arguments in
      return' @@ O.e_constant ~loc:e.location (Stage_common.Types.const_name cons_name) arguments
    | I.E_variable name     -> return @@ O.E_variable name
    | I.E_application app ->
      let%bind app = application self app in
      return @@ O.E_application app
    | I.E_lambda lamb ->
      let%bind lamb = lambda self self_type lamb in
      return @@ O.E_lambda lamb
    | I.E_recursive recs ->
      let%bind recs = recursive self self_type recs in
      return @@ O.E_recursive recs
    | I.E_let_in {let_binder;attributes;rhs;let_result} ->
      let%bind let_binder = binder self_type let_binder in
      let%bind rhs = self rhs in
      let%bind let_result = self let_result in
      return @@ O.E_let_in {let_binder;mut=false; attributes; rhs; let_result}
    | I.E_raw_code rc ->
      let%bind rc = raw_code self rc in
      return @@ O.E_raw_code rc
    | I.E_constructor const ->
      let%bind const = constructor self const in
      return @@ O.E_constructor const
    | I.E_matching m ->
      let%bind m = compile_matching m e.location in
      ok @@ m
    | I.E_record recd ->
      let%bind recd = record self recd in
      return @@ O.E_record recd
    | I.E_accessor acc ->
      let%bind acc = accessor self acc in
      return @@ O.E_accessor acc
    | I.E_update up ->
      let%bind up = update self up in
      return @@ O.E_update up
    | I.E_map map ->
      let%bind map = bind_map_list (
        bind_map_pair self
      ) map
      in
      return @@ O.E_map map
    | I.E_big_map big_map ->
      let%bind big_map = bind_map_list (
        bind_map_pair self
      ) big_map
      in
      return @@ O.E_big_map big_map
    | I.E_list lst ->
      let%bind lst = bind_map_list self lst in
      return @@ O.E_list lst
    | I.E_set set ->
      let%bind set = bind_map_list self set in
      return @@ O.E_set set
    | I.E_ascription ascr ->
      let%bind ascr = ascription self self_type ascr in
      return @@ O.E_ascription ascr
    | I.E_cond {condition;then_clause;else_clause} ->
      let%bind condition    = self condition in
      let%bind then_clause' = self then_clause in
      let%bind else_clause' = self else_clause in
      let env = Location.wrap (Var.fresh ~name:"env" ()) in
      let%bind ((_,free_vars_true), then_clause) = repair_mutable_variable_in_matching then_clause' [] env in
      let%bind ((_,free_vars_false), else_clause) = repair_mutable_variable_in_matching else_clause' [] env in
      let then_clause  = add_to_end then_clause (O.e_variable env) in
      let else_clause = add_to_end else_clause (O.e_variable env) in

      let free_vars = List.sort_uniq compare_var @@ free_vars_true @ free_vars_false in
      if (List.length free_vars != 0) then
        let cond_expr  = O.e_cond condition then_clause else_clause in
        let return_expr = fun expr ->
          O.e_let_in_ez env false [] (store_mutable_variable free_vars) @@
          O.e_let_in_ez env false [] cond_expr @@
          expr
        in
        ok @@ restore_mutable_variable return_expr free_vars env
      else
        return' @@ O.e_cond ~loc:e.location condition then_clause' else_clause'
    | I.E_sequence {expr1; expr2} ->
      let%bind expr1 = compile_expression' expr1 in
      let%bind expr2 = compile_expression' expr2 in
      ok @@ fun e -> (expr1 (Some (expr2 e))
        )
    | I.E_skip -> return @@ O.E_skip
    | I.E_tuple tuple ->
      let%bind tuple = bind_map_list self tuple in
      return @@ O.E_tuple tuple
    | I.E_assign {variable; access_path; expression} ->
      let%bind access_path = path self access_path in
      let%bind expression = self expression in
      let loc = e.location in
      let rhs = match access_path with
        [] -> expression
      | _  -> O.e_update ~loc (O.e_variable ~loc variable) access_path expression in
      ok @@ fun expr ->
        O.e_let_in_ez ~loc variable true [] rhs
        @@ Option.unopt ~default:(O.e_skip ()) expr
    | I.E_for f ->
      let%bind f = compile_for f in
      ok @@ f
    | I.E_for_each fe ->
      let%bind fe = compile_for_each fe in
      ok @@ fe
    | I.E_while w ->
      let%bind w = compile_while w in
      ok @@ w

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
          O.e_let_in_ez env false [] (store_mutable_variable free_vars) @@
          O.e_let_in_ez env false [] match_expr @@
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
          O.e_let_in_ez env false [] (store_mutable_variable free_vars) @@
          O.e_let_in_ez env false [] match_expr @@
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
          O.e_let_in_ez env false [] (store_mutable_variable free_vars) @@
          O.e_let_in_ez env false [] match_expr @@
          expr
        in
        ok @@ restore_mutable_variable return_expr free_vars env
      )
    | I.Match_record (lst,expr) ->
      let%bind expr = compile_expression expr in
      let aux (a,b) =
        let%bind b = binder compile_type_expression b in
        ok @@ (a,b) in
      let%bind binders = bind_map_list aux lst in
      return @@ O.e_matching ~loc matchee @@ O.Match_record (binders,expr)
    | I.Match_tuple (binders,expr) ->
      let%bind expr = compile_expression expr in
      let%bind binders = bind_map_list (binder compile_type_expression) binders in
      return @@ O.e_matching ~loc matchee @@ O.Match_tuple (binders,expr)
    | I.Match_variable (b,expr) ->
      let%bind expr   = compile_expression expr in
      let%bind binder = binder compile_type_expression b in
      return @@ O.e_matching ~loc matchee @@ O.Match_variable (binder,expr)

and compile_while I.{cond;body} =
  let env_rec = Location.wrap @@ Var.fresh ~name:"env_rec" () in
  let binder  = Location.wrap @@ Var.fresh ~name:"binder"  () in

  let%bind cond = compile_expression cond in
  let ctrl =
    (O.e_variable binder)
  in

  let%bind for_body = compile_expression body in
  let%bind ((_,captured_name_list),for_body) = repair_mutable_variable_in_loops for_body [] binder in
  let for_body = add_to_end for_body ctrl in

  let aux name expr=
    O.e_let_in_ez name false [] (O.e_accessor (O.e_variable binder) [Access_tuple Z.zero; Access_record (Var.to_name name.wrap_content)]) expr
  in
  let init_rec = O.e_tuple [store_mutable_variable @@ captured_name_list] in
  let restore = fun expr -> List.fold_right aux captured_name_list expr in
  let continue_expr = O.e_constant C_FOLD_CONTINUE [for_body] in
  let stop_expr = O.e_constant C_FOLD_STOP [O.e_variable binder] in
  let aux_func =
    O.e_lambda_ez binder None @@
    restore @@
    O.e_cond cond continue_expr stop_expr in
  let loop = O.e_constant C_FOLD_WHILE [aux_func; O.e_variable env_rec] in
  let return_expr = fun expr ->
    O.e_let_in_ez env_rec false [] init_rec @@
    O.e_let_in_ez env_rec false [] loop @@
    O.e_let_in_ez env_rec false [] (O.e_accessor (O.e_variable env_rec) [Access_tuple Z.zero]) @@
    expr
  in
  ok @@ restore_mutable_variable return_expr captured_name_list env_rec


and compile_for I.{binder;start;final;incr;f_body} =
  let env_rec = Location.wrap @@ Var.fresh ~name:"env_rec" () in
  (*Make the cond and the step *)
  let cond = I.e_annotation (I.e_constant (Const C_LE) [I.e_variable binder ; final]) (I.t_bool ()) in
  let%bind cond = compile_expression cond in
  let%bind step = compile_expression incr in
  let continue_expr = O.e_constant C_FOLD_CONTINUE [(O.e_variable env_rec)] in
  let ctrl =
    O.e_let_in_ez binder ~ascr:(O.t_int ()) false [] (O.e_constant C_ADD [ O.e_variable binder ; step ]) @@
    O.e_let_in_ez env_rec false [] (O.e_update (O.e_variable env_rec) [Access_tuple Z.one] @@ O.e_variable binder)@@
    continue_expr
  in
  (* Modify the body loop*)
  let%bind body = compile_expression f_body in
  let%bind ((_,captured_name_list),for_body) = repair_mutable_variable_in_loops body [binder] env_rec in
  let for_body = add_to_end for_body ctrl in

  let aux name expr=
    O.e_let_in_ez name false [] (O.e_accessor (O.e_variable env_rec) [Access_tuple Z.zero; Access_record (Var.to_name name.wrap_content)]) expr
  in

  (* restores the initial value of the free_var*)
  let restore = fun expr -> List.fold_right aux captured_name_list expr in

  (*Prep the lambda for the fold*)
  let stop_expr = O.e_constant C_FOLD_STOP [O.e_variable env_rec] in
  let aux_func = O.e_lambda_ez env_rec None @@
                 O.e_let_in_ez binder ~ascr:(O.t_int ()) false [] (O.e_accessor (O.e_variable env_rec) [Access_tuple Z.one]) @@
                 O.e_cond cond (restore for_body) (stop_expr) in

  (* Make the fold_while en precharge the vakye *)
  let loop = O.e_constant C_FOLD_WHILE [aux_func; O.e_variable env_rec] in
  let init_rec = O.e_pair (store_mutable_variable captured_name_list) @@ O.e_variable binder in

  let%bind start = compile_expression start in
  let return_expr = fun expr ->
    O.e_let_in_ez binder ~ascr:(O.t_int ()) false [] start @@
    O.e_let_in_ez env_rec false [] init_rec @@
    O.e_let_in_ez env_rec false [] loop @@
    O.e_let_in_ez env_rec false [] (O.e_accessor (O.e_variable env_rec) [Access_tuple Z.zero]) @@
    expr
  in
  ok @@ restore_mutable_variable return_expr captured_name_list env_rec

and compile_for_each I.{fe_binder;collection;collection_type; fe_body} =
  let env_rec = Location.wrap @@ Var.fresh ~name:"env_rec" () in
  let args    = Location.wrap @@ Var.fresh ~name:"args" () in

  let%bind element_names = ok @@ match snd fe_binder with
    | Some v -> [fst fe_binder;v]
    | None -> [fst fe_binder]
  in

  let%bind body = compile_expression fe_body in
  let%bind ((_,free_vars), body) = repair_mutable_variable_in_loops body element_names args in
  let for_body = add_to_end body @@ (O.e_accessor (O.e_variable args) [Access_tuple Z.zero]) in

  let init_record = store_mutable_variable free_vars in
  let%bind collect = compile_expression collection in
  let aux name expr=
    O.e_let_in_ez name false [] (O.e_accessor (O.e_variable args) [Access_tuple Z.zero; Access_record (Var.to_name name.wrap_content)]) expr
  in
  let restore = fun expr -> List.fold_right aux free_vars expr in
  let restore = match collection_type with
    | Map -> (match snd fe_binder with
      | Some v -> fun expr -> restore (O.e_let_in_ez (fst fe_binder) false [] (O.e_accessor (O.e_variable args) [Access_tuple Z.one; Access_tuple Z.zero])
                                    (O.e_let_in_ez v false [] (O.e_accessor (O.e_variable args) [Access_tuple Z.one; Access_tuple Z.one]) expr))
      | None -> fun expr -> restore (O.e_let_in_ez (fst fe_binder) false [] (O.e_accessor (O.e_variable args) [Access_tuple Z.one; Access_tuple Z.zero]) expr)
    )
    | _ -> fun expr -> restore (O.e_let_in_ez (fst fe_binder) false [] (O.e_accessor (O.e_variable args) [Access_tuple Z.one]) expr)
  in
  let lambda = O.e_lambda_ez args  None (restore for_body) in
  let%bind op_name = match collection_type with
   | Map -> ok @@ O.C_MAP_FOLD | Set -> ok @@ O.C_SET_FOLD | List -> ok @@ O.C_LIST_FOLD
  in
  let fold = fun expr ->
    O.e_let_in_ez env_rec false [] (O.e_constant op_name [lambda; collect ; init_record]) expr
  in
  ok @@ restore_mutable_variable fold free_vars env_rec

let compile_declaration : I.declaration Location.wrap -> _ =
  fun {wrap_content=declaration;location} ->
  let return decl = ok @@ Location.wrap ~loc:location decl in
  match declaration with
  | I.Declaration_type dt ->
    let%bind dt = declaration_type compile_type_expression dt in
    return @@ O.Declaration_type dt
  | I.Declaration_constant dc ->
    let%bind dc = declaration_constant compile_expression compile_type_expression dc in
    return @@ O.Declaration_constant dc

let compile_program : I.program -> (O.program , Errors.purification_error) result = fun p ->
  program compile_declaration p
