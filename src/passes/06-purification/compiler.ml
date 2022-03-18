module Location = Simple_utils.Location
module Pair     = Simple_utils.Pair
module Errors = Errors
module I = Ast_imperative
module O = Ast_sugar
open Simple_utils.Trace
open Stage_common.Maps
module VMap = Simple_utils.Map.Make(I.ValueVar)


let rec add_to_end (expression: O.expression) to_add =
  match expression.expression_content with
  | O.E_let_in lt ->
    let lt = {lt with let_result = add_to_end lt.let_result to_add} in
    {expression with expression_content = O.E_let_in lt}
  | O.E_sequence seq ->
    let seq = {seq with expr2 = add_to_end seq.expr2 to_add} in
    {expression with expression_content = O.E_sequence seq}
  | _ -> O.e_sequence expression to_add

let repair_mutable_variable_in_matching (match_body : O.expression) (element_names : O.expression_variable list) (init_fv : Z.t VMap.t) (i : Z.t) (env : I.expression_variable) =
  let i = ref i in
  let ((dv,fv),mb) = Self_ast_sugar.fold_map_expression
    (* TODO : these should use Variables sets *)
    (fun (decl_var,free_var : O.expression_variable list * Z.t VMap.t) (ass_exp : O.expression) ->
      match ass_exp.expression_content with
        | E_let_in {let_binder;mut=false;rhs;let_result;attributes=_} ->
          (true,(let_binder.var::decl_var, free_var),O.e_let_in let_binder false [] rhs let_result)
        | E_let_in {let_binder;mut=true; rhs;let_result;attributes=_} ->
          let name = let_binder.var in
          if List.mem ~equal:I.ValueVar.equal decl_var name then
            (true,(decl_var, free_var), O.e_let_in let_binder false [] rhs let_result)
          else(
            let free_var = if (VMap.mem name free_var) then free_var else (let x = !i in i := Z.succ x; VMap.add name x free_var) in
            let expr = O.e_let_in_ez env false [] (O.e_update (O.e_variable env) [
              O.Access_tuple (VMap.find name free_var)
              ] (O.e_variable name)) let_result in
            (true,(decl_var, free_var), O.e_let_in let_binder false [] rhs expr)
          )
        | E_constant {cons_name=C_MAP_FOLD;arguments= _}
        | E_constant {cons_name=C_SET_FOLD;arguments= _}
        | E_constant {cons_name=C_LIST_FOLD;arguments= _}
        | E_constant {cons_name=C_FOLD;arguments= _}
        | E_cond _
        | E_matching _ -> (false, (decl_var,free_var),ass_exp)
      | E_constant _
      | E_skip
      | E_literal _ | E_variable _
      | E_type_in _ | E_mod_in _ |  E_mod_alias _
      | E_application _ | E_lambda _| E_type_abstraction _| E_recursive _ | E_raw_code _
      | E_constructor _ | E_record _| E_accessor _|E_update _
      | E_ascription _  | E_sequence _ | E_tuple _
      | E_map _ | E_big_map _ |E_list _ | E_set _
      | E_module_accessor _
       -> (true, (decl_var, free_var),ass_exp)
    )
      (element_names,init_fv)
      match_body in
  ((dv,fv),mb,!i)

and repair_mutable_variable_in_loops (for_body : O.expression) (element_names : O.expression_variable list) (env : O.expression_variable) =
  let i = ref Z.zero in
  let ((dv,fv),fb) = Self_ast_sugar.fold_map_expression
    (* TODO : these should use Variables sets *)
    (fun (decl_var,free_var : O.expression_variable list * Z.t VMap.t) (ass_exp : O.expression) ->
      (* Format.printf "debug: dv:%a; fv:%a; expr:%a \n%!"
        (I.PP.list_sep_d I.PP.expression_variable) decl_var
        (I.PP.list_sep_d I.PP.expression_variable) decl_var
        O.PP.expression ass_exp
      ;*)
      match ass_exp.expression_content with
        | E_let_in {let_binder;mut=false; rhs=_;let_result=_;attributes=_} ->
          let {var;ascr=_;attributes=_} : _ O.binder = let_binder in
          (true,(var::decl_var, free_var),ass_exp)
        | E_let_in {let_binder;mut=true; rhs;let_result;attributes=_} ->
          let name = let_binder.var in
          if List.mem ~equal:O.ValueVar.equal decl_var name then
            (true,(decl_var, free_var), O.e_let_in let_binder false [] rhs let_result)
          else(
            let free_var =
              if (VMap.mem name free_var)
              then free_var
              else (let x = !i in i := Z.succ x; VMap.add name x free_var) in
            let expr = O.e_let_in_ez env false [] (
              O.e_update (O.e_variable env) [O.Access_tuple Z.zero;
              O.Access_tuple (VMap.find name free_var)] (O.e_variable name)
              )
              let_result in
            (true,(decl_var, free_var), O.e_let_in let_binder false  [] rhs expr)
          )
        | E_constant {cons_name=C_MAP_FOLD;arguments= _}
        | E_constant {cons_name=C_SET_FOLD;arguments= _}
        | E_constant {cons_name=C_LIST_FOLD;arguments= _}
        | E_constant {cons_name=C_FOLD;arguments= _}
        | E_cond _
        | E_matching _ -> (false,(decl_var,free_var),ass_exp)
      | E_constant _
      | E_skip
      | E_literal _ | E_variable _
      | E_type_in _ | E_mod_in _ | E_mod_alias _
      | E_application _ | E_lambda _| E_type_abstraction _| E_recursive _ | E_raw_code _
      | E_constructor _ | E_record _| E_accessor _| E_update _
      | E_ascription _  | E_sequence _ | E_tuple _
      | E_map _ | E_big_map _ |E_list _ | E_set _
      | E_module_accessor _
       -> (true, (decl_var, free_var),ass_exp)
    )
      (element_names,VMap.empty)
      for_body in
  ((dv,fv),fb)

and store_mutable_variable (free_vars : Z.t VMap.t) =
  if (VMap.is_empty free_vars) then
    O.e_unit ()
  else
    O.e_tuple @@ List.map ~f:O.e_variable @@
     List.map ~f:fst @@ List.dedup_and_sort
     ~compare:(fun (_,a) (_,b) -> Z.compare a b) @@ VMap.to_kv_list free_vars

and restore_mutable_variable (expr : O.expression->O.expression) (free_vars : Z.t VMap.t) (env : O.expression_variable) =
(*
  let aux (ev,i) =
    ev, O.e_accessor (O.e_variable env) [O.Access_tuple i] in
  let accesses = List.map ~f:aux @@ VMap.to_kv_list free_vars in
  let aux (f: O.expression -> O.expression) (ev, rhs: O.expression_variable * O.expression) =
    fun expr -> f @@ O.e_let_in_ez ev true [] rhs expr
  in
  let ef = List.fold_left ~f:aux ~init:(fun e -> e) accesses in
  *)
  let aux ev i (f: O.expression -> O.expression) =
    fun expr ->
      O.e_let_in_ez ev true []
      (O.e_accessor (O.e_variable env)
      [O.Access_tuple i]) @@ f expr in
  let ef = VMap.fold aux free_vars (fun e -> e) in
  fun e -> match e with
    | None ->
      expr (ef (O.e_skip ()))
    | Some e -> expr (ef e)


let rec compile_type_expression ~raise : I.type_expression -> O.type_expression =
  fun te ->
  (*
    At this point sum and record types becomes linear by their type (see previous pass in self_imperative)
    TODO: nano pass from non-linear to linear types (?)
  *)
  let self = compile_type_expression ~raise in
  let return tc = O.make_t ~loc:te.location tc in
  match te.type_content with
    | I.T_sum { attributes ; fields } ->
      let fields = O.LMap.(map (row_element self) (of_list fields)) in
      return @@ O.T_sum { attributes ; fields }
    | I.T_record { attributes ; fields } ->
      let fields = O.LMap.(map (row_element self) (of_list fields)) in
      return @@ O.T_record { attributes ; fields }
    | I.T_tuple tuple ->
      let tuple = List.map ~f:self tuple in
      return @@ O.T_tuple tuple
    | I.T_arrow arr ->
      let arr = arrow self arr in
      return @@ T_arrow arr
    | I.T_variable type_variable -> return @@ T_variable type_variable
    | I.T_app {type_operator;arguments=[l;r]} when I.TypeVar.equal Stage_common.Constant.v_michelson_or type_operator ->
      let (l, l_ann) = trace_option ~raise (Errors.corner_case "not an annotated type") @@ I.get_t_annoted l in
      let (r, r_ann) = trace_option ~raise (Errors.corner_case "not an annotated type") @@ I.get_t_annoted r in
      let (l,r) = Pair.map ~f:(compile_type_expression ~raise) (l,r) in
      let sum : (O.label * _ O.row_element) list = [
        (O.Label "M_left" , {associated_type = l ; attributes = [ "annot:"^l_ann ] ; decl_pos = 0});
        (O.Label "M_right", {associated_type = r ; attributes = [ "annot:"^r_ann ] ; decl_pos = 1}); ]
      in
      return @@ O.T_sum { fields = O.LMap.of_list sum ; attributes = [] }
    | I.T_app {type_operator;arguments=[l;r]} when I.TypeVar.equal Stage_common.Constant.v_michelson_pair type_operator ->
      let (l, l_ann) = trace_option ~raise (Errors.corner_case "not an annotated type") @@ I.get_t_annoted l in
      let (r, r_ann) = trace_option ~raise (Errors.corner_case "not an annotated type") @@ I.get_t_annoted r in
      let (l,r) = Pair.map ~f:(compile_type_expression ~raise) (l,r) in
      let sum : (O.label * _ O.row_element) list = [
        (O.Label "0", {associated_type = l ; attributes = [ "annot:"^l_ann ] ; decl_pos = 0});
        (O.Label "1", {associated_type = r ; attributes = [ "annot:"^r_ann ] ; decl_pos = 1}); ]
      in
      return @@ O.T_record { fields = (O.LMap.of_list sum) ; attributes = [] }
    | I.T_app c ->
      let c = type_app self c in
      return @@ T_app c
    | I.T_module_accessor ma ->
      let ma = module_access self ma in
      return @@ O.T_module_accessor ma
    | I.T_annoted (ty, _) -> self ty
    | I.T_singleton t -> return @@ O.T_singleton t
    | I.T_abstraction x ->
      let type_ = self x.type_ in
      return @@ O.T_abstraction { x with type_ }
    | I.T_for_all x ->
      let type_ = self x.type_ in
      return @@ O.T_for_all { x with type_ }


let rec compile_expression ~raise ~last : I.expression -> O.expression =
  fun e ->
  let e = compile_expression' ~raise ~last e in
  e None

and compile_expression' ~raise ~last : I.expression -> O.expression option -> O.expression =
  fun e ->
  let self = compile_expression ~raise ~last in
  let self_type = compile_type_expression ~raise in
  let return' expr = function
    | None -> expr
    | Some e -> O.e_sequence expr e
  in
  let return expr = return' @@ O.make_e ~loc:e.location expr in
  match e.expression_content with
    | I.E_literal literal   -> return @@ O.E_literal literal
    | I.E_constant {cons_name;arguments} ->
      let arguments = List.map ~f:(compile_expression ~raise ~last:true) arguments in
      return' @@ O.e_constant ~loc:e.location (Stage_common.Types.const_name cons_name) arguments
    | I.E_variable name     -> return @@ O.E_variable name
    | I.E_application app ->
      let app = application self app in
      return @@ O.E_application app
    | I.E_lambda lamb ->
      let lamb = lambda self self_type lamb in
      return @@ O.E_lambda lamb
    | I.E_type_abstraction ta ->
      let ta = type_abs self ta in
      return @@ O.E_type_abstraction ta
    | I.E_recursive recs ->
      let recs = recursive self self_type recs in
      return @@ O.E_recursive recs
    | I.E_let_in {let_binder;attributes;rhs;let_result} ->
      let let_binder = binder self_type let_binder in
      let rhs = self rhs in
      let let_result = self let_result in
      return @@ O.E_let_in {let_binder;mut=false; attributes; rhs; let_result}
    | I.E_type_in ti ->
      let ti = type_in self self_type ti in
      return @@ O.E_type_in ti
    | I.E_mod_in mi ->
      let mi = mod_in self self_type mi in
      return @@ O.E_mod_in mi
    | I.E_mod_alias ma ->
      let ma = mod_alias self ma in
      return @@ O.E_mod_alias ma
    | I.E_raw_code rc ->
      let rc = raw_code self rc in
      return @@ O.E_raw_code rc
    | I.E_constructor const ->
      let const = constructor self const in
      return @@ O.E_constructor const
    | I.E_matching m ->
      let m = compile_matching ~raise ~last m e.location in
      m
    | I.E_record recd ->
      (* at this point record expression become linear wrt labels *)
      let recd = List.map ~f:(fun (l,e) -> l, self e) recd in
      return @@ O.E_record (O.LMap.of_list recd)
    | I.E_accessor acc ->
      let acc = accessor self acc in
      return @@ O.E_accessor acc
    | I.E_update up ->
      let up = update self up in
      return @@ O.E_update up
    | I.E_map map ->
      let map = List.map ~f:(
        Pair.map ~f:self
      ) map
      in
      return @@ O.E_map map
    | I.E_big_map big_map ->
      let big_map = List.map ~f:(
        Pair.map ~f:self
      ) big_map
      in
      return @@ O.E_big_map big_map
    | I.E_list lst ->
      let lst = List.map ~f:self lst in
      return @@ O.E_list lst
    | I.E_set set ->
      let set = List.map ~f:self set in
      return @@ O.E_set set
    | I.E_ascription ascr ->
      let ascr = ascription self self_type ascr in
      return @@ O.E_ascription ascr
    | I.E_module_accessor ma ->
      let ma = module_access self ma in
      return @@ O.E_module_accessor ma
    | I.E_cond {condition;then_clause;else_clause} ->
      let condition    = self condition in
      let then_clause' = self then_clause in
      let else_clause' = self else_clause in
      let env = (I.ValueVar.fresh ~name:"env" ()) in
      let ((_,free_vars), then_clause,i) = repair_mutable_variable_in_matching then_clause' [] VMap.empty Z.zero env in
      let ((_,free_vars), else_clause,_) = repair_mutable_variable_in_matching else_clause' [] free_vars i env in
      let then_clause  = add_to_end then_clause (O.e_variable env) in
      let else_clause = add_to_end else_clause (O.e_variable env) in

      if (VMap.is_empty free_vars || last)  then
        return' @@ O.e_cond ~loc:e.location condition then_clause' else_clause'
      else
        let cond_expr  = O.e_cond condition then_clause else_clause in
        let return_expr = fun expr ->
          O.e_let_in_ez env false [] (store_mutable_variable free_vars) @@
          O.e_let_in_ez env false [] cond_expr @@
          expr
        in
        restore_mutable_variable return_expr free_vars env
    | I.E_sequence {expr1; expr2} ->
      let expr1 = compile_expression' ~raise ~last:false expr1 in
      let expr2 = compile_expression' ~raise ~last expr2 in
      fun e -> expr1 (Some (expr2 e))
    | I.E_skip -> return @@ O.E_skip
    | I.E_tuple tuple ->
      let tuple = List.map ~f:self tuple in
      return @@ O.E_tuple tuple
    | I.E_assign {variable; access_path; expression} ->
      let access_path = path self access_path in
      let expression = self expression in
      let loc = e.location in
      let rhs = match access_path with
        | [] -> expression
        | _  -> O.e_update ~loc (O.e_variable ~loc variable) access_path expression
      in
      (fun expr_opt ->
        O.e_let_in_ez ~loc variable true [] rhs (Option.value ~default:(O.e_skip ()) expr_opt)
      )
    | I.E_for f ->
      let f = compile_for ~raise ~last f in
      f
    | I.E_for_each fe ->
      let fe = compile_for_each ~raise ~last fe in
      fe
    | I.E_while w ->
      let w = compile_while ~raise ~last w in
      w

and compile_matching ~raise ~last : I.matching -> Location.t -> O.expression option -> O.expression =
  fun {matchee;cases} loc ->
  let return expr = function
    | None -> expr
    | Some e -> O.e_sequence expr e
  in
  let matchee = compile_expression ~raise ~last matchee in
  let env = I.ValueVar.fresh ~name:"env" () in
  let aux : (Z.t VMap.t * Z.t) ->
    _ I.match_case ->(Z.t VMap.t * Z.t) * (_ O.match_case * _ O.match_case) =
    fun (init_fv,i) {pattern ; body} ->
      let body = compile_expression ~raise ~last body in
      let get_pattern_vars : I.expression_variable list -> I.type_expression I.pattern -> I.expression_variable list =
        fun acc p->
          match p.wrap_content with
          | P_var b -> b.var::acc
          | _ -> acc
      in
      let n = Stage_common.Helpers.fold_pattern get_pattern_vars [] pattern in
      let pattern = Stage_common.Helpers.map_pattern_t (binder (compile_type_expression ~raise)) pattern in
      let ((_,free_vars), body',i) = repair_mutable_variable_in_matching body n init_fv i env in
      let body' = add_to_end body' (O.e_variable env) in
      let cases_no_fv : (O.expression, O.type_expression) O.match_case = { pattern ; body } in
      let cases_fv : (O.expression, O.type_expression) O.match_case = { pattern ; body = body' } in
      ((free_vars,i),(cases_no_fv,cases_fv))
  in
  let (free_vars,_),l = List.fold_map ~f:aux cases ~init:(VMap.empty,Z.zero) in
  let (cases_no_fv,cases_fv) = List.unzip @@ l in
  if  (VMap.is_empty free_vars || last ) then
    return (O.e_matching ~loc matchee cases_no_fv)
  else
    let match_expr  = O.e_matching matchee cases_fv in
    let return_expr = fun expr ->
      O.e_let_in_ez env false [] (store_mutable_variable free_vars) @@
      O.e_let_in_ez env false [] match_expr @@
      expr
    in
    restore_mutable_variable return_expr free_vars env

and compile_while ~raise ~last I.{cond;body} =
  let env_rec = I.ValueVar.fresh ~name:"env_rec" () in
  let binder  = I.ValueVar.fresh ~name:"binder"  () in

  let cond = compile_expression ~raise ~last cond in
  let ctrl =
    (O.e_variable binder)
  in

  let for_body = compile_expression ~raise ~last body in
  let ((_,captured_name_list),for_body) = repair_mutable_variable_in_loops for_body [] binder in
  let for_body = add_to_end for_body ctrl in

  let aux name i expr =
    O.e_let_in_ez name false [] (O.e_accessor (O.e_variable binder) [Access_tuple Z.zero; Access_tuple i]) expr
  in
  let init_rec = O.e_tuple [store_mutable_variable @@ captured_name_list] in
  let restore = fun expr -> VMap.fold aux captured_name_list expr in
  let continue_expr = O.e_constant C_LOOP_CONTINUE [for_body] in
  let stop_expr = O.e_constant C_LOOP_STOP [O.e_variable binder] in
  let aux_func =
    O.e_lambda_ez binder None @@
    restore @@
    O.e_cond cond continue_expr stop_expr in
  let loop = O.e_constant C_LOOP_LEFT [aux_func; O.e_variable env_rec] in
  let return_expr = fun expr ->
    O.e_let_in_ez env_rec false [] init_rec @@
    O.e_let_in_ez env_rec false [] loop @@
    O.e_let_in_ez env_rec false [] (O.e_accessor (O.e_variable env_rec) [Access_tuple Z.zero]) @@
    expr
  in
  let return expr = function
    | None -> expr
    | Some e -> O.e_sequence expr e
  in
  if last then
    return (
      O.e_let_in_ez env_rec false [] init_rec @@
      O.e_let_in_ez env_rec false [] loop @@
      (O.e_accessor (O.e_variable env_rec) [Access_tuple Z.zero]))
  else
    restore_mutable_variable return_expr captured_name_list env_rec


and compile_for ~raise ~last I.{binder;start;final;incr;f_body} =
  let env_rec     = I.ValueVar.fresh ~name:"env_rec" () in
  let loop_binder = I.ValueVar.fresh ~name:"loop_binder" () in
  (*Make the cond and the step *)
  let cond = I.e_annotation (I.e_constant (Const C_LE) [I.e_variable binder ; final]) (I.t_bool ()) in
  let cond = compile_expression ~raise ~last cond in
  let step = compile_expression ~raise ~last incr in
  let continue_expr = O.e_constant C_LOOP_CONTINUE [(O.e_variable loop_binder)] in
  let ctrl =
    O.e_let_in_ez binder ~ascr:(O.t_int ()) false [] (O.e_constant C_ADD [ O.e_variable binder ; step ]) @@
    O.e_let_in_ez loop_binder false [] (O.e_update (O.e_variable loop_binder) [Access_tuple Z.one] @@ O.e_variable binder)@@
    continue_expr
  in
  (* Modify the body loop*)
  let body = compile_expression ~raise ~last f_body in
  let ((_,captured_name_list),for_body) = repair_mutable_variable_in_loops body [binder] loop_binder in
  let for_body = add_to_end for_body ctrl in

  let aux name i expr =
    O.e_let_in_ez name false [] (O.e_accessor (O.e_variable loop_binder) [Access_tuple Z.zero; Access_tuple i]) expr
  in

  (* restores the initial value of the free_var*)
  let restore = fun expr -> VMap.fold aux captured_name_list expr in

  (*Prep the lambda for the fold*)
  let stop_expr = O.e_constant C_LOOP_STOP [O.e_variable loop_binder] in
  let aux_func = O.e_lambda_ez loop_binder None @@
                 O.e_let_in_ez binder ~ascr:(O.t_int ()) false [] (O.e_accessor (O.e_variable loop_binder) [Access_tuple Z.one]) @@
                 O.e_cond cond (restore for_body) (stop_expr) in

  (* Make the fold_while en precharge the vakye *)
  let loop = O.e_constant C_LOOP_LEFT [aux_func; O.e_variable env_rec] in
  let init_rec = O.e_pair (store_mutable_variable captured_name_list) @@ O.e_variable binder in

  let start = compile_expression ~raise ~last start in
  let return_expr = fun expr ->
    O.e_let_in_ez binder ~ascr:(O.t_int ()) false [] start @@
    O.e_let_in_ez env_rec false [] init_rec @@
    O.e_let_in_ez env_rec false [] loop @@
    O.e_let_in_ez env_rec false [] (O.e_accessor (O.e_variable env_rec) [Access_tuple Z.zero]) @@
    expr
  in
  let return expr = function
    | None -> expr
    | Some e -> O.e_sequence expr e
  in
  if last then
    return (
      O.e_let_in_ez binder ~ascr:(O.t_int ()) false [] start @@
      O.e_let_in_ez env_rec false [] init_rec @@
      O.e_let_in_ez env_rec false [] loop @@
      (O.e_accessor (O.e_variable env_rec) [Access_tuple Z.zero])
    )
  else
    restore_mutable_variable return_expr captured_name_list env_rec

and compile_for_each ~raise ~last I.{fe_binder;collection;collection_type; fe_body} =
  let env_rec = I.ValueVar.fresh ~name:"env_rec" () in
  let args    = I.ValueVar.fresh ~name:"args" () in

  let element_names = match snd fe_binder with
    | Some v -> [fst fe_binder;v]
    | None -> [fst fe_binder]
  in

  let body = compile_expression ~raise ~last fe_body in
  let ((_,free_vars), body) = repair_mutable_variable_in_loops body element_names args in
  let for_body = add_to_end body @@ (O.e_accessor (O.e_variable args) [Access_tuple Z.zero]) in

  let init_record = store_mutable_variable free_vars in
  let collect = compile_expression ~raise ~last collection in
  let aux name i expr =
    O.e_let_in_ez name false [] (O.e_accessor (O.e_variable args) [Access_tuple Z.zero; Access_tuple i]) expr
  in
  let restore = fun expr -> VMap.fold aux free_vars expr in
  let restore = match collection_type with
    | Map -> (match snd fe_binder with
      | Some v -> fun expr -> restore (O.e_let_in_ez (fst fe_binder) false [] (O.e_accessor (O.e_variable args) [Access_tuple Z.one; Access_tuple Z.zero])
                                    (O.e_let_in_ez v false [] (O.e_accessor (O.e_variable args) [Access_tuple Z.one; Access_tuple Z.one]) expr))
      | None -> fun expr -> restore (O.e_let_in_ez (fst fe_binder) false [] (O.e_accessor (O.e_variable args) [Access_tuple Z.one; Access_tuple Z.zero]) expr)
    )
    | _ -> fun expr -> restore (O.e_let_in_ez (fst fe_binder) false [] (O.e_accessor (O.e_variable args) [Access_tuple Z.one]) expr)
  in
  let lambda = O.e_lambda_ez args None (restore for_body) in
  let op_name =
    match collection_type with
   | Map -> O.C_MAP_FOLD | Set -> O.C_SET_FOLD | List -> O.C_LIST_FOLD | Any -> O.C_FOLD
  in
  let fold = fun expr ->
    O.e_let_in_ez env_rec false [] (O.e_constant op_name [lambda; collect ; init_record]) expr
  in
  let return expr = function
    | None -> expr
    | Some e -> O.e_sequence expr e
  in
  if last then
    return (O.e_constant op_name [lambda; collect ; init_record])
  else
    restore_mutable_variable fold free_vars env_rec


and compile_module ~raise : I.module_ -> O.module_ = fun m ->
  module' (compile_expression ~raise ~last:true) (compile_type_expression ~raise) m
