open Errors
open Trace
open Function

module CST = Cst.Pascaligo
module AST = Ast_imperative

open AST

let nseq_to_list (hd, tl) = hd :: tl
let npseq_to_list (hd, tl) = hd :: (List.map snd tl)
let npseq_to_ne_list (hd, tl) = (hd, List.map snd tl)

open Predefined.Tree_abstraction.Pascaligo

let r_split = Location.r_split


let rec compile_type_expression : CST.type_expr -> _ result = fun te ->
  let return te = ok @@ te in
  match te with
    TSum sum ->
    let (nsepseq, loc) = r_split sum in
    let lst = npseq_to_list nsepseq in
    let aux (variant : CST.variant CST.reg) =
      let (v, _) = r_split variant in
      let%bind type_expr = bind_map_option (compile_type_expression <@ snd) v.arg in
      let type_expr = Option.unopt ~default:(t_unit ()) type_expr in
      ok @@ (v.constr.value,type_expr)
    in
    let%bind sum = bind_map_list aux lst in
    return @@ t_sum_ez ~loc sum
  | TRecord record ->
    let (nsepseq, loc) = r_split record in
    let lst = npseq_to_list nsepseq.ne_elements in
    let aux (field : CST.field_decl CST.reg) =
      let (f, _) = r_split field in
      let%bind type_expr = compile_type_expression f.field_type in
      return @@ (f.field_name.value,type_expr)
    in
    let%bind record = bind_map_list aux lst in
    return @@ t_record_ez ~loc record
  | TProd prod ->
    let (nsepseq, loc) = r_split prod in
    let lst = npseq_to_list nsepseq in
    let%bind lst = bind_map_list compile_type_expression lst in
    return @@ t_tuple ~loc lst
  | TApp app ->
    let get_t_string_singleton_opt = function
      | CST.TString s -> Some s.value
      | _ -> None
    in
    let ((operator,args), loc) = r_split app in
    (* this is a bad design, michelson_or and pair should be an operator
       see AnnotType *)
    (match operator.value with
      | "michelson_or" ->
        let lst = npseq_to_list args.value.inside in
        (match lst with
        | [a ; b ; c ; d ] -> (
          let%bind b' =
            trace_option (michelson_type_wrong te operator.value) @@
              get_t_string_singleton_opt b in
          let%bind d' =
            trace_option (michelson_type_wrong te operator.value) @@
              get_t_string_singleton_opt d in
          let%bind a' = compile_type_expression a in
          let%bind c' = compile_type_expression c in
          return @@ t_michelson_or ~loc a' b' c' d'
          )
        | _ -> fail @@ michelson_type_wrong_arity loc operator.value)
      | "michelson_pair" ->
        let lst = npseq_to_list args.value.inside in
        (match lst with
        | [a ; b ; c ; d ] -> (
          let%bind b' =
            trace_option (michelson_type_wrong te operator.value) @@
              get_t_string_singleton_opt b in
          let%bind d' =
            trace_option (michelson_type_wrong te operator.value) @@
              get_t_string_singleton_opt d in
          let%bind a' = compile_type_expression a in
          let%bind c' = compile_type_expression c in
          return @@ t_michelson_pair ~loc a' b' c' d'
          )
        | _ -> fail @@ michelson_type_wrong_arity loc operator.value)
    | _ ->
      let%bind operators =
        trace_option (unknown_predefined_type operator) @@
        type_operators operator.value in
      let lst = npseq_to_list args.value.inside in
      let%bind lst = bind_map_list compile_type_expression lst in
      return @@ t_operator ~loc operators lst
    )
  | TFun func ->
    let ((input_type,_,output_type), loc) = r_split func in
    let%bind input_type = compile_type_expression input_type in
    let%bind output_type = compile_type_expression output_type in
    return @@ t_function ~loc input_type output_type
  | TPar par ->
    let (par, _) = r_split par in
    let type_expr = par.inside in
    compile_type_expression type_expr
  | TVar var ->
    let (name,loc) = r_split var in
    (match type_constants name with
      Some const -> return @@ t_constant ~loc const
    | None -> return @@ t_variable_ez ~loc name
    )
  | TWild reg ->
    let loc = Location.lift reg in
    return @@ make_t ~loc @@ T_wildcard
  | TString _s -> fail @@ unsupported_string_singleton te

let compile_selection (selection : CST.selection) =
  match selection with
    FieldName name ->
    let (name, loc) = r_split name in
    (Access_record name, loc)
  | Component comp ->
    let ((_,index), loc) = r_split comp in
    (Access_tuple index, loc)

let rec compile_expression : CST.expr -> (AST.expr , abs_error) result = fun e ->
  let return e = ok @@ e in
  let compile_tuple_expression (tuple_expr : CST.tuple_expr) =
    let (lst, loc) = r_split tuple_expr in
    let%bind lst = bind_map_list compile_expression @@ npseq_to_list lst.inside in
    match lst with
      hd::[] -> return hd
    | lst -> return @@ e_tuple ~loc lst
  in
  let compile_path (path : CST.path) =
    match path with
      Name var ->
        let (var, loc) = r_split var in
        return @@ e_variable_ez ~loc var
    | Path proj ->
        let (proj, loc) = r_split proj in
        let (var, _loc_var) = r_split proj.struct_name in
        let var  = e_variable_ez ~loc var in
        let (sels, _) = List.split @@ List.map compile_selection @@ npseq_to_list proj.field_path in
        return @@ e_accessor var sels
  in
  let compile_bin_op (op_type : AST.constant') (op : _ CST.bin_op CST.reg) =
    let (op, loc) = r_split op in
    let%bind a = compile_expression op.arg1 in
    let%bind b = compile_expression op.arg2 in
    return @@ e_constant ~loc op_type [a; b]
  in
  let compile_un_op (op_type : AST.constant') (op : _ CST.un_op CST.reg) =
    let (op, loc) = r_split op in
    let%bind arg = compile_expression op.arg in
    return @@ e_constant ~loc op_type [arg]
  in
  match e with
    EVar var ->
    let (var, loc) = r_split var in
    (match constants var with
      Some const -> return @@ e_constant ~loc const []
    | None -> return @@ e_variable_ez ~loc var
    )
  | EPar par -> compile_expression par.value.inside
  | EUnit reg ->
    let loc = Location.lift reg in
    return @@ e_unit ~loc ()
  | EBytes bytes ->
    let (bytes, loc) = r_split bytes in
    let (_s,b) = bytes in
    return @@ e_bytes_hex ~loc b
  | EString str ->(
    match str with
      Cat c ->
      let (op,loc) = r_split c in
      let%bind a = compile_expression op.arg1 in
      let%bind b = compile_expression op.arg2 in
      return @@ e_constant ~loc C_CONCAT [a;b]
    | String str ->
      let (str, loc) = r_split str in
      return @@ e_string ~loc str
    | Verbatim str ->
      let (str, loc) = r_split str in
      return @@ e_verbatim ~loc str
  )
  | EArith arth ->
    ( match arth with
      Add plus   -> compile_bin_op C_ADD plus
    | Sub minus  -> compile_bin_op C_SUB minus
    | Mult times -> compile_bin_op C_MUL times
    | Div slash  -> compile_bin_op C_DIV slash
    | Mod mod_   -> compile_bin_op C_MOD mod_
    | Neg minus  -> compile_un_op C_NEG minus
    | Int i ->
      let ((_,i), loc) = r_split i in
      return @@ e_int_z ~loc i
    | Nat n ->
      let ((_,n), loc) = r_split n in
      return @@ e_nat_z ~loc n
    | Mutez mtez ->
      let ((_,mtez), loc) = r_split mtez in
      return @@ e_mutez_z ~loc mtez
    )
  | ELogic logic -> (
    match logic with
      BoolExpr be -> (
      match be with
        Or or_   -> compile_bin_op C_OR  or_
      | And and_ -> compile_bin_op C_AND and_
      | Not not_ -> compile_un_op  C_NOT not_
      | True  reg -> let loc = Location.lift reg in return @@ e_true  ~loc ()
      | False reg -> let loc = Location.lift reg in return @@ e_false ~loc ()
    )
    | CompExpr ce -> (
      match ce with
        Lt lt    -> compile_bin_op C_LT  lt
      | Leq le   -> compile_bin_op C_LE  le
      | Gt gt    -> compile_bin_op C_GT  gt
      | Geq ge   -> compile_bin_op C_GE  ge
      | Equal eq -> compile_bin_op C_EQ  eq
      | Neq ne   -> compile_bin_op C_NEQ ne
    )
  )
  (* This case is due to a bad besign of our constant it as to change
    with the new typer so LIGO-684 on Jira *)
  | ECall {value=(EVar var,args);region} ->
    let loc = Location.lift region in
    let (var, loc_var) = r_split var in
    (match constants var with
      Some const ->
      let (args, _) = r_split args in
      let%bind args = bind_map_list compile_expression @@ npseq_to_list args.inside in
      return @@ e_constant ~loc const args
    | None ->
      let func = e_variable_ez ~loc:loc_var var in
      let%bind args = compile_tuple_expression args in
      return @@ e_application ~loc func args
    )
  | ECall call ->
    let ((func, args), loc) = r_split call in
    let%bind func = compile_expression func in
    let%bind args = compile_tuple_expression args in
    return @@ e_application ~loc func args
  | ETuple lst ->
    compile_tuple_expression lst
  | ERecord record ->
    let (record, loc) = r_split record in
    let aux (fa : CST.field_assignment CST.reg) =
      let (fa, _) = r_split fa in
      let (name, _) = r_split fa.field_name in
      let%bind expr = compile_expression fa.field_expr in
      return (name, expr)
    in
    let%bind record = bind_map_list aux @@ npseq_to_list record.ne_elements in
    return @@ e_record_ez ~loc record
  | EProj proj ->
    let (proj, loc) = r_split proj in
    let (var, _loc_var) = r_split proj.struct_name in
    let var  = e_variable_ez ~loc var in
    let (sels, _) = List.split @@ List.map compile_selection @@ npseq_to_list proj.field_path in
    return @@ e_accessor var sels
  | EUpdate update ->
    let (update, _loc) = r_split update in
    let%bind record = compile_path update.record in
    let (updates, _loc) = r_split update.updates in
    let aux (up : CST.field_path_assignment CST.reg) =
      let (up, loc) = r_split up in
      let path = up.field_path in
      let%bind expr = compile_expression up.field_expr in
      let path = (match path with
        Name var -> [Access_record var.value]
      | Path proj ->
        let (proj, _) = r_split proj in
        let (path, _) = List.split @@ List.map compile_selection @@ npseq_to_list proj.field_path in
        (Access_record proj.struct_name.value)::path
      )
      in
      return (path, expr, loc)
    in
    let%bind updates = bind_map_list aux @@ npseq_to_list updates.ne_elements in
    let aux e (path, update, loc) = e_update ~loc e path update in
    return @@ List.fold_left aux record updates
  | EFun func ->
    let compile_param (param : CST.param_decl) =
      match param with
        ParamConst p ->
        let (p, _) = r_split p in
        let (var, loc) = r_split p.var in
        let%bind p_type = bind_map_option (compile_type_expression <@ snd) p.param_type in
        return (Location.wrap ?loc:(Some loc) @@ Var.of_name var, p_type)
      | ParamVar p ->
        let (p, _) = r_split p in
        let (var, loc) = r_split p.var in
        let%bind p_type = bind_map_option (compile_type_expression <@ snd) p.param_type in
        return (Location.wrap ?loc:(Some loc) @@ Var.of_name var, p_type)
    in
    let (func, loc) = r_split func in
    let (param, loc_par)  = r_split func.param in
    let%bind param = bind_map_list compile_param @@ npseq_to_list param.inside in
    let (param, param_type) = List.split param in
    let%bind ret_type = bind_map_option (compile_type_expression <@ snd )func.ret_type in
    let%bind body = compile_expression func.return in
    let (lambda, fun_type) = match param_type with
      ty::[] ->
      e_lambda ~loc (List.hd param) ty ret_type body,
      Option.map (fun (a,b) -> t_function a b)@@ Option.bind_pair (ty,ret_type)
    (* Cannot be empty *)
    | lst ->
      let lst = Option.bind_list lst in
      let input_type = Option.map t_tuple lst in
      let binder = Location.wrap ?loc:(Some loc_par) @@ Var.fresh ~name:"parameter" () in
      e_lambda ~loc binder input_type (ret_type) @@
        e_matching_tuple ~loc:loc_par (e_variable binder) param lst body,
      Option.map (fun (a,b) -> t_function a b)@@ Option.bind_pair (input_type,ret_type)
    in
    return @@ Option.unopt ~default:lambda @@
      Option.map (e_annotation ~loc lambda) fun_type
  | EConstr (SomeApp some) ->
    let ((_, arg), loc) = r_split some in
    let%bind args = compile_tuple_expression arg in
    return @@ e_some ~loc args
  | EConstr (NoneExpr reg) ->
    let loc = Location.lift reg in
    return @@ e_none ~loc ()
  | EConstr (ConstrApp constr) ->
    let ((constr,args_o), loc) = r_split constr in
    let%bind args_o = bind_map_option compile_tuple_expression args_o in
    let args = Option.unopt ~default:(e_unit ~loc:(Location.lift constr.region) ()) args_o in
    return @@ e_constructor ~loc constr.value args
  | ECase case ->
    let (case, loc) = r_split case in
    let%bind matchee = compile_expression case.expr in
    let (cases, _) = r_split case.cases in
    let%bind cases = compile_matching_expr compile_expression @@ npseq_to_ne_list cases in
    return @@ e_matching ~loc matchee cases
  | EAnnot annot ->
    let (annot, loc) = r_split annot in
    let (expr, _, ty) = annot.inside in
    let%bind expr = compile_expression expr in
    let%bind ty   = compile_type_expression ty in
    return @@ e_annotation ~loc expr ty
  | ECond cond ->
    let (cond, loc) = r_split cond in
    let%bind test        = compile_expression cond.test in
    let%bind then_clause = compile_expression cond.ifso in
    let%bind else_clause = compile_expression cond.ifnot in
    return @@ e_cond ~loc test then_clause else_clause
  | EList lst -> (
    match lst with
      ECons cons ->
      let (cons, loc) = r_split cons in
      let%bind a  = compile_expression cons.arg1 in
      let%bind b  = compile_expression cons.arg2 in
      return @@ e_constant ~loc C_CONS [a; b]
    | EListComp lc ->
      let (lc,loc) = r_split lc in
      let lst =
        Option.unopt ~default:[] @@
        Option.map npseq_to_list lc.elements
      in
      let%bind lst = bind_map_list compile_expression lst in
      return @@ e_list ~loc lst
    | ENil nil ->
      let loc = Location.lift nil in
      return @@ e_list ~loc []
      (* Is seems that either ENil is redondant or EListComp should be an nsepseq and not a sepseq  *)
  )
  | ESet set -> (
    match set with
      SetInj si ->
      let (si, loc) = r_split si in
      let set =
        Option.unopt ~default:[] @@
        Option.map npseq_to_list si.elements
      in
      let%bind set = bind_map_list compile_expression set in
      return @@ e_set ~loc set
    | SetMem sm ->
      let (sm, loc) = r_split sm in
      let%bind set  = compile_expression sm.set in
      let%bind elem = compile_expression sm.element in
      return @@ e_constant ~loc C_SET_MEM [elem;set]
  )
  | EMap map -> (
    match map with
      MapLookUp mlu ->

        let (mlu, loc) = r_split mlu in
        let%bind path  = compile_path mlu.path in
        let (index, _) = r_split mlu.index in
        let%bind index = compile_expression index.inside in
        return @@ e_accessor ~loc path [Access_map index]
    | MapInj mij ->
      let (mij, loc) = r_split mij in
      let lst = Option.unopt ~default:[] @@
        Option.map npseq_to_list mij.elements in
      let aux (binding : CST.binding CST.reg) =
        let (binding, _) = r_split binding in
        let%bind key   = compile_expression binding.source in
        let%bind value = compile_expression binding.image in
        return (key,value)
      in
      let%bind map = bind_map_list aux lst in
      return @@ e_map ~loc map
    | BigMapInj mij ->
      let (mij, loc) = r_split mij in
      let lst = Option.unopt ~default:[] @@
        Option.map npseq_to_list mij.elements in
      let aux (binding : CST.binding CST.reg) =
        let (binding, _) = r_split binding in
        let%bind key   = compile_expression binding.source in
        let%bind value = compile_expression binding.image in
        return (key,value)
      in
      let%bind map = bind_map_list aux lst in
      return @@ e_big_map ~loc map
  )
  | ECodeInj ci ->
    let (ci, loc) = r_split ci in
    let (language, _) = r_split ci.language in
    let (language, _) = r_split language in
    let%bind code = compile_expression ci.code in
    return @@ e_raw_code ~loc language code
  | EBlock be ->
    let be, _ = r_split be in
    let%bind next = compile_expression be.expr in
    compile_block ~next be.block


and compile_matching_expr : type a.(a -> _ result) -> a CST.case_clause CST.reg List.Ne.t -> _ =
fun compiler cases ->
  let return = ok in
  let compile_pattern pattern = return pattern in
  let compile_simple_pattern (pattern : CST.pattern) =
    match pattern with
      PVar var ->
        let (var, _) = r_split var in
        return @@ Var.of_name var
    | _ -> fail @@ unsupported_non_var_pattern pattern
  in
  let compile_list_pattern (cases : (CST.pattern * _) list) =
    match cases with
      [(PList PNil _, match_nil);(PList PCons cons, econs)]
    | [(PList PCons cons, econs);(PList PNil _, match_nil)] ->
      let (cons,_) = r_split cons in
      let%bind (hd,tl) = match snd @@ List.split (snd cons) with
        tl::[] -> return (fst cons,tl)
      | _ -> fail @@ unsupported_deep_list_patterns @@ fst cons
      in
      let hd_loc = Location.lift @@ Raw.pattern_to_region hd in
      let tl_loc = Location.lift @@ Raw.pattern_to_region hd in
      let%bind (hd,tl) = bind_map_pair compile_simple_pattern (hd,tl) in
      let hd = Location.wrap ?loc:(Some hd_loc) hd in
      let tl = Location.wrap ?loc:(Some tl_loc) tl in
      let match_cons = (hd,tl,econs) in
        return (match_nil,match_cons)
    | _ -> fail @@ unsupported_deep_list_patterns @@ fst @@ List.hd cases
  in
  let compile_simple_tuple_pattern (tuple : CST.tuple_pattern) =
    let (lst, _) = r_split tuple in
    match lst.inside with
      hd,[] -> compile_simple_pattern hd
    | _ -> fail @@ unsupported_deep_tuple_patterns tuple
  in
  let compile_constr_pattern (constr : CST.pattern) =
    match constr with
      PConstr c ->
      ( match c with
        PUnit _ ->
         fail @@ unsupported_pattern_type constr
      | PFalse _ -> return (Label "false", Location.wrap @@ Var.of_name "_")
      | PTrue  _ -> return (Label "true", Location.wrap @@ Var.of_name "_")
      | PNone  _ -> return (Label "None", Location.wrap @@ Var.of_name "_")
      | PSomeApp some ->
        let (some,_) = r_split some in
        let (_, pattern) = some in
        let (pattern,loc) = r_split pattern in
        let%bind pattern = compile_simple_pattern pattern.inside in
        return (Label "Some", Location.wrap ~loc pattern)
      | PConstrApp constr ->
        let (constr, _) = r_split constr in
        let (constr, patterns) = constr in
        let (constr, _) = r_split constr in
        let pattern_loc = match patterns with
          | Some (v:CST.tuple_pattern) -> Location.lift v.region
          | None -> Location.generated in
        let%bind pattern = bind_map_option compile_simple_tuple_pattern patterns in
        let pattern = Location.wrap ?loc:(Some pattern_loc) @@ Option.unopt ~default:(Var.of_name "_") pattern in
        return (Label constr, pattern)
    )
    | _ -> fail @@ unsupported_pattern_type constr
  in
  let aux (case : a CST.case_clause CST.reg) =
    let (case, _loc) = r_split case in
    let%bind pattern = compile_pattern case.pattern in
    let%bind expr    = compiler case.rhs in
    return (pattern, expr)
  in
  let%bind cases = bind_map_ne_list aux cases in
  match cases with
  | (PVar var, expr), [] ->
    let (var, loc) = r_split var in
    let var = Location.wrap ?loc:(Some loc) @@ Var.of_name var in
    return @@ AST.Match_variable (var, None, expr)
  | (PTuple tuple, _expr), [] ->
    fail @@ unsupported_tuple_pattern @@ CST.PTuple tuple
  | (PList _, _), _ ->
    let%bind (match_nil,match_cons) = compile_list_pattern @@ List.Ne.to_list cases in
    return @@ AST.Match_list {match_nil;match_cons}
  | (PConstr _,_), _ ->
    let (pattern, lst) = List.split @@ List.Ne.to_list cases in
    let%bind constrs = bind_map_list compile_constr_pattern pattern in
    return @@ AST.Match_variant (List.combine constrs lst)
  | (p, _), _ -> fail @@ unsupported_pattern_type p

and compile_attribute_declaration = function
  None   -> false
| Some _ -> true

and compile_parameters (params : CST.parameters) =
  let compile_param_decl (param : CST.param_decl) =
    let return = ok in
    match param with
      ParamConst pc ->
      let (pc, _loc) = r_split pc in
      let (var, loc) = r_split pc.var in
      let var = Location.wrap ?loc:(Some loc) @@ Var.of_name var in
      let%bind param_type = bind_map_option (compile_type_expression <@ snd) pc.param_type in
      return (var, param_type)
    | ParamVar pv ->
      let (pv, _loc) = r_split pv in
      let (var, loc) = r_split pv.var in
      let var = Location.wrap ?loc:(Some loc) @@ Var.of_name var in
      let%bind param_type = bind_map_option (compile_type_expression <@ snd) pv.param_type in
      return (var, param_type)
  in
  let (params, _loc) = r_split params in
  let params = npseq_to_list params.inside in
  bind_map_list compile_param_decl params

and compile_instruction : ?next: AST.expression -> CST.instruction -> _ result  = fun ?next instruction ->
  let return expr = match next with 
    Some e -> ok @@ e_sequence expr e
  | None -> ok @@ expr 
  in
  let compile_tuple_expression (tuple_expr : CST.tuple_expr) =
    let (lst, loc) = r_split tuple_expr in
    let%bind lst = bind_map_list compile_expression @@ npseq_to_list lst.inside in
    match lst with
      hd::[] -> ok hd
    | lst -> ok @@ e_tuple ~loc lst
  in
  let compile_if_clause : ?next:AST.expression -> CST.if_clause -> _ = fun ?next if_clause ->
    match if_clause with
      ClauseInstr i -> compile_instruction ?next i
    | ClauseBlock (LongBlock  block) -> compile_block ?next block
    | ClauseBlock (ShortBlock block) ->
      (* This looks like it should be the job of the parser *)
      let CST.{lbrace; inside; rbrace} = block.value in
      let region = block.region in
      let enclosing = CST.Block (Region.ghost, lbrace, rbrace)
      and (statements,terminator) = inside in
      let value = CST.{enclosing;statements;terminator} in
      let block : _ CST.reg = {value; region} in
      compile_block ?next block

  in
  let compile_path : CST.path -> _ = fun path ->
    match path with
      Name var ->
      let (var,loc) = r_split var in
      let str = e_variable_ez ~loc var in
      ok (str, var, [])
    | Path proj ->
      let (proj, loc) = r_split proj in
      let (var, loc_var) = r_split proj.struct_name in
      let path = List.map compile_selection @@ npseq_to_list proj.field_path in
      let (path, _) = List.split path in
      let str = e_accessor ~loc (e_variable_ez ~loc:loc_var var) path in
      ok (str, var, path)
  in
  let compile_lhs : CST.lhs -> _ = fun lhs ->
    match lhs with
    | Path path ->
      let%bind (_, var, path) = compile_path path in
      ok @@ (var, path)
    | MapPath (mlu) ->
      let (mlu, _loc) = r_split mlu in
      let%bind (_, var, path) = compile_path mlu.path in
      let%bind index = compile_expression @@ mlu.index.value.inside in
      ok @@ (var, path @ [Access_map index])
  in
  match instruction with
    Cond c ->
    let (c, loc) = r_split c in
    let%bind test = compile_expression c.test in
    let%bind ifso = compile_if_clause c.ifso in
    let%bind ifnot = compile_if_clause c.ifnot in
    return @@ e_cond ~loc test ifso ifnot
  | CaseInstr ci ->
    let (ci, loc) = r_split ci in
    let%bind matchee = compile_expression ci.expr in
    let%bind cases = compile_matching_expr compile_if_clause @@ npseq_to_ne_list ci.cases.value in
    return @@ e_matching ~loc matchee cases
  | Assign a ->
    let (a,loc) = r_split a in
    let%bind (var,path) = compile_lhs a.lhs in
    let%bind rhs = compile_expression a.rhs in
    return @@ e_assign_ez ~loc var path rhs
  | Loop (While wl) ->
    let (wl, loc) = r_split wl in
    let%bind cond = compile_expression wl.cond in
    let%bind body = compile_block wl.block in
    return @@ e_while ~loc cond body
  | Loop (For (ForInt fl)) ->
    let (fl, loc) = r_split fl in
    let (binder, binder_loc) = r_split fl.binder in
    let%bind start = compile_expression fl.init in
    let%bind bound = compile_expression fl.bound in
    let%bind increment = Option.unopt ~default:(ok @@ e_int_z Z.one) @@
      Option.map (compile_expression <@ snd) fl.step
    in
    let%bind body  = compile_block fl.block in
    return @@ e_for ~loc (Location.wrap ?loc:(Some binder_loc) @@ Var.of_name binder) start bound increment body
  | Loop (For (ForCollect el)) ->
    let (el, loc) = r_split el in
    let binder =
      let (key, loc) = r_split el.var in
      let key' = Location.wrap ?loc:(Some loc) @@ Var.of_name key in
      let value = Option.map
        (fun x -> 
          let (v,loc) = r_split (snd x) in
          Location.wrap ?loc:(Some loc) @@ Var.of_name v)
        el.bind_to in
      (key',value)
    in
    let%bind collection = compile_expression el.expr in
    let (collection_type, _) = match el.collection with
      Map loc -> (Map, loc) | Set loc -> (Set, loc) | List loc -> (List, loc)
    in
    let%bind body = compile_block el.block in
    return @@ e_for_each ~loc binder collection collection_type body
  | ProcCall {value=(EVar var,args);region} ->
    let loc = Location.lift region in
    let (var, loc_var) = r_split var in
    (match constants var with
      Some const ->
      let (args, _) = r_split args in
      let%bind args = bind_map_list compile_expression @@ npseq_to_list args.inside in
      return @@ e_constant ~loc const args
    | None ->
      let func = e_variable_ez ~loc:loc_var var in
      let%bind args = compile_tuple_expression args in
      return @@ e_application ~loc func args
    )
  | ProcCall pc ->
    let (pc, loc) = r_split pc in
    let (func, args) = pc in
    let%bind func = compile_expression func in
    let%bind args = compile_tuple_expression args in
    return @@ e_application ~loc func args
  | Skip s ->
    let loc = Location.lift s in
    return @@ e_skip ~loc ()
  | RecordPatch rp ->
    let (rp, loc) = r_split rp in
    let%bind (record, var, path) = compile_path rp.path in
    let (updates, _) = r_split rp.record_inj in
    let updates = npseq_to_list updates.ne_elements in
    let aux record (update: CST.field_assignment CST.reg) =
      let (update,loc) = r_split update in
      let path = [Access_record update.field_name.value] in
      let%bind expr = compile_expression update.field_expr in
      ok @@ e_update ~loc record path expr
    in
    let%bind new_record = bind_fold_list aux record updates in
    return @@ e_assign_ez ~loc var path @@ new_record
  | MapPatch mp ->
    let (mp, loc) = r_split mp in
    let%bind (map, var, path) = compile_path mp.path in
    let (updates, _) = r_split mp.map_inj in
    let updates = npseq_to_list updates.ne_elements in
    let aux map (update: CST.binding CST.reg) =
      let (update,loc) = r_split update in
      let%bind key = compile_expression update.source in
      let%bind value = compile_expression update.image in
      ok @@ e_map_add ~loc key value map
    in
    let%bind new_map = bind_fold_list aux map updates in
    return @@ e_assign_ez ~loc var path @@ new_map
  | SetPatch sp ->
    let (sp, loc) = r_split sp in
    let%bind (set, var, path) = compile_path sp.path in
    let (updates, _) = r_split sp.set_inj in
    let updates = npseq_to_list updates.ne_elements in
    let aux set (update: CST.expr) =
      let%bind key = compile_expression update in
      ok @@ e_constant ~loc C_SET_ADD [key; set]
    in
    let%bind new_map = bind_fold_list aux set updates in
    return @@ e_assign_ez ~loc var path @@ new_map
  | MapRemove mr ->
    let (mr, loc) = r_split mr in
    let%bind (map, var, path) = compile_path mr.map in
    let%bind key = compile_expression mr.key in
    return @@ e_assign_ez ~loc var path @@
      e_constant ~loc C_MAP_REMOVE [key;map]
  | SetRemove sr ->
    let (sr, loc) = r_split sr in
    let%bind (set, var, path)  = compile_path sr.set in
    let%bind ele  = compile_expression sr.element in
    return @@ e_assign_ez ~loc var path @@
      e_constant ~loc C_SET_REMOVE [ele;set]

and compile_data_declaration : next:AST.expression -> ?attr:CST.attr_decl -> CST.data_decl -> _ = fun ~next ?attr data_decl ->
  let return loc name type_ init =
    let attr = compile_attribute_declaration attr in
    ok @@ e_let_in ~loc (name,type_) attr init next in
  match data_decl with
    LocalConst const_decl ->
    let (cd, loc) = r_split const_decl in
    let (name, ploc) = r_split cd.name in
    let%bind type_ = bind_map_option (compile_type_expression <@ snd)cd.const_type in
    let%bind init = compile_expression cd.init in
    let p = Location.wrap ?loc:(Some ploc) @@ Var.of_name name in
    return loc p type_ init
  | LocalVar var_decl ->
    let (vd, loc) = r_split var_decl in
    let (name, ploc) = r_split vd.name in
    let%bind type_ = bind_map_option (compile_type_expression <@ snd) vd.var_type in
    let%bind init = compile_expression vd.init in
    let p = Location.wrap ?loc:(Some ploc) @@ Var.of_name name in
    return loc p type_ init
  | LocalFun fun_decl ->
    let (fun_decl,loc) = r_split fun_decl in
    let%bind (fun_name,fun_type,_attr,lambda) = compile_fun_decl fun_decl in
    return loc fun_name fun_type lambda

and compile_statement : ?next:AST.expression -> CST.attr_decl option -> CST.statement -> _ result = fun ?next attr statement ->
  let return = ok in
  match statement with
    Instr i ->
      let%bind i = compile_instruction ?next i in
      return (Some i, None)
  | Data dd ->
    let next = Option.unopt ~default:(e_skip ()) next in
    let%bind dd = compile_data_declaration ~next ?attr dd in
    return (Some dd, None)
  | Attr at -> return (next, Some at)

and compile_block : ?next:AST.expression -> CST.block CST.reg -> _ result = fun ?next block ->
  let return = ok in
  let (block', _loc) = r_split block in
  let statements = npseq_to_list block'.statements in
  let aux (next,attr) statement =
    let%bind (statement, attr) = compile_statement ?next attr statement
    in return (statement,attr)
  in
  let%bind (block', _) = bind_fold_right_list aux (next,None) statements in
  match block' with
    Some block -> return block
  | None -> fail @@ block_start_with_attribute block

and compile_fun_decl ({kwd_recursive; fun_name; param; ret_type; return=r; attributes}: CST.fun_decl) =
  let return = ok in
  let attr = compile_attribute_declaration attributes in
  let (fun_name, loc) = r_split fun_name in
  let fun_binder = Location.wrap ?loc:(Some loc) @@ Var.of_name fun_name in
  let%bind ret_type = bind_map_option (compile_type_expression <@ snd) ret_type in
  let%bind param = compile_parameters param in
  let%bind result    = compile_expression r in
  let (param, param_type) = List.split param in
  (* This handle the parameter case *)
  let (lambda,fun_type) = (match param_type with
    ty::[] ->
    let lambda : AST.lambda = {
      binder = List.hd param;
      input_type  = ty ;
      output_type = ret_type ;
      result;
    } in
    lambda,Option.map (fun (a,b) -> t_function a b)@@ Option.bind_pair (ty,ret_type)
  | lst ->
    let lst = Option.bind_list lst in
    let input_type = Option.map t_tuple lst in
    let binder = Location.wrap @@ Var.fresh ~name:"parameters" () in
    let lambda : AST.lambda = {
      binder;
      input_type = input_type;
      output_type = ret_type;
      result = e_matching_tuple (e_variable binder) param lst result;
    } in
    lambda,Option.map (fun (a,b) -> t_function a b) @@ Option.bind_pair (input_type,ret_type)
  )
  in
  (* This handle the recursion *)
  let%bind func = match kwd_recursive with
    Some reg ->
      let%bind fun_type = trace_option (untyped_recursive_fun loc) @@ fun_type in
      return @@ e_recursive ~loc:(Location.lift reg) fun_binder fun_type lambda
  | None   ->
      return @@ make_e ~loc @@ E_lambda lambda
  in
  return (fun_binder,fun_type, attr, func)

(* Currently attributes are badly proccess, some adaptation are made to accomodate this
  maked as ATR *)
let compile_declaration : (CST.attr_decl option * _) -> CST.declaration -> _ = fun (attr, lst) decl ->
  let return ?attr reg decl =
    ok @@ (attr, (Location.wrap ~loc:(Location.lift reg) decl)::lst) in (*ATR*)
  match decl with
    TypeDecl {value={name; type_expr; _};region} ->
    (* Todo : if attr isn't none, send warning *)
    let (name,_) = r_split name in
    let%bind type_expr = compile_type_expression type_expr in
    return region @@ AST.Declaration_type (Var.of_name name, type_expr)
  | ConstDecl {value={name; const_type; init; attributes=_};region} ->
    let (name, loc) = r_split name in
    let name = Location.wrap ?loc:(Some loc) @@ Var.of_name name in
    let attributes = attr in (*ATR*)
    let%bind const_type = bind_map_option (compile_type_expression <@ snd) const_type in
    let%bind init = compile_expression init in
    let      attr = compile_attribute_declaration attributes in
    return region @@ AST.Declaration_constant (name, const_type,attr,init)
  | FunDecl {value;region} ->
    let value = {value with attributes = attr} in (*ATR*)
    let%bind (fun_name,fun_type,attr,lambda) = compile_fun_decl value in
    return region @@ AST.Declaration_constant (fun_name, fun_type, attr, lambda)
  | AttrDecl decl -> ok (Some decl, lst) (*ATR*)

(* This should be change to the commented function when attributes are fixed
let compile_program : CST.ast -> _ result = fun t ->
    bind_map_list compile_declaration @@ nseq_to_list t.decl
 *)
let compile_program : CST.ast -> _ result =
  fun t ->
  let return = ok in
  let declarations = List.rev @@ nseq_to_list t.decl in
  let attr = (None, []) in
  let%bind (_, declarations) = bind_fold_list compile_declaration attr declarations in
  return declarations
