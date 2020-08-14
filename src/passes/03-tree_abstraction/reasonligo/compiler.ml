open Errors
open Trace
open Function

module CST = Cst.Reasonligo
module AST = Ast_imperative
(* TODO: move 1-parser/shared/Utils.ml{i} to Simple_utils/ *)

open AST

let nseq_to_list (hd, tl) = hd :: tl
let npseq_to_list (hd, tl) = hd :: (List.map snd tl)
let npseq_to_ne_list (hd, tl) = hd, (List.map snd tl)
let pseq_to_list = function
  | None -> []
  | Some lst -> npseq_to_list lst
let get_value : 'a Raw.reg -> 'a = fun x -> x.value

open Predefined.Tree_abstraction.Cameligo

let r_split = Location.r_split

let compile_variable var = Location.map Var.of_name @@ Location.lift_region var

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
        type_constants operator.value in
      let lst = npseq_to_list args.value.inside in
      let%bind lst = bind_map_list compile_type_expression lst in
      return @@ t_constant ~loc operators lst
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
      Some const -> return @@ t_constant ~loc const []
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
  let compile_tuple_expression ?loc tuple_expr =
    let%bind lst = bind_map_list compile_expression @@ nseq_to_list tuple_expr in
    match lst with
      hd::[] -> return hd
    | lst -> return @@ e_tuple ?loc lst
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
  | EUnit the_unit ->
    let loc = Location.lift the_unit.region in
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
      let%bind args = bind_map_list compile_expression @@ nseq_to_list args in
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
    let (lst, loc) = r_split lst in
    let lst = npseq_to_ne_list lst in
    compile_tuple_expression ~loc lst
  | ERecord record ->
    let (record, loc) = r_split record in
    let aux (fa : CST.field_assign CST.reg) =
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
    (* todo : make it in common with let function *)
    let (func, loc) = r_split func in
    let ({binders; lhs_type; body} : CST.fun_expr) = func in
    let%bind lhs_type = bind_map_option (compile_type_expression <@ snd) lhs_type in
    let%bind ((binder,ty_opt),exprs) = compile_parameter binders in
    let%bind body = compile_expression body in
    let aux (binder, ty_opt,attr,rhs) expr = e_let_in (binder, ty_opt) attr rhs expr in
    let expr = List.fold_right aux exprs body  in
    return @@ e_lambda ~loc binder ty_opt lhs_type expr 
  | EConstr (ESomeApp some) ->
    let ((_, arg), loc) = r_split some in
    let%bind args = compile_tuple_expression @@ List.Ne.singleton arg in
    return @@ e_some ~loc args
  | EConstr (ENone reg) ->
    let loc = Location.lift reg in
    return @@ e_none ~loc ()
  | EConstr (EConstrApp constr) ->
    let ((constr,args_o), loc) = r_split constr in
    let%bind args_o = bind_map_option (compile_tuple_expression <@ List.Ne.singleton) args_o in
    let args = Option.unopt ~default:(e_unit ~loc:(Location.lift constr.region) ()) args_o in
    return @@ e_constructor ~loc constr.value args
  | ECase case ->
    let (case, loc) = r_split case in
    let%bind matchee = compile_expression case.expr in
    let (cases, _) = r_split case.cases in
    let%bind cases = compile_matching_expr @@ npseq_to_ne_list cases in
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
    let%bind else_clause = bind_map_option (compile_expression <@ snd) cond.ifnot in
    return @@ e_cond ~loc test then_clause @@ Option.unopt ~default:(e_unit ~loc ()) else_clause
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
  )
  | ELetIn li ->
    let (li, loc) = r_split li in
    let ({kwd_rec;binding;body;attributes;_} : CST.let_in) = li in
    let%bind lst = compile_let_binding ?kwd_rec attributes binding in
    let%bind body = compile_expression body in
    let aux (binder,ty,attr,rhs) expr = e_let_in ~loc (binder,ty) attr rhs expr in
    return @@ List.fold_right aux lst body
  | ECodeInj ci ->
    let (ci, loc) = r_split ci in
    let (language, _) = r_split ci.language in
    let (language, _) = r_split language in
    let%bind code = compile_expression ci.code in
    return @@ e_raw_code ~loc language code
  | ESeq seq ->
    let (seq, loc) = r_split seq in
    let%bind seq = bind_map_list compile_expression @@ pseq_to_list seq.elements in
    match seq with 
      [] -> return @@ e_unit ~loc ()
    | hd :: tl ->
      let rec aux prev = function
       [] ->  return @@ prev
      | hd :: tl -> bind (return <@ e_sequence ~loc prev) @@ aux hd tl 
      in
      aux hd @@ tl 

and compile_matching_expr :  'a CST.case_clause CST.reg List.Ne.t -> _ =
fun cases ->
  let return = ok in
  let compile_pattern pattern = return pattern in
  let compile_simple_pattern (pattern : CST.pattern) =
    let rec aux = function
      CST.PVar var ->
        return @@ Var.of_name var.value
    | PPar par ->
        aux par.value.inside 
    | _ -> fail @@ unsupported_non_var_pattern pattern
    in aux pattern
  in
  let compile_list_pattern (cases : (CST.pattern * _) list) =
    match cases with
      [(PList PListComp {value={elements=None;_};_}, match_nil);(PList PCons cons, econs)]
    | [(PList PCons cons, econs);(PList PListComp {value={elements=None;_};_}, match_nil)] ->
      let (cons,_)  = r_split cons in
      let (hd,_,tl) = cons in
      let hd_loc = Location.lift @@ Raw.pattern_to_region hd in
      let tl_loc = Location.lift @@ Raw.pattern_to_region hd in
      let%bind (hd,tl) = bind_map_pair compile_simple_pattern (hd,tl) in
      let hd = Location.wrap ?loc:(Some hd_loc) hd in
      let tl = Location.wrap ?loc:(Some tl_loc) tl in
      let match_cons = (hd,tl,econs) in
        return (match_nil,match_cons)
    | _ -> fail @@ unsupported_deep_list_patterns @@ fst @@ List.hd cases
  in
  let compile_constr_pattern (constr : CST.pattern) =
    match constr with
      PConstr c ->
      ( match c with
      | PNone  _ -> return (Label "None", Location.wrap @@ Var.of_name "_")
      | PSomeApp some ->
        let (some,loc) = r_split some in
        let (_, pattern) = some in
        let%bind pattern = compile_simple_pattern pattern in
        return (Label "Some", Location.wrap ~loc pattern)
      | PConstrApp constr ->
        let (constr, pattern_loc) = r_split constr in
        let (constr, patterns) = constr in
        let (constr, _) = r_split constr in
        let%bind pattern = bind_map_option compile_simple_pattern patterns in
        let pattern = Location.wrap ?loc:(Some pattern_loc) @@ Option.unopt ~default:(Var.of_name "_") pattern in
        return (Label constr, pattern)
      | PFalse _ -> return (Label "false", Location.wrap @@ Var.of_name "_")
      | PTrue  _ -> return (Label "true", Location.wrap @@ Var.of_name "_")
    )
    | _ -> fail @@ unsupported_pattern_type constr
  in
  let aux (case : 'a CST.case_clause CST.reg) =
    let (case, _loc) = r_split case in
    let%bind pattern = compile_pattern case.pattern in
    let%bind expr    = compile_expression case.rhs in
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
  | _ -> fail @@ unsupported_pattern_type @@ List.hd @@ List.map fst @@ List.Ne.to_list cases

and compile_let_binding ?kwd_rec attributes binding =
  let return lst = ok lst in
  let return_1 a = return [a] in
  let ({binders; lhs_type; let_rhs; _} : CST.let_binding) = binding in
  let attr = compile_attribute_declaration attributes in
  let%bind lhs_type = bind_map_option (compile_type_expression <@ snd) lhs_type in
  let%bind expr = compile_expression let_rhs in
  let rec aux = function
  | CST.PPar par ->
    let par, _ = r_split par in
    aux par.inside
  | PVar name -> (*function or const *)
    let fun_binder = compile_variable name in
    (* This handle the recursion *)
    let%bind expr = match kwd_rec with
      Some reg ->
        let%bind lambda = trace_option (corner_case "recursion on non function") @@ get_e_lambda expr.expression_content in
        let lhs_type = Option.map (Utils.uncurry t_function) @@ Option.bind_pair (lambda.input_type, lambda.output_type) in 
        let%bind fun_type = trace_option (untyped_recursive_fun reg) @@ lhs_type in
        ok @@ e_recursive ~loc:(Location.lift reg) fun_binder fun_type lambda
    | None   ->
        ok @@ expr
    in 
    return_1 @@ (fun_binder,lhs_type, attr, expr)
  | PTuple tuple -> (* Tuple destructuring *)
    let (tuple, loc) = r_split tuple in
    let%bind lst = bind_map_ne_list compile_parameter @@ npseq_to_ne_list tuple in
    let (lst, exprs) = List.Ne.split lst in
    let exprs = List.flatten @@ List.Ne.to_list exprs in
    let var = Location.wrap ~loc @@ Var.fresh () in
    let body = e_variable var in
    let aux i (var, ty_opt) = Z.add i Z.one, (var,ty_opt, attr, e_accessor body @@ [Access_tuple i]) in
    return @@ (var,None, false, expr) :: (List.fold_map aux Z.zero @@ List.Ne.to_list lst) @ exprs
  | _ -> fail @@ unsupported_pattern_type @@ binders
  in aux binders

and compile_parameter : CST.pattern -> _ result = fun pattern ->
  let return ?ty loc exprs var = 
    ok ((Location.wrap ~loc var, ty), exprs) in
  match pattern with
    PConstr _ -> fail @@ unsupported_pattern_type pattern
  | PUnit the_unit  ->
    let loc = Location.lift the_unit.region in
    return ~ty:(t_unit ~loc ()) loc [] @@ Var.fresh ()
  | PVar var ->
    let (var,loc) = r_split var in
    return loc [] @@ Var.of_name var
  | PWild reg ->
    let loc = Location.lift reg in
    return loc [] @@ Var.fresh ()
  | PTuple tuple ->
    let (tuple, loc) = r_split tuple in
    let%bind lst = bind_map_ne_list compile_parameter @@ npseq_to_ne_list tuple in
    let (lst,exprs) = List.Ne.split lst in
    let (vars,ty_opt) = List.Ne.split lst in
    let ty = match ty_opt with 
      | ty, [] -> ty
      | ty, lst ->
        Option.map (t_tuple ~loc) @@ Option.bind_list @@ ty::lst
    in
    let var, expr = match vars with
      var, [] -> 
      Location.unwrap var, []
    | var, lst ->
      let binder = Var.fresh () in
      let aux i var = Z.add i Z.one, (var,None, false, e_accessor (e_variable @@ Location.wrap ~loc binder) @@ [Access_tuple i]) in
      binder, List.fold_map aux Z.zero @@ var :: lst
    in
    let exprs = List.flatten @@ expr :: List.Ne.to_list exprs in
    return ?ty loc exprs @@ var 
  | PPar par ->
    compile_parameter par.value.inside
  | PRecord _ -> fail @@ unsupported_pattern_type pattern
  | PTyped tp ->
    let (tp, loc) = r_split tp in
    let {pattern; type_expr} : CST.typed_pattern = tp in
    let%bind ty = compile_type_expression type_expr in
    let%bind ((var, _), exprs) = compile_parameter pattern in
    return ~ty loc exprs @@ Location.unwrap var
  | _ -> fail @@ unsupported_pattern_type pattern


and compile_attribute_declaration = fun lst ->
  let lst = List.map (fst <@ r_split) lst in
  let inline = List.filter (String.equal "inline") lst in
  match inline with 
    [] -> false
  | _  -> true

let compile_declaration : CST.declaration -> _ = fun decl ->
  let return reg decl =
    ok @@ List.map (Location.wrap ~loc:(Location.lift reg)) decl in
  let return_1 reg decl = return reg [decl] in
  match decl with
    TypeDecl {value={name; type_expr; _};region} ->
    let (name,_) = r_split name in
    let%bind type_expr = compile_type_expression type_expr in
    return_1 region @@ AST.Declaration_type (Var.of_name name, type_expr)
  | ConstDecl {value = (_kwd_let, kwd_rec, let_binding, attributes); region} ->
    let%bind lst = compile_let_binding ?kwd_rec attributes let_binding in
    let aux (name,ty,attr, expr) =  AST.Declaration_constant (name, ty, attr, expr) in
    return region @@ List.map aux lst

let compile_program : CST.ast -> _ result = fun t ->
    let%bind lst = bind_map_list compile_declaration @@ nseq_to_list t.decl in
    ok @@ List.flatten lst
