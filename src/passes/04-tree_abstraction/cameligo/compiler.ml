open Errors
open Trace
open Function

module CST = Cst.Cameligo
module AST = Ast_imperative

open AST

type nested_match_repr =
  | PatternVar of AST.ty_expr binder
  | TupleVar of AST.ty_expr binder * nested_match_repr list
  | RecordVar of AST.ty_expr binder * AST.label list * nested_match_repr list

let nseq_to_list (hd, tl) = hd :: tl

let npseq_to_list (hd, tl) = hd :: (List.map snd tl)

let npseq_to_ne_list (hd, tl) = hd, (List.map snd tl)

let pseq_to_list = function
  | None -> []
  | Some lst -> npseq_to_list lst
let get_value : 'a Raw.reg -> 'a = fun x -> x.value

let build_ins = ["Test";"Tezos";"Crypto";"Bytes";"List";"Set";"Map";"Big_map";"Bitwise";"String";"Layout"]
  @ ["Michelson";"Loop";"Current"]

open Predefined.Tree_abstraction.Cameligo

let r_split = Location.r_split

let compile_variable var = Location.map Var.of_name @@ Location.lift_region var
let compile_attributes attributes : string list =
  List.map (fst <@ r_split) attributes

let rec compile_type_expression : CST.type_expr -> _ result = fun te ->
  let self = compile_type_expression in
  let return te = ok @@ te in
  match te with
    TSum sum ->
      let sum_type, loc = r_split sum in
      let {variants; attributes; _} : CST.sum_type = sum_type in
      let lst = npseq_to_list variants in
      let attr = compile_attributes attributes in
      let aux (variant : CST.variant CST.reg) =
        let v, _ = r_split variant in
        let%bind type_expr =
          bind_map_option (self <@ snd) v.arg in
        let type_expr = Option.unopt ~default:(t_unit ()) type_expr in
        let variant_attr = compile_attributes v.attributes in
        ok @@ (v.constr.value, type_expr, variant_attr) in
      let%bind sum = bind_map_list aux lst
      in return @@ t_sum_ez_attr ~loc ~attr sum
  | TRecord record ->
      let injection, loc = r_split record in
      let attr = compile_attributes injection.attributes in
      let lst = npseq_to_list injection.ne_elements in
      let aux (field : CST.field_decl CST.reg) =
        let f, _ = r_split field in
        let%bind type_expr = self f.field_type in
        let field_attr = List.map (fun x -> x.Region.value) f.attributes
        in return @@ (f.field_name.value, type_expr, field_attr) in
      let%bind fields = bind_map_list aux lst in
      return @@ t_record_ez_attr ~loc ~attr fields
  | TProd prod ->
    let (nsepseq, loc) = r_split prod in
    let lst = npseq_to_list nsepseq in
    let%bind lst = bind_map_list self lst in
    return @@ t_tuple ~loc lst
  | TApp app ->
    let get_t_string_singleton_opt = function
      | CST.TString s -> Some s.value
      | _ -> None
    in
    let get_t_int_singleton_opt = function
      | CST.TInt x ->
        let (_,z) = x.value in
        Some z
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
          let%bind a' = self a in
          let%bind c' = self c in
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
          let%bind a' = self a in
          let%bind c' = self c in
          return @@ t_michelson_pair ~loc a' b' c' d'
          )
        | _ -> fail @@ michelson_type_wrong_arity loc operator.value)
      | "sapling_state" ->
        let lst = npseq_to_list args.value.inside in
        (match lst with
        | [(a : CST.type_expr)] -> (
          let sloc = Location.lift @@ Raw.type_expr_to_region a in
          let%bind a' =
            trace_option (michelson_type_wrong te operator.value) @@
              get_t_int_singleton_opt a in
          let singleton = t_singleton ~loc:sloc (Literal_int a') in
          return @@ t_sapling_state ~loc singleton
          )
        | _ -> fail @@ michelson_type_wrong_arity loc operator.value)
      | "sapling_transaction" ->
        let lst = npseq_to_list args.value.inside in
        (match lst with
        | [(a : CST.type_expr)] -> (
          let sloc = Location.lift @@ Raw.type_expr_to_region a in
          let%bind a' =
            trace_option (michelson_type_wrong te operator.value) @@
              get_t_int_singleton_opt a in
          let singleton = t_singleton ~loc:sloc (Literal_int a') in
          return @@ t_sapling_transaction ~loc singleton
          )
        | _ -> fail @@ michelson_type_wrong_arity loc operator.value)
    | _ ->
      let operator = Var.of_name operator.value in
      let lst = npseq_to_list args.value.inside in
      let%bind lst = bind_map_list self lst in
      return @@ t_app ~loc operator lst
    )
  | TFun func ->
    let ((input_type,_,output_type), loc) = r_split func in
    let%bind input_type = self input_type in
    let%bind output_type = self output_type in
    return @@ t_function ~loc input_type output_type
  | TPar par ->
    let (par, _) = r_split par in
    let type_expr = par.inside in
    self type_expr

  | TVar var ->
    let (name,loc) = r_split var in
    let v = Var.of_name name in
    return @@ t_variable ~loc v
  | TWild _ -> failwith "unsupported TWild"
  | TString _s -> fail @@ unsupported_string_singleton te
  | TInt _s -> fail @@ unsupported_string_singleton te
  | TModA ma ->
    let (ma, loc) = r_split ma in
    let (module_name, _) = r_split ma.module_name in
    let%bind element = self ma.field in
    return @@ t_module_accessor ~loc module_name element

let compile_selection (selection : CST.selection) =
  match selection with
    FieldName name ->
    let (name, loc) = r_split name in
    (Access_record name, loc)
  | Component comp ->
    let ((_,index), loc) = r_split comp in
    (Access_tuple index, loc)

let rec compile_expression : CST.expr -> (AST.expr , abs_error) result = fun e ->
  let self = compile_expression in
  let return e = ok @@ e in
  let compile_tuple_expression ?loc tuple_expr =
    let%bind lst = bind_map_list self @@ nseq_to_list tuple_expr in
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
    let%bind a = self op.arg1 in
    let%bind b = self op.arg2 in
    return @@ e_constant ~loc (Const op_type) [a; b]
  in
  let compile_un_op (op_type : AST.constant') (op : _ CST.un_op CST.reg) =
    let (op, loc) = r_split op in
    let%bind arg = self op.arg in
    return @@ e_constant ~loc (Const op_type) [arg]
  in
  match e with
    EVar var ->
    let (var, loc) = r_split var in
    (match constants var with
      Some const -> return @@ e_constant ~loc const []
    | None -> return @@ e_variable_ez ~loc var
    )
  | EPar par -> self par.value.inside
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
      let%bind a = self op.arg1 in
      let%bind b = self op.arg2 in
      return @@ e_constant ~loc (Const C_CONCAT) [a;b]
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
      let%bind args = bind_map_list self @@ nseq_to_list args in
      return @@ e_constant ~loc const args
    | None ->
      let func = e_variable_ez ~loc:loc_var var in
      let%bind args = bind_map_list self @@ nseq_to_list args in
      return @@ List.fold_left (e_application ~loc) func @@ args
    )
  (*TODO: move to proper module*)
  | ECall {value=(EModA {value={module_name;field};region=_},args);region} when
    List.mem module_name.value build_ins ->
    let loc = Location.lift region in
    let%bind fun_name = match field with
      EVar v -> ok @@ v.value
      | EModA _ -> fail @@ unknown_constant module_name.value loc
      |ECase _|ECond _|EAnnot _|EList _|EConstr _|EUpdate _|ELetIn _|EFun _|ESeq _|ECodeInj _
      |ELogic _|EArith _|EString _|ERecord _|EProj _|ECall _|EBytes _|EUnit _|ETypeIn _
      |ETuple _|EPar _ -> failwith "Corner case : This couldn't be produce by the parser"
    in
    let var = module_name.value ^ "." ^ fun_name in
    (match constants var with
      Some const ->
      let%bind args = bind_map_list self @@ nseq_to_list args in
      return @@ e_constant ~loc const args
    | None ->
      fail @@ unknown_constant var loc
      )
  | ECall call ->
    let ((func, args), loc) = r_split call in
    let%bind func = self func in
    let%bind args = bind_map_list self @@ nseq_to_list args in
    return @@ List.fold_left (e_application ~loc) func @@ args
  | ETuple lst ->
    let (lst, loc) = r_split lst in
    let lst = npseq_to_ne_list lst in
    compile_tuple_expression ~loc lst
  | ERecord record ->
    let (record, loc) = r_split record in
    let aux (fa : CST.field_assign CST.reg) =
      let (fa, _) = r_split fa in
      let (name, _) = r_split fa.field_name in
      let%bind expr = self fa.field_expr in
      return (name, expr)
    in
    let%bind record = bind_map_list aux @@ npseq_to_list record.ne_elements in
    return @@ e_record_ez ~loc record
  | EProj proj ->
    let (proj, loc) = r_split proj in
    let (var, loc_var) = r_split proj.struct_name in
    let var  = e_variable_ez ~loc:loc_var var in
    let (sels, _) = List.split @@ List.map compile_selection @@ npseq_to_list proj.field_path in
    return @@ e_accessor ~loc var sels
  | EModA ma ->
    let (ma, loc) = r_split ma in
    let (module_name, _) = r_split ma.module_name in
    let%bind element = self ma.field in
    (*TODO: move to proper module*)
    if List.mem module_name build_ins then
      let%bind fun_name = match ma.field with
        EVar v -> ok @@ v.value
      | EModA _ -> fail @@ unknown_constant module_name loc
      |ECase _|ECond _|EAnnot _|EList _|EConstr _|EUpdate _|ELetIn _|EFun _|ESeq _|ECodeInj _
      |ELogic _|EArith _|EString _|ERecord _|EProj _|ECall _|EBytes _|EUnit _|ETypeIn _
      |ETuple _|EPar _ -> failwith "Corner case : This couldn't be produce by the parser"
      in
      let var = module_name ^ "." ^ fun_name in
      (match constants var with
        Some const -> return @@ e_constant ~loc const []
      | None -> return @@ e_variable_ez ~loc var
      )
    else return @@ e_module_accessor ~loc module_name element
  | EUpdate update ->
    let (update, _loc) = r_split update in
    let%bind record = compile_path update.record in
    let (updates, _loc) = r_split update.updates in
    let aux (up : CST.field_path_assignment CST.reg) =
      let (up, loc) = r_split up in
      let path = up.field_path in
      let%bind expr = self up.field_expr in
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
    let%bind () = check_annotation (fst binders) in
    let%bind () = bind_list_iter check_annotation (snd binders) in
    let%bind lhs_type = bind_map_option (compile_type_expression <@ snd) lhs_type in
    let%bind (binder,exprs),lst = bind_map_ne_list compile_parameter binders in
    let%bind body = self body in
    let rec aux lst =
      match lst with
        [] -> body,lhs_type
      | (binder,exprs):: lst ->
        let expr,lhs_type = aux lst in
        let aux expr (binder,attr,rhs) = e_let_in binder attr rhs expr in
        let expr = List.fold_left aux expr exprs in
        e_lambda ~loc binder lhs_type expr,
        Option.map (Utils.uncurry @@ t_function ~loc) @@ Option.bind_pair (binder.ascr,lhs_type)
    in
    let expr,lhs_type = aux lst in
    let aux (binder,attr,rhs) expr = e_let_in binder attr rhs expr in
    let expr = List.fold_right aux exprs expr  in
    return @@ e_lambda ~loc binder lhs_type expr
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
    let%bind matchee = self case.expr in
    let (cases, _) = r_split case.cases in
    let%bind cases = compile_matching_expr @@ npseq_to_ne_list cases in
    return @@ e_matching ~loc matchee cases
  | EAnnot annot ->
    let (annot, loc) = r_split annot in
    let (expr, _, ty) = annot.inside in
    let%bind expr = self expr in
    let%bind ty   = compile_type_expression ty in
    return @@ e_annotation ~loc expr ty
  | ECond cond ->
    let (cond, loc) = r_split cond in
    let%bind test        = self cond.test in
    let%bind then_clause = self cond.ifso in
    let%bind else_clause = bind_map_option (self <@ snd) cond.ifnot in
    return @@ e_cond ~loc test then_clause @@ Option.unopt ~default:(e_unit ~loc ()) else_clause
  | EList lst -> (
    match lst with
      ECons cons ->
      let (cons, loc) = r_split cons in
      let%bind a  = self cons.arg1 in
      let%bind b  = self cons.arg2 in
      return @@ e_constant ~loc (Const C_CONS) [a; b]
    | EListComp lc ->
      let (lc,loc) = r_split lc in
      let lst =
        Option.unopt ~default:[] @@
        Option.map npseq_to_list lc.elements
      in
      let%bind lst = bind_map_list self lst in
      return @@ e_list ~loc lst
  )
  | ELetIn li -> (
    let (li, loc) = r_split li in
    let ({kwd_rec;binding;body;attributes;_} : CST.let_in) = li in
    let%bind body = self body in
    match binding with
    | { binders ; let_rhs } -> (
      let (pattern,arg) = binders in
      match (unepar pattern,arg) with
      | CST.PTuple tuple , [] ->
        let%bind matchee = compile_expression let_rhs in
        compile_tuple_let_destructuring matchee body tuple
      | CST.PRecord record , [] ->
        let%bind matchee = compile_expression let_rhs in
        compile_record_let_destructuring matchee body record
      | _ -> (
        let%bind lst = compile_let_binding ?kwd_rec attributes binding in
        let aux (binder,attr,rhs) expr = e_let_in ~loc binder attr rhs expr in
        return @@ List.fold_right aux lst body
      )
    )
  )
  | ETypeIn ti ->
    let (ti, loc) = r_split ti in
    let ({type_decl={name;type_expr;_};kwd_in=_;body} : CST.type_in) = ti in
    let type_binder = Var.of_name name.value in
    let%bind rhs = compile_type_expression type_expr in
    let%bind body = compile_expression body in
    return @@ e_type_in ~loc type_binder rhs body
  | ECodeInj ci ->
    let (ci, loc) = r_split ci in
    let (language, _) = r_split ci.language in
    let (language, _) = r_split language in
    let%bind code = self ci.code in
    return @@ e_raw_code ~loc language code
  | ESeq seq ->
    let (seq, loc) = r_split seq in
    let%bind seq = bind_map_list self @@ pseq_to_list seq.elements in
    match seq with
      [] -> return @@ e_unit ~loc ()
    | hd :: tl ->
      let rec aux prev = function
       [] ->  return @@ prev
      | hd :: tl -> bind (return <@ e_sequence ~loc prev) @@ aux hd tl
      in
      aux hd @@ tl

and conv : CST.pattern -> (nested_match_repr,_) result =
  fun p ->
  match unepar p with
  | CST.PWild reg ->
    let loc = Location.lift reg in
    let var = Location.wrap ~loc @@ Var.fresh () in
    ok (PatternVar { var ; ascr = None })
  | CST.PVar var ->
    let (var,loc) = r_split var in
    let var = Location.wrap ~loc @@ Var.of_name var in
    ok (PatternVar { var ; ascr = None })
  | CST.PTuple tuple ->
    let (tuple, _loc) = r_split tuple in
    let lst = npseq_to_ne_list tuple in
    let patterns = List.Ne.to_list lst in
    let%bind nested = bind_map_list conv patterns in
    let var = Location.wrap @@ Var.fresh () in
    ok (TupleVar ({var ; ascr = None} , nested))
  | CST.PRecord record ->
    let (inj, _loc) = r_split record in
    let aux : CST.field_pattern CST.reg -> (label * nested_match_repr,_) result = fun field ->
      let { field_name ; eq=_ ; pattern } : CST.field_pattern = field.value in
      let%bind pattern = conv pattern in
      ok (AST.Label field_name.value , pattern)
    in
    let%bind lst = bind_map_ne_list aux @@ npseq_to_ne_list inj.ne_elements in
    let lst = List.Ne.to_list lst in
    let (labels,nested) = List.split lst in
    let var = Location.wrap @@ Var.fresh () in
    ok (RecordVar ({var ; ascr = None}, labels , nested))
  | _ -> fail @@ unsupported_pattern_type [p]

and get_binder : nested_match_repr -> AST.ty_expr binder =
  fun s ->
  match s with
  | TupleVar (x,_) -> x
  | PatternVar x -> x
  | RecordVar (x,_,_) -> x

and fold_nested_z
  f acc l =
  match l with
  | [] -> acc
  | ( PatternVar _ as z ) :: l ->
    let x  = f acc z in
    fold_nested_z f x l
  | (TupleVar (_,inner) as z)::l ->
    let x = f acc z in
    let x = fold_nested_z f x inner in
    fold_nested_z f x l
  | (RecordVar (_,_,inner) as z)::l ->
    let x = f acc z in
    let x = fold_nested_z f x inner in
    fold_nested_z f x l

and nestrec : AST.expression -> (AST.expression -> AST.expression) -> nested_match_repr list -> AST.expression =
  fun res f lst ->
    let aux :  (AST.expression -> AST.expression) -> nested_match_repr -> (AST.expression -> AST.expression) =
      fun f z ->
        match z with
        | PatternVar _ -> f
        | TupleVar (matchee,nested) ->
          let binders = List.map get_binder nested in
          let f' = fun body -> f (e_matching (e_variable matchee.var) (Match_tuple (binders,body))) in
          f'
        | RecordVar (matchee,labels,nested) ->
          let binders = List.map get_binder nested in
          let lbinders = List.combine labels binders in
          let f' = fun body -> f (e_matching (e_variable matchee.var) (Match_record (lbinders,body))) in
          f'
    in
    match lst with
    | PatternVar _ :: tl -> nestrec res f tl
    | TupleVar (matchee,nested) :: tl ->
      let binders = List.map get_binder nested in
      let f' = fun body -> f (e_matching (e_variable matchee.var) (Match_tuple (binders,body))) in
      let f'' = fold_nested_z aux f' nested in
      nestrec res f'' tl
    | RecordVar (matchee,labels,nested) :: tl ->
      let binders = List.map get_binder nested in
      let lbinders = List.combine labels binders in
      let f' = fun body -> f (e_matching (e_variable matchee.var) (Match_record (lbinders,body))) in
      let f'' = fold_nested_z aux f' nested in
      nestrec res f'' tl
    | [] -> f res

and compile_tuple_let_destructuring : AST.expression -> AST.expression -> (CST.pattern, CST.comma) Utils.nsepseq CST.reg -> (AST.expression,_) result =
  fun matchee let_result tuple ->
    let (tuple, loc) = r_split tuple in
    let lst = npseq_to_ne_list tuple in
    let patterns = List.Ne.to_list lst in
    let%bind patterns = bind_map_list conv patterns in
    let binders = List.map get_binder patterns in
    let f = fun body -> e_matching ~loc matchee (Match_tuple (binders, body)) in
    let body = nestrec let_result f patterns in
    ok body

and compile_record_let_destructuring : AST.expression -> AST.expression -> CST.field_pattern CST.reg CST.ne_injection CST.reg -> (AST.expression,_) result =
  fun matchee let_result record ->
    let (record, loc) = r_split record in
    let aux : CST.field_pattern CST.reg -> (label * CST.pattern,_) result = fun field ->
      let { field_name ; eq=_ ; pattern } : CST.field_pattern = field.value in
      ok (AST.Label field_name.value , pattern)
    in
    let%bind lst = bind_map_ne_list aux @@ npseq_to_ne_list record.ne_elements in
    let lst = List.Ne.to_list lst in
    let (labels,patterns) = List.split lst in
    let%bind patterns = bind_map_list conv patterns in
    let binders = List.map get_binder patterns in
    let lbinders = List.combine labels binders in
    let f = fun body -> e_matching ~loc matchee (Match_record (lbinders, body)) in
    let body = nestrec let_result f patterns in
    ok body

and compile_matching_expr :  'a CST.case_clause CST.reg List.Ne.t -> (matching_expr, abs_error) result =
fun cases ->
  let return = ok in
  let compile_pattern pattern = return pattern in
  let compile_simple_pattern (pattern : CST.pattern) =
    let rec aux = function
      CST.PVar var ->
        return @@ Var.of_name var.value
    | PPar par ->
        aux par.value.inside
    | _ -> fail @@ unsupported_pattern_type [pattern]
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
      let hd = Location.wrap ~loc:hd_loc hd in
      let tl = Location.wrap ~loc:tl_loc tl in
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
        let pattern = Location.wrap ~loc:pattern_loc @@ Option.unopt ~default:(Var.of_name "_") pattern in
        return (Label constr, pattern)
      | PFalse _ -> return (Label "false", Location.wrap @@ Var.of_name "_")
      | PTrue  _ -> return (Label "true", Location.wrap @@ Var.of_name "_")
    )
    | _ -> fail @@ unsupported_pattern_type [constr]
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
    let var = Location.wrap ~loc @@ Var.of_name var in
    let binder = {var; ascr=None} in
    return @@ AST.Match_variable (binder, expr)
  | (PTuple tuple , expr), []
  | (PPar { value = { inside = PTuple tuple ; _} ;_}, expr), [] ->
    let aux : CST.pattern -> (ty_expr binder,_) result = fun var ->
      match var with
      | CST.PVar var ->
        let (name, loc) = r_split var in
        let var = Location.wrap ~loc @@ Var.of_name name in
        ok @@ { var ; ascr=None }
      | x ->
        (* TODO : patterns in match not supported see !909 *)
        fail @@ unsupported_deep_pattern_matching (Raw.pattern_to_region x)
    in
    let%bind lst = bind_map_ne_list aux @@ npseq_to_ne_list tuple.value in
    let lst : ty_expr binder list = List.Ne.to_list lst in
    return @@ AST.Match_tuple (lst, expr)
  | (PRecord record , expr), []
  | (PPar { value = { inside = PRecord record; _} ;_}, expr), [] ->
    let (inj, _loc) = r_split record in
    let aux : CST.field_pattern CST.reg -> (label * ty_expr binder,_) result = fun field ->
      let { field_name ; eq=_ ; pattern } : CST.field_pattern = field.value in 
      match pattern with
      | CST.PVar var ->
        let (name, loc) = r_split var in
        let var = Location.wrap ~loc @@ Var.of_name name in
        ok @@ (Label field_name.value , { var ; ascr=None })
      | x ->
        (* TODO : patterns in match not supported see !909 *)
        fail @@ unsupported_deep_pattern_matching (Raw.pattern_to_region x)
    in
    let%bind lst = bind_map_ne_list aux @@ npseq_to_ne_list inj.ne_elements in
    let lst = List.Ne.to_list lst in
    return @@ AST.Match_record (lst, expr)
  | (PList _, _), _ ->
    let%bind (match_nil,match_cons) = compile_list_pattern @@ List.Ne.to_list cases in
    return @@ AST.Match_list {match_nil;match_cons}
  | (PConstr _,_), _ ->
    let (pattern, lst) = List.split @@ List.Ne.to_list cases in
    let%bind constrs = bind_map_list compile_constr_pattern pattern in
    return @@ AST.Match_variant (List.combine constrs lst)
  | _ ->
    fail @@ unsupported_pattern_type @@ List.map fst @@ List.Ne.to_list cases

and unepar = function
| CST.PPar { value = { inside; _ }; _ } -> unepar inside
| _ as v -> v

and untpar = function
| CST.TPar { value = { inside; _ }; _ } -> untpar inside
| _ as v -> v

and check_annotation = function
| CST.PVar v -> fail (missing_funarg_annotation v)
| CST.PPar { value = { inside ; _ }; _ } -> check_annotation inside
| CST.PTuple { value ; _ } ->
  let l = Utils.nsepseq_to_list value in
  bind_list_iter check_annotation l
| CST.PTyped { value = { pattern; type_expr; _ }; _ } -> (
  let (pattern: CST.pattern) = unepar pattern in
  let (type_expr: CST.type_expr) = untpar type_expr in
  match pattern, type_expr with
  | PTuple { value = pval; region }, TProd { value = tval; _ } -> (
    let no_of_tuple_components = List.length (Utils.nsepseq_to_list pval) in
    let no_of_tuple_type_components = List.length (Utils.nsepseq_to_list tval) in
    if (no_of_tuple_components <> no_of_tuple_type_components) then
      fail (funarg_tuple_type_mismatch region pattern type_expr)
    else
      ok ())
  | _ -> ok ()
)
| _ -> ok ()

and compile_let_binding ?kwd_rec attributes binding =
  let return lst = ok lst in
  let return_1 a = return [a] in
  let ({binders; lhs_type; let_rhs; _} : CST.let_binding) = binding in
  let attributes = compile_attributes attributes in
  let%bind lhs_type = bind_map_option (compile_type_expression <@ snd) lhs_type in
  let%bind expr = compile_expression let_rhs in
  let rec aux = function
  | CST.PPar par, [] ->
    let par, _ = r_split par in
    aux (par.inside, [])
  | PVar name, args -> (*function *)
    let%bind () = bind_list_iter check_annotation args in
    let%bind args = bind_map_list compile_parameter args in
    let fun_binder = compile_variable name in
    let rec aux lst =
      match lst with
        [] -> expr,lhs_type
      | (binder,exprs):: lst ->
        let loc = Location.get_location @@ binder.var in
        let expr,lhs_type = aux lst in
        let aux expr (binder ,attr,rhs) = e_let_in binder attr rhs expr in
        let expr = List.fold_left aux expr exprs in
        e_lambda ~loc binder lhs_type expr,
        Option.map (Utils.uncurry @@ t_function ~loc) @@ Option.bind_pair (binder.ascr,lhs_type)
    in
    let expr,lhs_type = aux args in
    (* This handle the recursion *)
    let%bind expr = match kwd_rec with
      Some reg ->
        let%bind fun_type = trace_option (untyped_recursive_fun reg) @@ lhs_type in
        let%bind lambda = trace_option (recursion_on_non_function expr.location) @@ get_e_lambda expr.expression_content in
        ok @@ e_recursive ~loc:(Location.lift reg) fun_binder fun_type lambda
    | None   ->
        ok @@ expr
    in
    return_1 @@ ({var=fun_binder;ascr=lhs_type}, attributes, expr)
  | PTuple tuple, [] -> (* tuple destructuring (for top-level only) TODO: this should be deprecated or handled in a better way *)
    let (tuple, loc) = r_split tuple in
    let%bind lst = bind_map_ne_list compile_parameter @@ npseq_to_ne_list tuple in
    let (lst, exprs) = List.Ne.split lst in
    let exprs = List.flatten @@ List.Ne.to_list exprs in
    let var = Location.wrap ~loc @@ Var.fresh () in
    let body = e_variable var in
    let aux i binder = Z.add i Z.one, (binder, attributes, e_accessor body @@ [Access_tuple i]) in
    return @@ ({var;ascr=None}, [], expr) :: (List.fold_map aux Z.zero @@ List.Ne.to_list lst) @ exprs
  | _ -> fail @@ unsupported_pattern_type @@ nseq_to_list binders
  in aux binders

and compile_parameter : CST.pattern -> _ result =
  fun pattern ->
  let return ?ascr loc exprs var =
    ok ({var=Location.wrap ~loc var; ascr}, exprs) in
  match pattern with
    PConstr _ -> fail @@ unsupported_pattern_type [pattern]
  | PUnit the_unit  ->
    let loc = Location.lift the_unit.region in
    return ~ascr:(t_unit ~loc ()) loc [] @@ Var.fresh ()
  | PVar var ->
    let (var,loc) = r_split var in
    return loc [] @@ Var.of_name var
  | PWild reg ->
    let loc = Location.lift reg in
    return loc [] @@ Var.fresh ()
  | PTuple tuple ->
    let (tuple, loc) = r_split tuple in
    let%bind lst = bind_map_ne_list compile_parameter @@ npseq_to_ne_list tuple in
    let (binder,exprs) = List.Ne.split lst in
    let var, ascr, expr = match binder with
      {var; ascr}, [] ->
      Location.unwrap var, ascr, []
    | var, lst ->
      let binder = Var.fresh () in
      let aux i b = Z.add i Z.one, (b, [], e_accessor (e_variable @@ Location.wrap ~loc binder) @@ [Access_tuple i]) in
      binder,
      Option.map (t_tuple ~loc) @@ Option.bind_list @@ List.map (fun e -> e.ascr) @@ var::lst,
      List.fold_map aux Z.zero @@ var :: lst
    in
    let exprs = List.flatten @@ expr :: List.Ne.to_list exprs in
    return ?ascr loc exprs @@ var
  | PPar par ->
    let (par,loc) = r_split par in
    let%bind ({var;ascr}, expr) = compile_parameter par.inside in
    return ?ascr loc expr @@ Location.unwrap var
  | PRecord _ -> fail @@ unsupported_pattern_type [pattern]
  | PTyped tp ->
    let (tp, loc) = r_split tp in
    let {pattern; type_expr} : CST.typed_pattern = tp in
    let%bind ascr = compile_type_expression type_expr in
    let%bind ({var; _}, exprs) = compile_parameter pattern in
    return ~ascr loc exprs @@ Location.unwrap var
  | _ -> fail @@ unsupported_pattern_type [pattern]

let compile_declaration : CST.declaration -> _ = fun decl ->
  let return reg decl =
    ok @@ List.map (Location.wrap ~loc:(Location.lift reg)) decl in
  let return_1 reg decl = return reg [decl] in
  match decl with
    TypeDecl {value={name; type_expr; _};region} ->
    let (name,_) = r_split name in
    let%bind type_expr = compile_type_expression type_expr in
    return_1 region @@ AST.Declaration_type  {type_binder=Var.of_name name; type_expr}
  | Let {value = (_kwd_let, kwd_rec, let_binding, attributes); region} ->
    let%bind lst = compile_let_binding ?kwd_rec attributes let_binding in
    let aux (binder,attr, expr) =  AST.Declaration_constant {binder; attr; expr} in
    return region @@ List.map aux lst

let compile_program : CST.ast -> _ result = fun t ->
    let%bind lst = bind_map_list compile_declaration @@ nseq_to_list t.decl in
    ok @@ List.flatten lst