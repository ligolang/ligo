open Errors
open Trace
open Function

module CST = Cst.Reasonligo
module AST = Ast_imperative

open AST

let nseq_to_list (hd, tl) = hd :: tl

let npseq_to_list (hd, tl) = hd :: (List.map snd tl)

let npseq_to_ne_list (hd, tl) = hd, (List.map snd tl)

let pseq_to_list = function
  | None -> []
  | Some lst -> npseq_to_list lst

let get_value : 'a Raw.reg -> 'a = fun x -> x.value

let build_ins = ["Test";"Tezos";"Crypto";"Bytes";"List";"Set";"Map";"Big_map";"Bitwise";"String";"Layout"]
  @ ["Michelson"]

open Predefined.Tree_abstraction.Cameligo

let r_split = Location.r_split

let mk_var var = if String.compare var Var.wildcard = 0 then Var.fresh () else Var.of_name var

let compile_variable var = Location.map Var.of_name @@ Location.lift_region var
let compile_attributes attributes : string list =
  List.map (fst <@ r_split) attributes

let rec compile_type_expression : CST.type_expr -> _ result =
  fun te ->
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
        let* type_expr =
          bind_map_option (self <@ snd) v.arg in
        let type_expr = Option.unopt ~default:(t_unit ()) type_expr in
        let variant_attr = compile_attributes v.attributes in
        ok @@ (v.constr.value, type_expr, variant_attr) in
      let* sum = bind_map_list aux lst
      in return @@ t_sum_ez_attr ~loc ~attr sum
  | TRecord record ->
    let injection, loc = r_split record in
    let attributes = compile_attributes injection.attributes in
    let lst = npseq_to_list injection.ne_elements in
    let aux (field : CST.field_decl CST.reg) =
      let f, _ = r_split field in
      let* type_expr =
        self f.field_type in
      let field_attr = compile_attributes f.attributes in
      return @@ (f.field_name.value, type_expr, field_attr) in
    let* fields = bind_map_list aux lst in
    return @@ t_record_ez_attr ~loc ~attr:attributes fields
  | TProd prod ->
    let (nsepseq, loc) = r_split prod in
    let lst = npseq_to_list nsepseq.inside in
    let* lst = bind_map_list self lst in
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
          let* b' =
            trace_option (michelson_type_wrong te operator.value) @@
              get_t_string_singleton_opt b in
          let* d' =
            trace_option (michelson_type_wrong te operator.value) @@
              get_t_string_singleton_opt d in
          let* a' = self a in
          let* c' = self c in
          return @@ t_michelson_or ~loc a' b' c' d'
          )
        | _ -> fail @@ michelson_type_wrong_arity loc operator.value)
      | "michelson_pair" ->
        let lst = npseq_to_list args.value.inside in
        (match lst with
        | [a ; b ; c ; d ] -> (
          let* b' =
            trace_option (michelson_type_wrong te operator.value) @@
              get_t_string_singleton_opt b in
          let* d' =
            trace_option (michelson_type_wrong te operator.value) @@
              get_t_string_singleton_opt d in
          let* a' = self a in
          let* c' = self c in
          return @@ t_michelson_pair ~loc a' b' c' d'
          )
        | _ -> fail @@ michelson_type_wrong_arity loc operator.value)
      | "sapling_state" ->
        let lst = npseq_to_list args.value.inside in
        (match lst with
        | [(a : CST.type_expr)] -> (
          let sloc = Location.lift @@ Raw.type_expr_to_region a in
          let* a' =
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
          let* a' =
            trace_option (michelson_type_wrong te operator.value) @@
              get_t_int_singleton_opt a in
          let singleton = t_singleton ~loc:sloc (Literal_int a') in
          return @@ t_sapling_transaction ~loc singleton
          )
        | _ -> fail @@ michelson_type_wrong_arity loc operator.value)
    | _ ->
      let operators = Var.of_name operator.value in
      let lst = npseq_to_list args.value.inside in
      let* lst = bind_map_list self lst in
      return @@ t_app ~loc operators lst
    )
  | TFun func ->
    let ((input_type,_,output_type), loc) = r_split func in
    let* input_type = self input_type in
    let* output_type = self output_type in
    return @@ t_function ~loc input_type output_type
  | TPar par ->
    let (par, _) = r_split par in
    let type_expr = par.inside in
    self type_expr
  | TVar var ->
    let (name,loc) = r_split var in
    let v = Var.of_name name in
    return @@ t_variable ~loc v
  | TWild _reg -> fail @@ unsupported_twild te
  | TString _s -> fail @@ unsupported_string_singleton te
  | TInt _s -> fail @@ unsupported_string_singleton te
  | TModA ma ->
    let (ma, loc) = r_split ma in
    let (module_name, _) = r_split ma.module_name in
    let* element = self ma.field in
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
    let* lst = bind_map_list self @@ nseq_to_list tuple_expr in
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
    let* a = self op.arg1 in
    let* b = self op.arg2 in
    return @@ e_constant ~loc (Const op_type) [a; b]
  in
  let compile_un_op (op_type : AST.constant') (op : _ CST.un_op CST.reg) =
    let (op, loc) = r_split op in
    let* arg = self op.arg in
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
      let* a = self op.arg1 in
      let* b = self op.arg2 in
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
    let args = match args with
      | Unit the_unit -> CST.EUnit the_unit,[]
      | Multiple xs ->
         let hd,tl = xs.value.inside in
         hd,List.map snd tl in
    let loc = Location.lift region in
    let (var, loc_var) = r_split var in
    (match constants var with
      Some const ->
      let* args = bind_map_list self @@ nseq_to_list args in
      return @@ e_constant ~loc const args
    | None ->
      let func = e_variable_ez ~loc:loc_var var in
      let* args = compile_tuple_expression args in
      return @@ e_application ~loc func args
    )
  (*TODO: move to proper module*)
  | ECall {value=(EModA {value={module_name;field};region=_},args);region} when
    List.mem module_name.value build_ins ->
    let args = match args with
      | Unit the_unit -> CST.EUnit the_unit,[]
      | Multiple xs ->
         let hd,tl = xs.value.inside in
         hd,List.map snd tl in
    let loc = Location.lift region in
    let* fun_name = match field with
      EVar v -> ok @@ v.value | EModA _ -> fail @@ unknown_constant module_name.value loc
      |ECase _|ECond _|EAnnot _|EList _|EConstr _|EUpdate _|ELetIn _|EFun _|ESeq _|ECodeInj _
      |ELogic _|EArith _|EString _|ERecord _|EProj _|ECall _|EBytes _|EUnit _|ETypeIn _|EModIn _
      |EModAlias _|ETuple _|EPar _ -> failwith "Corner case : This couldn't be produce by the parser"
    in
    let var = module_name.value ^ "." ^ fun_name in
    (match constants var with
      Some const ->
      let* args = bind_map_list self @@ nseq_to_list args in
      return @@ e_constant ~loc const args
    | None ->
      fail @@ unknown_constant var loc
      )
  | ECall call ->
    let ((func, args), loc) = r_split call in
    let args = match args with
      | Unit the_unit -> CST.EUnit the_unit,[]
      | Multiple xs ->
         let hd,tl = xs.value.inside in
         hd,List.map snd tl in
    let* func = self func in
    let* args = compile_tuple_expression args in
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
      let* expr = self fa.field_expr in
      return (name, expr)
    in
    let* record = bind_map_list aux @@ npseq_to_list record.ne_elements in
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
    let* element = self ma.field in
    (*TODO: move to proper module*)
    if List.mem module_name build_ins then
      let* fun_name = match ma.field with
        EVar v -> ok @@ v.value
      | EModA _ -> fail @@ unknown_constant module_name loc
      |ECase _|ECond _|EAnnot _|EList _|EConstr _|EUpdate _|ELetIn _|EFun _|ESeq _|ECodeInj _
      |ELogic _|EArith _|EString _|ERecord _|EProj _|ECall _|EBytes _|EUnit _|ETypeIn _|EModIn _
      |EModAlias _|ETuple _|EPar _ -> failwith "Corner case : This couldn't be produce by the parser"
      in
      let var = module_name ^ "." ^ fun_name in
      (match constants var with
        Some const -> return @@ e_constant ~loc const []
      | None -> return @@ e_variable_ez ~loc var
      )
    else
      return @@ e_module_accessor ~loc module_name element
  | EUpdate update ->
    let (update, _loc) = r_split update in
    let* record = compile_path update.record in
    let (updates, _loc) = r_split update.updates in
    let aux (up : CST.field_path_assignment CST.reg) =
      let (up, loc) = r_split up in
      let path = up.field_path in
      let* expr = self up.field_expr in
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
    let* updates = bind_map_list aux @@ npseq_to_list updates.ne_elements in
    let aux e (path, update, loc) = e_update ~loc e path update in
    return @@ List.fold_left aux record updates
  | EFun func ->
    (* todo : make it in common with let function *)
    let (func, loc) = r_split func in
    let ({binders; lhs_type; body} : CST.fun_expr) = func in
    let* lhs_type = bind_map_option (compile_type_expression <@ snd) lhs_type in
    let* (binder,fun_) = compile_parameter binders in
    let* body = self body in
    let expr = fun_ body in
    return @@ e_lambda ~loc binder lhs_type expr
  | EConstr (ESomeApp some) ->
    let ((_, arg), loc) = r_split some in
    let* args = compile_tuple_expression @@ List.Ne.singleton arg in
    return @@ e_some ~loc args
  | EConstr (ENone reg) ->
    let loc = Location.lift reg in
    return @@ e_none ~loc ()
  | EConstr (EConstrApp constr) ->
    let ((constr,args_o), loc) = r_split constr in
    let* args_o = bind_map_option (compile_tuple_expression <@ List.Ne.singleton) args_o in
    let args = Option.unopt ~default:(e_unit ~loc:(Location.lift constr.region) ()) args_o in
    return @@ e_constructor ~loc constr.value args
  | ECase case ->
    let (case, loc) = r_split case in
    let* matchee = self case.expr in
    let (cases, _) = r_split case.cases in
    let* cases = compile_matching_expr @@ npseq_to_ne_list cases in
    return @@ e_matching ~loc matchee cases
  | EAnnot annot ->
    let (annot, loc) = r_split annot in
    let (expr, _, ty) = annot in
    let* expr = self expr in
    let* ty   = compile_type_expression ty in
    return @@ e_annotation ~loc expr ty
  | ECond cond ->
    let (cond, loc) = r_split cond in
    let* test        = self cond.test in
    let* then_clause = self (fst cond.ifso.inside) in
    let* else_clause =
      bind_map_option (fun ((_,x) : _ * _ CST.braced) -> self (fst x.CST.inside))
        cond.ifnot in
    return @@ e_cond ~loc test then_clause @@ Option.unopt ~default:(e_unit ~loc ()) else_clause
  | EList lst -> (
    match lst with
      ECons cons ->
      let (cons, loc) = r_split cons in
      let* a  = self cons.lexpr in
      let* b  = self cons.rexpr in
      return @@ e_constant ~loc (Const C_CONS) [a; b]
    | EListComp lc ->
      let (lc,loc) = r_split lc in
      let lst =
        Option.unopt ~default:[] @@
        Option.map npseq_to_list lc.elements
      in
      let* lst = bind_map_list self lst in
      return @@ e_list ~loc lst
  )
  | ELetIn li -> (
    let (li, loc) = r_split li in
    let ({kwd_rec;binding;body;attributes;_} : CST.let_in) = li in
    let* body = self body in
    match binding with
    | { binders ; let_rhs } -> (
      (* let (pattern,arg) = binders in *)
      match unepar binders with
      | CST.PTuple tuple ->
        let* matchee = compile_expression let_rhs in
        compile_tuple_let_destructuring matchee body tuple
      | CST.PRecord record ->
        let* matchee = compile_expression let_rhs in
        compile_record_let_destructuring matchee body record
      | _ -> (
        let* lst = compile_let_binding ?kwd_rec attributes binding in
        let aux (_name,binder,attr,rhs) expr = e_let_in ~loc binder attr rhs expr in
        return @@ List.fold_right aux lst body
      )
    )
  )
  | ETypeIn ti ->
    let (ti, loc) = r_split ti in
    let ({type_decl={name;type_expr;_};semi=_;body} : CST.type_in) = ti in
    let type_binder = Var.of_name name.value in
    let* rhs = compile_type_expression type_expr in
    let* body = compile_expression body in
    return @@ e_type_in ~loc type_binder rhs body
  | EModIn mi ->
    let (mi, loc) = r_split mi in
    let ({mod_decl={name;module_;_};semi=_;body} : CST.mod_in) = mi in
    let module_binder = name.value in
    let* rhs = compile_module module_ in
    let* body = compile_expression body in
    return @@ e_mod_in ~loc module_binder rhs body
  | EModAlias ma ->
    let (ma, loc) = r_split ma in
    let ({mod_alias={alias;binders;_};semi=_;body} : CST.mod_alias) = ma in
    let alias   = alias.value in
    let binders,_ = List.Ne.split @@ List.Ne.map r_split @@ npseq_to_ne_list binders in
    let* body = compile_expression body in
    return @@ e_mod_alias ~loc alias binders body
  | ECodeInj ci ->
    let (ci, loc) = r_split ci in
    let (language, _) = r_split ci.language in
    let (language, _) = r_split language in
    let* code = self ci.code in
    return @@ e_raw_code ~loc language code
  | ESeq seq ->
    let (seq, loc) = r_split seq in
    let* seq = bind_map_list self @@ pseq_to_list seq.elements in
    match seq with
      [] -> return @@ e_unit ~loc ()
    | hd :: tl ->
      let rec aux prev = function
       [] ->  return @@ prev
      | hd :: tl -> bind (return <@ e_sequence ~loc prev) @@ aux hd tl
      in
      aux hd @@ tl

and conv : CST.pattern -> (AST.ty_expr AST.pattern,_) result =
  fun p ->
  match unepar p with
  | CST.PVar {var;attributes} ->
    let (var,loc) = r_split var in
    let attributes = attributes |> List.map (fun x -> x.Region.value) |>
                       Tree_abstraction_shared.Helpers.binder_attributes_of_strings in
    let b =
      let var = Location.wrap ~loc @@ Var.of_name var in
      { var ; ascr = None ; attributes }
    in
    ok @@ Location.wrap ~loc @@ P_var b
  | CST.PTuple tuple ->
    let (tuple, loc) = r_split tuple in
    let lst = npseq_to_ne_list tuple in
    let patterns = List.Ne.to_list lst in
    let* nested = bind_map_list conv patterns in
    ok @@ Location.wrap ~loc @@ P_tuple nested
  | CST.PRecord record ->
    let (inj, loc) = r_split record in
    let aux : CST.field_pattern CST.reg -> (label * AST.ty_expr AST.pattern,_) result =
      fun field ->
        let { field_name ; eq=_ ; pattern } : CST.field_pattern = field.value in
        let* pattern = conv pattern in
        ok (AST.Label field_name.value , pattern)
    in
    let* lst = bind_map_ne_list aux @@ npseq_to_ne_list inj.ne_elements in
    let lst = List.Ne.to_list lst in
    let (labels,nested) = List.split lst in
    ok @@ Location.wrap ~loc @@ P_record (labels , nested)
  | CST.PConstr constr_pattern -> (
    match constr_pattern with
    | PFalse p ->
      let loc = Location.lift p in
      ok @@ Location.wrap ~loc @@ P_variant (Label "false" , Location.wrap ~loc P_unit)
    | PTrue p ->
      let loc = Location.lift p in
      ok @@ Location.wrap ~loc @@ P_variant (Label "true" , Location.wrap ~loc P_unit)
    | PNone p ->
      let loc = Location.lift p in
      ok @@ Location.wrap ~loc @@ P_variant (Label "None" , Location.wrap ~loc P_unit)
    | PSomeApp some ->
      let ((_,p), loc) = r_split some in
      let* pattern' = conv p in
      ok @@ Location.wrap ~loc @@ P_variant (Label "Some", pattern')
    | PConstrApp constr_app ->
      let ((constr,p_opt), loc) = r_split constr_app in
      let (l , _loc) = r_split constr in
      let* pv_opt = match p_opt with
        | Some pv -> conv pv
        | None -> ok @@ Location.wrap P_unit
      in
      ok @@ Location.wrap ~loc @@ P_variant (Label l, pv_opt)
  )
  | CST.PList list_pattern -> (
    let* repr = match list_pattern with
    | PListComp p_inj -> (
      let loc = Location.lift p_inj.region in
      match p_inj.value.elements with
      | None ->
        ok @@ Location.wrap ~loc @@ P_list (List [])
      | Some _ -> fail @@ unsupported_pattern_type p
    )
    | PCons p ->
      let loc = Location.lift p.region in
      let (hd, tl) = (p.value.lpattern, p.value.rpattern) in
      let* hd = conv hd in
      let* tl = conv tl in
      ok @@ Location.wrap ~loc @@ P_list (Cons (hd,tl))
    in
    ok repr
  )
  | CST.PUnit p ->
    let loc = Location.lift p.region in
    ok @@ Location.wrap ~loc @@ P_unit
  | _ -> fail @@ unsupported_pattern_type p

and compile_tuple_let_destructuring :
  AST.expression -> AST.expression -> (CST.pattern, CST.comma) Utils.nsepseq CST.reg -> (AST.expression,_) result =
  fun matchee body tuple ->
    let (tuple, loc) = r_split tuple in
    let lst = npseq_to_ne_list tuple in
    let patterns = List.Ne.to_list lst in
    let* nested_patterns = bind_map_list conv patterns in
    let pattern = Location.wrap @@ P_tuple nested_patterns in
    let cases : (AST.expression , AST.ty_expr) AST.match_case = { pattern ; body } in
    ok @@ e_matching ~loc matchee [cases]

and compile_record_let_destructuring :
  AST.expression -> AST.expression -> CST.field_pattern CST.reg CST.ne_injection CST.reg -> (AST.expression,_) result =
  fun matchee body record ->
    let (record, loc) = r_split record in
    let aux : CST.field_pattern CST.reg -> (label * CST.pattern,_) result = fun field ->
      let { field_name ; eq=_ ; pattern } : CST.field_pattern = field.value in
      ok (AST.Label field_name.value , pattern)
    in
    let* lst = bind_map_ne_list aux @@ npseq_to_ne_list record.ne_elements in
    let lst = List.Ne.to_list lst in
    let (labels,patterns) = List.split lst in
    let* nested_patterns = bind_map_list conv patterns in
    let pattern = Location.wrap @@ P_record (labels , nested_patterns) in
    let cases : (AST.expression , AST.ty_expr) AST.match_case = { pattern ; body } in
    ok @@ e_matching ~loc matchee [cases]

and compile_matching_expr :  'a CST.case_clause CST.reg List.Ne.t -> ((AST.expression, AST.ty_expr) AST.match_case list, _ ) result =
  fun cases ->
    let aux (case : CST.expr CST.case_clause CST.reg) =
      let (case, _loc) = r_split case in
      let* expr    = compile_expression case.rhs in
      ok (case.pattern, expr)
    in
    let* cases = bind_map_ne_list aux cases in
    let cases : (CST.pattern * AST.expression) list = List.Ne.to_list cases in
    let aux : (CST.pattern * AST.expression) -> ((AST.expression , AST.ty_expr) match_case, _) result =
      fun (raw_pattern, body) ->
        let* pattern = conv raw_pattern in
        ok @@ { pattern ; body }
    in
    bind_map_list aux cases

and unepar = function
| CST.PPar { value = { inside; _ }; _ } -> unepar inside
| _ as v -> v

and untpar = function
| CST.TPar { value = { inside; _ }; _ } -> untpar inside
| _ as v -> v

and check_annotation = function
| CST.PVar {var} -> fail (missing_funarg_annotation var)
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
    let no_of_tuple_type_components = List.length (Utils.nsepseq_to_list tval.inside) in
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
  let* lhs_type =
    bind_map_option (compile_type_expression <@ snd) lhs_type in
  let* expr = compile_expression let_rhs in
  let rec aux = function
  | CST.PPar par ->
    let par, _ = r_split par in
    aux par.inside
  | PVar {var=name;attributes=var_attributes} -> (*function or const *)
    let var_attributes = var_attributes |> List.map (fun x -> x.Region.value) |>
                        Tree_abstraction_shared.Helpers.binder_attributes_of_strings in
    let fun_binder = compile_variable name in
    (* This handle the recursion *)
    let* expr = match kwd_rec with
      Some reg ->
        let* lambda = trace_option (recursion_on_non_function expr.location) @@ get_e_lambda expr.expression_content in
        let lhs_type = Option.map (Utils.uncurry t_function) @@ Option.bind_pair (lambda.binder.ascr, lambda.output_type) in
        let* fun_type = trace_option (untyped_recursive_fun reg) @@ lhs_type in
        ok @@ e_recursive ~loc:(Location.lift reg) fun_binder fun_type lambda
    | None   ->
        ok @@ expr
    in
    return_1 @@ (Some name.value, {var=fun_binder;ascr=lhs_type;attributes = var_attributes}, attributes, expr)
  | _ -> fail @@ unsupported_pattern_type @@ binders
  in aux binders

and compile_parameter : CST.pattern -> (_ binder * (_ -> _),_) result =
  fun pattern ->
  let return ?ascr ?(attributes = Stage_common.Helpers.const_attribute) loc fun_ var =
    ok ({var=Location.wrap ~loc var; ascr; attributes}, fun_) in
  let return_1 ?ascr ?(attributes = Stage_common.Helpers.const_attribute) loc var = return ?ascr ~attributes loc (fun e -> e) var in
  match pattern with
    PConstr _ -> fail @@ unsupported_pattern_type pattern
  | PUnit the_unit  ->
    let loc = Location.lift the_unit.region in
    return_1 ~ascr:(t_unit ~loc ()) loc @@ Var.fresh ()
  | PVar {var;attributes} ->
    let (var,loc) = r_split var in
    let attributes = attributes |> List.map (fun x -> x.Region.value) |>
                       Tree_abstraction_shared.Helpers.binder_attributes_of_strings in
    return_1 ~attributes loc @@ mk_var var
  | PTuple tuple ->
    let (tuple, loc) = r_split tuple in
    let var = Var.fresh () in
    let aux (binder_lst, fun_) pattern =
      let* (binder,fun_') = compile_parameter pattern in
      ok @@ (binder :: binder_lst, fun_' <@ fun_)
    in
    let* binder_lst, fun_ = bind_fold_right_list aux ([],fun e -> e) @@ npseq_to_list tuple in
    let expr = fun expr -> e_matching_tuple (e_variable @@ Location.wrap var) binder_lst @@ fun_ expr in
    let ascr = Option.bind_list @@ List.map (fun binder -> binder.ascr) binder_lst in
    let ascr = Option.map (t_tuple) ascr in
    return ?ascr loc expr var
  | PPar par ->
    compile_parameter par.value.inside
  | PRecord _ -> fail @@ unsupported_pattern_type pattern
  | PTyped tp ->
    let (tp, loc) = r_split tp in
    let {pattern; type_expr} : CST.typed_pattern = tp in
    let* ascr = compile_type_expression type_expr in
    let* ({var;attributes;_}, exprs) = compile_parameter pattern in
    return ~ascr ~attributes loc exprs @@ Location.unwrap var
  | _ -> fail @@ unsupported_pattern_type pattern

and compile_declaration : CST.declaration -> _ = fun decl ->
  let return reg decl =
    ok @@ List.map (Location.wrap ~loc:(Location.lift reg)) decl in
  let return_1 reg decl = return reg [decl] in
  match decl with
    TypeDecl {value={name; type_expr; _};region} ->
    let (name,_) = r_split name in
    let* type_expr = compile_type_expression type_expr in
    return_1 region @@ AST.Declaration_type {type_binder=Var.of_name name; type_expr}
  | ModuleDecl {value={name; module_; _};region} ->
    let (name,_) = r_split name in
    let* module_ = compile_module module_ in
    return_1 region @@ AST.Declaration_module  {module_binder=name; module_}
  | ModuleAlias {value={alias; binders; _};region} ->
    let (alias,_) = r_split alias in
    let binders,_ = List.Ne.split @@ List.Ne.map r_split @@ npseq_to_ne_list binders in
    return_1 region @@ AST.Module_alias {alias ; binders}
  | Directive _ -> ok []

  | ConstDecl {value = (_kwd_let, kwd_rec, let_binding, attributes); region} ->
    match let_binding with
    | { binders ; let_rhs } -> (
      match (unepar binders) with
      | CST.PTuple tuple ->
        let attributes = compile_attributes attributes in
        let* matchee = compile_expression let_rhs in
        let tuple,_loc = r_split tuple in
        let* lst = bind_map_list compile_parameter @@ npseq_to_list tuple in
        let (lst, exprs) = List.split lst in
        let expr = List.fold_right (@@) exprs matchee in
        let aux i binder = Z.add i Z.one, (None, binder, attributes, e_accessor expr @@ [Access_tuple i]) in
        let lst = List.fold_map aux Z.zero @@ lst in
        let aux (name, binder,attr, expr) =  AST.Declaration_constant {name; binder; attr; expr} in
        return region @@ List.map aux lst
      | CST.PRecord record ->
        let attributes = compile_attributes attributes in
        let* matchee = compile_expression let_rhs in
        let record,_loc = r_split record in
        let aux ({value={field_name;eq=_;pattern};_}:CST.field_pattern CST.reg) =
          let field_name = field_name.value in
          let* binder,fun_ = compile_parameter pattern in
          ok @@ ((field_name,binder),fun_)
        in
        let* lst = bind_map_list aux @@ npseq_to_list record.ne_elements in
        let (lst, exprs) = List.split lst in
        let expr = List.fold_right (@@) exprs matchee in
        let aux (field_name,binder) = (None, binder, attributes, e_accessor expr @@ [Access_record field_name]) in
        let lst = List.map aux @@ lst in
        let aux (name, binder,attr, expr) =  AST.Declaration_constant {name; binder; attr; expr} in
        return region @@ List.map aux lst
      | _ -> (
        let* lst = compile_let_binding ?kwd_rec attributes let_binding in
        let aux (name, binder,attr, expr) =  AST.Declaration_constant {name; binder; attr; expr} in
        return region @@ List.map aux lst
      )
    )

and compile_module : CST.ast -> _ result = fun t ->
    let* lst = bind_map_list compile_declaration @@ nseq_to_list t.decl in
    ok @@ List.flatten lst
