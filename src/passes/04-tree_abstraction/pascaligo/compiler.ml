open Errors
open Trace
open Function

module CST = Cst.Pascaligo
module AST = Ast_imperative

open AST

let nseq_to_list (hd, tl) = hd :: tl
let npseq_to_list (hd, tl) = hd :: (List.map snd tl)
let npseq_to_ne_list (hd, tl) = (hd, List.map snd tl)
let build_ins = ["Test";"Tezos";"Crypto";"Bytes";"List";"Set";"Map";"Big_map";"Bitwise";"String";"Layout"]

open Predefined.Tree_abstraction.Pascaligo

let r_split = Location.r_split

let mk_var var = if String.compare var Var.wildcard = 0 then Var.fresh () else Var.of_name var

let compile_attributes : CST.attributes -> AST.attributes = fun attributes ->
  List.map (fst <@ r_split) attributes

let rec compile_type_expression : CST.type_expr ->_ result =
  fun te ->
  let self = compile_type_expression in
  let return te = ok @@ te in
  match te with
    TSum sum ->
      let sum_type, loc = r_split sum in
      let {variants; attributes; _} : CST.sum_type = sum_type in
      let attr = compile_attributes attributes in
      let lst = npseq_to_list variants in
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
        let* type_expr = self f.field_type in
        let field_attr = compile_attributes f.attributes in
        return @@ (f.field_name.value, type_expr, field_attr) in
      let* fields = bind_map_list aux lst in
      return @@ t_record_ez_attr ~loc ~attr:attributes fields
  | TProd prod ->
    let (nsepseq, loc) = r_split prod in
    let lst = npseq_to_list nsepseq in
    let* lst = bind_map_list (self) lst
    in return @@ t_tuple ~loc lst
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
    let ((type_constant,args), loc) = r_split app in
    (* this is a bad design, michelson_or and pair should be an type_constant
       see AnnotType *)
    (match type_constant.value with
      | "michelson_or" ->
        let lst = npseq_to_list args.value.inside in
        (match lst with
        | [a ; b ; c ; d ] -> (
          let* b' =
            trace_option (michelson_type_wrong te type_constant.value) @@
              get_t_string_singleton_opt b in
          let* d' =
            trace_option (michelson_type_wrong te type_constant.value) @@
              get_t_string_singleton_opt d in
          let* a' = self a in
          let* c' = self c in
          return @@ t_michelson_or ~loc a' b' c' d'
          )
        | _ -> fail @@ michelson_type_wrong_arity loc type_constant.value)
      | "michelson_pair" ->
        let lst = npseq_to_list args.value.inside in
        (match lst with
        | [a ; b ; c ; d ] -> (
          let* b' =
            trace_option (michelson_type_wrong te type_constant.value) @@
              get_t_string_singleton_opt b in
          let* d' =
            trace_option (michelson_type_wrong te type_constant.value) @@
              get_t_string_singleton_opt d in
          let* a' = self a  in
          let* c' = self c  in
          return @@ t_michelson_pair ~loc a' b' c' d'
          )
        | _ -> fail @@ michelson_type_wrong_arity loc type_constant.value)
      | "sapling_state" ->
        let lst = npseq_to_list args.value.inside in
        (match lst with
        | [(a : CST.type_expr)] -> (
          let sloc = Location.lift @@ Raw.type_expr_to_region a in
          let* a' =
            trace_option (michelson_type_wrong te type_constant.value) @@
              get_t_int_singleton_opt a in
          let singleton = t_singleton ~loc:sloc (Literal_int a') in
          return @@ t_sapling_state ~loc singleton
          )
        | _ -> fail @@ michelson_type_wrong_arity loc type_constant.value)
      | "sapling_transaction" ->
        let lst = npseq_to_list args.value.inside in
        (match lst with
        | [(a : CST.type_expr)] -> (
          let sloc = Location.lift @@ Raw.type_expr_to_region a in
          let* a' =
            trace_option (michelson_type_wrong te type_constant.value) @@
              get_t_int_singleton_opt a in
          let singleton = t_singleton ~loc:sloc (Literal_int a') in
          return @@ t_sapling_transaction ~loc singleton
          )
        | _ -> fail @@ michelson_type_wrong_arity loc type_constant.value)
    | _ ->
      let operator = Var.of_name type_constant.value in
      let lst = npseq_to_list args.value.inside in
      let* lst = bind_map_list self lst in
      return @@ t_app ~loc operator lst
    )
  | TFun func ->
    let ((input_type,_,output_type), loc) = r_split func in
    let* input_type = self input_type  in
    let* output_type = self output_type  in
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


and compile_selection (selection : CST.selection) =
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
  let compile_tuple_expression (tuple_expr : CST.tuple_expr) =
    let (lst, loc) = r_split tuple_expr in
    let* lst = bind_map_list self @@ npseq_to_list lst.inside in
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
  (* This case is due to a bad besign of our constant it has to change
    with the new typer see LIGO-684 on Jira *)
  | ECall {value=(EVar var,args);region} ->
    let loc = Location.lift region in
    let (var, loc_var) = r_split var in
    (match constants var with
      Some const ->
      let (args, _) = r_split args in
      let* args = bind_map_list self @@ npseq_to_list args.inside in
      return @@ e_constant ~loc const args
    | None ->
      let func = e_variable_ez ~loc:loc_var var in
      let* args = compile_tuple_expression args in
      return @@ e_application ~loc func args
    )
  (*TODO: move to proper module*)
  | ECall {value=(EModA {value={module_name;field};region=_},args);region} when
    List.mem module_name.value build_ins ->
    let loc = Location.lift region in
    let* fun_name = match field with
      EVar v -> ok @@ v.value | EModA _ -> fail @@ unknown_constant module_name.value loc
      |ECase _|ECond _|EAnnot _|EList _|EConstr _|EUpdate _|EFun _|ECodeInj _
      |ELogic _|EArith _|EString _|ERecord _|EProj _|ECall _|EBytes _|EUnit _|ETuple _|EPar _
      |ESet _|EMap _|EBlock _ -> failwith "Corner case : This couldn't be produce by the parser"
    in
    let var = module_name.value ^ "." ^ fun_name in
    (match constants var with
      Some const ->
      let (args, _) = r_split args in
      let* args = bind_map_list self @@ npseq_to_list args.inside in
      return @@ e_constant ~loc const args
    | None ->
      fail @@ unknown_constant var loc
      )
  | ECall call ->
    let ((func, args), loc) = r_split call in
    let* func = self func in
    let* args = compile_tuple_expression args in
    return @@ e_application ~loc func args
  | ETuple lst ->
    compile_tuple_expression lst
  | ERecord record ->
    let (record, loc) = r_split record in
    let aux (fa : CST.field_assignment CST.reg) =
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
      |ECase _|ECond _|EAnnot _|EList _|EConstr _|EUpdate _|EFun _|ECodeInj _
      |ELogic _|EArith _|EString _|ERecord _|EProj _|ECall _|EBytes _|EUnit _|ETuple _|EPar _
      |ESet _|EMap _|EBlock _ -> failwith "Corner case : This couldn't be produce by the parser"
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
    let compile_param (param : CST.param_decl) =
      match param with
        ParamConst p ->
        let (p, _) = r_split p in
        let (var, loc) = r_split p.var in
        let* p_type =
          bind_map_option (compile_type_expression  <@ snd)
                          p.param_type in
        return {var=Location.wrap ~loc @@ Var.of_name var;ascr=p_type;attributes=Stage_common.Helpers.const_attribute}
      | ParamVar p ->
        let (p, _) = r_split p in
        let (var, loc) = r_split p.var in
        let* p_type =
          bind_map_option (compile_type_expression  <@ snd)
                          p.param_type in
        return {var=Location.wrap ~loc @@ Var.of_name var;ascr=p_type;attributes=Stage_common.Helpers.var_attribute} in
    let (func, loc) = r_split func in
    let (param, loc_par)  = r_split func.param in
    let* param =
      bind_map_list compile_param @@ npseq_to_list param.inside in
    let* ret_type =
      bind_map_option (compile_type_expression  <@ snd )
                      func.ret_type in
    let* body = self func.return in
    let (lambda, fun_type) = match param with
      binder::[] ->
      e_lambda ~loc binder ret_type body,
      Option.map (fun (a,b) -> t_function a b)@@ Option.bind_pair (binder.ascr,ret_type)
    (* Cannot be empty EDIT Use "| _::_ as lst -> ... | [] -> assert false" *)
    | lst ->
      let input_type = Option.map t_tuple @@ Option.bind_list @@ List.map (fun b -> b.ascr) lst in
      let binder = Location.wrap ~loc:loc_par @@ Var.fresh ~name:"parameter" () in
      e_lambda_ez ~loc binder ?ascr:input_type (ret_type) @@
        e_matching_tuple ~loc:loc_par (e_variable binder) param body,
      Option.map (fun (a,b) -> t_function a b)@@ Option.bind_pair (input_type,ret_type)
    in
    return @@ Option.unopt ~default:lambda @@
      Option.map (e_annotation ~loc lambda) fun_type
  | EConstr (SomeApp some) ->
    let ((_, arg), loc) = r_split some in
    let* args = compile_tuple_expression arg in
    return @@ e_some ~loc args
  | EConstr (NoneExpr reg) ->
    let loc = Location.lift reg in
    return @@ e_none ~loc ()
  | EConstr (ConstrApp constr) ->
    let ((constr,args_o), loc) = r_split constr in
    let* args_o = bind_map_option compile_tuple_expression args_o in
    let args = Option.unopt ~default:(e_unit ~loc:(Location.lift constr.region) ()) args_o in
    return @@ e_constructor ~loc constr.value args
  | ECase case ->
    let (case, loc) = r_split case in
    let* matchee = self case.expr in
    let (cases, _) = r_split case.cases in
    let* cases = compile_matching_expr self @@ npseq_to_ne_list cases in
    return @@ e_matching ~loc matchee cases
  | EAnnot annot ->
    let (annot, loc) = r_split annot in
    let (expr, _, ty) = annot.inside in
    let* expr = self expr in
    let* ty   = compile_type_expression ty  in
    return @@ e_annotation ~loc expr ty
  | ECond cond ->
    let (cond, loc) = r_split cond in
    let* test        = self cond.test in
    let* then_clause = self cond.ifso in
    let* else_clause = self cond.ifnot in
    return @@ e_cond ~loc test then_clause else_clause
  | EList lst -> (
    match lst with
      ECons cons ->
      let (cons, loc) = r_split cons in
      let* a  = self cons.arg1 in
      let* b  = self cons.arg2 in
      return @@ e_constant ~loc (Const C_CONS) [a; b]
    | EListComp lc ->
      let (lc,loc) = r_split lc in
      let lst =
        Option.unopt ~default:[] @@
        Option.map npseq_to_list lc.elements
      in
      let* lst = bind_map_list self lst in
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
      let* set = bind_map_list self set in
      return @@ e_set ~loc set
    | SetMem sm ->
      let (sm, loc) = r_split sm in
      let* set  = self sm.set in
      let* elem = self sm.element in
      return @@ e_constant ~loc (Const C_SET_MEM) [elem;set]
  )
  | EMap map -> (
    match map with
      MapLookUp mlu ->

        let (mlu, loc) = r_split mlu in
        let* path  = compile_path mlu.path in
        let (index, _) = r_split mlu.index in
        let* index = self index.inside in
        return @@ e_accessor ~loc path [Access_map index]
    | MapInj mij ->
      let (mij, loc) = r_split mij in
      let lst = Option.unopt ~default:[] @@
        Option.map npseq_to_list mij.elements in
      let aux (binding : CST.binding CST.reg) =
        let (binding, _) = r_split binding in
        let* key   = self binding.source in
        let* value = self binding.image in
        return (key,value)
      in
      let* map = bind_map_list aux lst in
      return @@ e_map ~loc map
    | BigMapInj mij ->
      let (mij, loc) = r_split mij in
      let lst = Option.unopt ~default:[] @@
        Option.map npseq_to_list mij.elements in
      let aux (binding : CST.binding CST.reg) =
        let (binding, _) = r_split binding in
        let* key   = self binding.source in
        let* value = self binding.image in
        return (key,value)
      in
      let* map = bind_map_list aux lst in
      return @@ e_big_map ~loc map
  )
  | ECodeInj ci ->
    let (ci, loc) = r_split ci in
    let (language, _) = r_split ci.language in
    let (language, _) = r_split language in
    let* code = self ci.code in
    return @@ e_raw_code ~loc language code
  | EBlock be ->
    let be, _ = r_split be in
    let* next = self be.expr in
    compile_block ~next be.block

and conv : ?const:bool -> CST.pattern -> (AST.ty_expr AST.pattern,_) result =
  fun ?(const = false) p ->
  match p with
  | CST.PVar var ->
     let (var,loc) = r_split var in
     let attributes = if const then Stage_common.Helpers.const_attribute else Stage_common.Helpers.var_attribute in
    let b =
      let var = Location.wrap ~loc @@ Var.of_name var in
      { var ; ascr = None ; attributes }
    in
    ok @@ Location.wrap ~loc @@ P_var b
  | CST.PTuple tuple -> (
    let (tuple, loc) = r_split tuple in
    let lst = npseq_to_ne_list tuple.inside in
    let patterns = List.Ne.to_list lst in
    let* nested = bind_map_list (conv ~const) patterns in
    match nested with (* (x) == x *)
    | [x] -> ok x
    | _ -> ok @@ Location.wrap ~loc @@ P_tuple nested
  )
  | CST.PConstr constr_pattern -> (
    match constr_pattern with
    | PUnit p ->
      let loc = Location.lift p in
      ok @@ Location.wrap ~loc @@ P_unit
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
      let* pattern' = conv ~const p in
      ok @@ Location.wrap ~loc @@ P_variant (Label "Some", pattern')
    | PConstrApp constr_app ->
      let ((constr,p_opt), loc) = r_split constr_app in
      let (l , _loc) = r_split constr in
      let* pv_opt = match p_opt with
        | Some p -> conv ~const (CST.PTuple p)
        | None -> ok @@ Location.wrap ~loc P_unit
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
      | Some lst ->
        let lst = Utils.nsepseq_to_list lst in
        let aux : AST.type_expression AST.pattern -> CST.pattern -> (AST.type_expression AST.pattern,_) result =
          fun acc p ->
            let* p' = conv ~const p in
            ok @@ Location.wrap (P_list (Cons (p', acc)))
        in
        let* conscomb = bind_fold_right_list aux (Location.wrap ~loc (P_list (List []))) lst in 
        ok conscomb
    )
    | PParCons p ->
      let (hd, _, tl) = p.value.inside in
      let loc = Location.lift p.region in
      let* hd = conv ~const hd in
      let* tl = conv ~const tl in
      ok @@ Location.wrap ~loc @@ P_list (Cons (hd,tl))
    | PCons l -> (
      let loc = Location.lift l.region in
      let patterns  = Utils.nsepseq_to_list l.value in
      match patterns with
      | [ hd ; tl ] ->
        let* hd = conv ~const hd in
        let* tl = conv ~const tl in
        ok @@ Location.wrap ~loc @@ P_list (Cons (hd,tl))
      | _ -> fail @@ unsupported_pattern_type p
    )
    | PNil n ->
      let loc = Location.lift n in
      ok @@ Location.wrap ~loc @@ P_list (List [])
    in
    ok @@ repr
  )
  | CST.PRecord record_pattern -> (
    let (inj,loc) = r_split record_pattern in
    let lst = Utils.sepseq_to_list inj.elements in
    let aux : CST.field_pattern CST.reg -> (AST.label * AST.ty_expr AST.pattern, _) result =
      fun x ->
        let (field_pattern, _) = r_split x in
        let* pattern = conv ~const field_pattern.pattern in
        ok (AST.Label field_pattern.field_name.value , pattern)
    in
    let* lst' = bind_map_list aux lst in
    let (labels,patterns) = List.split lst' in
    ok @@ Location.wrap ~loc (P_record (labels,patterns))
  )
  | _ -> fail @@ unsupported_pattern_type p

and compile_matching_expr : type a . (a-> (AST.expression,_) result) -> a CST.case_clause CST.reg List.Ne.t -> ((AST.expression, AST.ty_expr) AST.match_case list, _ ) result =
  fun compiler cases ->
    let aux (case : a CST.case_clause CST.reg) =
      let (case, _loc) = r_split case in
      let* expr    = compiler case.rhs in
      ok (case.pattern, expr)
    in
    let* cases = bind_map_ne_list aux cases in
    let cases : (CST.pattern * AST.expression) list = List.Ne.to_list cases in
    let aux : (CST.pattern * AST.expression) -> ((AST.expression , AST.ty_expr) match_case, _) result =
      fun (raw_pattern, body) ->
        let* pattern = conv ~const:true raw_pattern in
        ok @@ { pattern ; body }
    in
    bind_map_list aux cases

and compile_parameters (params : CST.parameters) =
  let compile_param_decl (param : CST.param_decl) =
    let return = ok in
    match param with
      ParamConst pc ->
      let (pc, _loc) = r_split pc in
      let (var, loc) = r_split pc.var in
      let var = Location.wrap ~loc @@ Var.of_name var in
      let* param_type =
        bind_map_option (compile_type_expression <@ snd)
                        pc.param_type in
      return {var;ascr= param_type ; attributes = Stage_common.Helpers.const_attribute}
    | ParamVar pv ->
      let (pv, _loc) = r_split pv in
      let (var, loc) = r_split pv.var in
      let var = Location.wrap ~loc @@ Var.of_name var in
      let* param_type =
        bind_map_option (compile_type_expression  <@ snd)
                        pv.param_type in
      return {var; ascr=param_type; attributes = Stage_common.Helpers.var_attribute}
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
    let* lst = bind_map_list compile_expression @@ npseq_to_list lst.inside in
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
      let* (_, var, path) = compile_path path in
      ok @@ (var, path)
    | MapPath (mlu) ->
      let (mlu, _loc) = r_split mlu in
      let* (_, var, path) = compile_path mlu.path in
      let* index = compile_expression @@ mlu.index.value.inside in
      ok @@ (var, path @ [Access_map index])
  in
  match instruction with
    Cond c ->
    let (c, loc) = r_split c in
    let* test = compile_expression c.test in
    let* ifso = compile_if_clause c.ifso in
    let* ifnot = compile_if_clause c.ifnot in
    return @@ e_cond ~loc test ifso ifnot
  | CaseInstr ci ->
    let (ci, loc) = r_split ci in
    let* matchee = compile_expression ci.expr in
    let* cases = compile_matching_expr compile_if_clause @@ npseq_to_ne_list ci.cases.value in
    return @@ e_matching ~loc matchee cases
  | Assign a ->
    let (a,loc) = r_split a in
    let* (var,path) = compile_lhs a.lhs in
    let* rhs = compile_expression a.rhs in
    return @@ e_assign_ez ~loc var path rhs
  | Loop (While wl) ->
    let (wl, loc) = r_split wl in
    let* cond = compile_expression wl.cond in
    let* body = compile_block wl.block in
    return @@ e_while ~loc cond body
  | Loop (For (ForInt fl)) ->
    let (fl, loc) = r_split fl in
    let (binder, binder_loc) = r_split fl.binder in
    let* start = compile_expression fl.init in
    let* bound = compile_expression fl.bound in
    let* increment = Option.unopt ~default:(ok @@ e_int_z Z.one) @@
      Option.map (compile_expression <@ snd) fl.step
    in
    let* body  = compile_block fl.block in
    return @@ e_for ~loc (Location.wrap ~loc:binder_loc @@ Var.of_name binder) start bound increment body
  | Loop (For (ForCollect el)) ->
    let (el, loc) = r_split el in
    let binder =
      let (key, loc) = r_split el.var in
      let key' = Location.wrap ~loc @@ Var.of_name key in
      let value = Option.map
        (fun x ->
          let (v,loc) = r_split (snd x) in
          Location.wrap ~loc @@ Var.of_name v)
        el.bind_to in
      (key',value)
    in
    let* collection = compile_expression el.expr in
    let (collection_type, _) = match el.collection with
      Map loc -> (Map, loc) | Set loc -> (Set, loc) | List loc -> (List, loc)
    in
    let* body = compile_block el.block in
    return @@ e_for_each ~loc binder collection collection_type body
  | ProcCall {value=(EVar var,args);region} ->
    let loc = Location.lift region in
    let (var, loc_var) = r_split var in
    (match constants var with
      Some const ->
      let (args, _) = r_split args in
      let* args = bind_map_list compile_expression @@ npseq_to_list args.inside in
      return @@ e_constant ~loc const args
    | None ->
      let func = e_variable_ez ~loc:loc_var var in
      let* args = compile_tuple_expression args in
      return @@ e_application ~loc func args
    )
  (*TODO: move to proper module*)
  | ProcCall {value=(EModA {value={module_name;field};region=_},args);region} when
    List.mem module_name.value build_ins ->
    let loc = Location.lift region in
    let* fun_name = match field with
      EVar v -> ok @@ v.value
      | EModA _ -> fail @@ unknown_constant module_name.value loc
      |ECase _|ECond _|EAnnot _|EList _|EConstr _|EUpdate _|EFun _|ECodeInj _
      |ELogic _|EArith _|EString _|ERecord _|EProj _|ECall _|EBytes _|EUnit _|ETuple _|EPar _
      |ESet _|EMap _|EBlock _ -> failwith "Corner case : This couldn't be produce by the parser"
    in
    let var = module_name.value ^ "." ^ fun_name in
    (match constants var with
      Some const ->
      let (args, _) = r_split args in
      let* args = bind_map_list compile_expression @@ npseq_to_list args.inside in
      return @@ e_constant ~loc const args
    | None ->
      fail @@ unknown_constant var loc
      )
  | ProcCall pc ->
    let (pc, loc) = r_split pc in
    let (func, args) = pc in
    let* func = compile_expression func in
    let* args = compile_tuple_expression args in
    return @@ e_application ~loc func args
  | Skip s ->
    let loc = Location.lift s in
    return @@ e_skip ~loc ()
  | RecordPatch rp ->
    let (rp, loc) = r_split rp in
    let* (record, var, path) = compile_path rp.path in
    let (updates, _) = r_split rp.record_inj in
    let updates = npseq_to_list updates.ne_elements in
    let aux record (update: CST.field_assignment CST.reg) =
      let (update,loc) = r_split update in
      let path = [Access_record update.field_name.value] in
      let* expr = compile_expression update.field_expr in
      ok @@ e_update ~loc record path expr
    in
    let* new_record = bind_fold_list aux record updates in
    return @@ e_assign_ez ~loc var path @@ new_record
  | MapPatch mp ->
    let (mp, loc) = r_split mp in
    let* (map, var, path) = compile_path mp.path in
    let (updates, _) = r_split mp.map_inj in
    let updates = npseq_to_list updates.ne_elements in
    let aux map (update: CST.binding CST.reg) =
      let (update,loc) = r_split update in
      let* key = compile_expression update.source in
      let* value = compile_expression update.image in
      ok @@ e_map_add ~loc key value map
    in
    let* new_map = bind_fold_list aux map updates in
    return @@ e_assign_ez ~loc var path @@ new_map
  | SetPatch sp ->
    let (sp, loc) = r_split sp in
    let* (set, var, path) = compile_path sp.path in
    let (updates, _) = r_split sp.set_inj in
    let updates = npseq_to_list updates.ne_elements in
    let aux set (update: CST.expr) =
      let* key = compile_expression update in
      ok @@ e_constant ~loc (Const C_SET_ADD) [key; set]
    in
    let* new_map = bind_fold_list aux set updates in
    return @@ e_assign_ez ~loc var path @@ new_map
  | MapRemove mr ->
    let (mr, loc) = r_split mr in
    let* (map, var, path) = compile_path mr.map in
    let* key = compile_expression mr.key in
    return @@ e_assign_ez ~loc var path @@
      e_constant ~loc (Const C_MAP_REMOVE) [key;map]
  | SetRemove sr ->
    let (sr, loc) = r_split sr in
    let* (set, var, path)  = compile_path sr.set in
    let* ele  = compile_expression sr.element in
    return @@ e_assign_ez ~loc var path @@
      e_constant ~loc (Const C_SET_REMOVE) [ele;set]

and compile_let_destructuring :
  ?const:bool -> Location.t -> CST.expr -> CST.pattern -> AST.expression -> AST.type_expression option -> (AST.expression , _) result =
    fun ?(const = false) loc value pattern body ty_opt ->
      let* init = compile_expression value in
      let* pattern = conv ~const pattern in
      let match_case = { pattern ; body } in
      let match_ = e_matching ~loc init [match_case] in
      match ty_opt with
      | Some t -> ok (e_annotation ~loc match_ t)
      | None -> ok match_

and compile_data_declaration : next:AST.expression -> CST.data_decl -> _ =
  fun ~next data_decl ->
  let return loc var ascr var_attr attr init =
    ok @@ e_let_in ~loc {var;ascr;attributes=var_attr} attr init next
  in
  match data_decl with
    LocalConst const_decl -> (
      let cd, loc = r_split const_decl in
      let* type_ = bind_map_option (compile_type_expression <@ snd) cd.const_type in
      match cd.pattern with
      | PVar name -> (
        let name, ploc = r_split name in
        let* init = compile_expression cd.init in
        let p = Location.wrap ~loc:ploc @@ Var.of_name name
        and attr = const_decl.value.attributes in
        let attr = compile_attributes attr in
        return loc p type_ Stage_common.Helpers.const_attribute attr init
      )
      | pattern ->
        (* not sure what to do with  attributes in that case *)
        compile_let_destructuring ~const:true loc cd.init pattern next type_
  )
  | LocalVar var_decl -> (
      let vd, loc = r_split var_decl in
      let* type_ = bind_map_option (compile_type_expression <@ snd) vd.var_type in
      match vd.pattern with
      | PVar name ->
        let name, ploc = r_split name in
        let* init = compile_expression vd.init in
        let p = Location.wrap ~loc:ploc @@ Var.of_name name in
        return loc p type_ Stage_common.Helpers.var_attribute [] init
      | pattern ->
        (* not sure what to do with  attributes in that case *)
        compile_let_destructuring loc vd.init pattern next type_
  )
  | LocalFun fun_decl ->
      let fun_decl, loc = r_split fun_decl in
      let* _fun_name, fun_var, fun_type, attr, lambda =
        compile_fun_decl fun_decl in
      return loc fun_var fun_type Stage_common.Helpers.empty_attribute attr lambda

  | LocalType type_decl ->
    let td,loc = r_split type_decl in
    let name,_ = r_split td.name in
    let* rhs = compile_type_expression td.type_expr in
    let name = Var.of_name name in
    ok @@ e_type_in ~loc name rhs next

  | LocalModule module_decl ->
    let md,loc = r_split module_decl in
    let name,_ = r_split md.name in
    let* rhs = compile_module md.module_ in
    ok @@ e_mod_in ~loc name rhs next

  | LocalModuleAlias module_alias ->
    let ma,loc = r_split module_alias in
    let alias,_ = r_split ma.alias in
    let binders,_ = List.Ne.split @@ List.Ne.map r_split @@ npseq_to_ne_list ma.binders in
    ok @@ e_mod_alias ~loc alias binders next

and compile_statement : ?next:AST.expression -> CST.statement -> _ result =
  fun ?next statement ->
  let return = ok in
  match statement with
    Instr i ->
      let* i = compile_instruction ?next i in
      return (Some i)
  | Data dd ->
    let next = Option.unopt ~default:(e_skip ()) next in
    let* dd = compile_data_declaration ~next dd
    in return (Some dd)

and compile_block : ?next:AST.expression -> CST.block CST.reg -> _ result =
  fun ?next block ->
  let return = ok in
  let (block', _loc) = r_split block in
  let statements = npseq_to_list block'.statements in
  let aux next statement =
    let* statement = compile_statement ?next statement
    in return statement
  in
  let* block' = bind_fold_right_list aux next statements in
  match block' with
    Some block -> return block
  | None -> fail @@ block_start_with_attribute block

and compile_fun_decl : CST.fun_decl -> (string * expression_variable * type_expression option * AST.attributes * expression , _) Trace.result =
  fun ({kwd_recursive; fun_name; param; ret_type; return=r; attributes}: CST.fun_decl) ->
  let return = ok in
  let (fun_name, loc) = r_split fun_name in
  let fun_binder = Location.wrap ~loc @@ Var.of_name fun_name in
  let* ret_type =
    bind_map_option (compile_type_expression <@ snd) ret_type in
  let* param = compile_parameters param in
  let* result = compile_expression r in

  (* This handles the parameter case: *)
  let (lambda, fun_type) =
    match param with
      binder::[] ->
        let lambda : _ AST.lambda = {
          binder;
          output_type = ret_type;
          result}
        in lambda, Option.map (fun (a,b) -> t_function a b)
                   @@ Option.bind_pair (binder.ascr,ret_type)
    | lst ->
        let lst = Option.bind_list @@ List.map (fun e -> e.ascr) lst in
        let input_type = Option.map t_tuple lst in
        let binder = Location.wrap @@ Var.fresh ~name:"parameters" () in
        let lambda : _ AST.lambda = {
          binder={var=binder;ascr=input_type;attributes=Stage_common.Helpers.empty_attribute};
          output_type = ret_type;
          result = e_matching_tuple (e_variable binder) param result;
          } in
        lambda, Option.map (fun (a,b) -> t_function a b)
                @@ Option.bind_pair (input_type,ret_type) in
  (* This handles the recursion *)
  let* func = match kwd_recursive with
    Some reg ->
      let* fun_type =
        trace_option (untyped_recursive_fun loc) @@ fun_type in
      return @@ e_recursive ~loc:(Location.lift reg) fun_binder fun_type lambda
  | None   ->
      return @@ make_e ~loc @@ E_lambda lambda
  in
  let attr = compile_attributes attributes in
  return (fun_name, fun_binder, fun_type, attr, func)

and compile_declaration : CST.declaration -> _ result =
  fun decl ->
  let return reg decl =
    ok @@ [Location.wrap ~loc:(Location.lift reg) decl] in
  match decl with
    TypeDecl {value={name; type_expr; _}; region} ->
    let name, _ = r_split name in
    let* type_expr = compile_type_expression type_expr in
    return region @@ AST.Declaration_type {type_binder=Var.of_name name; type_expr}
  | ConstDecl {value={pattern; const_type; init; attributes; _}; region} -> (
    let attr = compile_attributes attributes in
    match pattern with
    | PVar name ->
      let name, loc = r_split name in
      let var = Location.wrap ~loc @@ Var.of_name name in
      let* ascr =
        bind_map_option (compile_type_expression <@ snd) const_type in
      let* expr = compile_expression init in
      let binder = {var;ascr;attributes=Stage_common.Helpers.const_attribute} in
      return region @@ AST.Declaration_constant {name = Some name; binder;attr;expr}
    | _ ->
      fail (unsupported_top_level_destructuring region)
  )
  | FunDecl {value;region} ->
    let* (name,var,ascr,attr,expr) = compile_fun_decl value in
    let binder = {var;ascr;attributes=Stage_common.Helpers.empty_attribute} in
    let ast = AST.Declaration_constant {name = Some name; binder;attr;expr}
    in return region ast

  | ModuleDecl {value={name; module_; _};region} ->
      let (name,_) = r_split name in
      let* module_ = compile_module module_ in
      let ast = AST.Declaration_module  {module_binder=name; module_}
      in return region ast

  | ModuleAlias {value={alias; binders; _};region} ->
     let alias, _ = r_split alias in
     let binders, _ =
       List.Ne.split @@ List.Ne.map r_split @@ npseq_to_ne_list binders in
     let ast = AST.Module_alias {alias; binders}
     in return region ast

  | Directive _ -> ok []

and compile_module : CST.ast -> _ result =
  fun t ->
    let* lst = bind_map_list compile_declaration @@ nseq_to_list t.decl
    in ok @@ List.flatten lst
