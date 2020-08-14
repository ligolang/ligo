module AST = Ast_imperative
module CST = Cst.Cameligo
module Predefined = Predefined.Tree_abstraction.Cameligo

open Trace
open Function

(* Utils *)
let rg = Region.ghost
let wrap : type a. a -> a CST.reg = fun value -> {value;region=rg}
let list_to_sepseq lst =
  match lst with
    [] -> None
  |  hd :: lst ->
      let aux e = (rg, e) in
      Some (hd, List.map aux lst)
let list_to_nsepseq lst =
  match list_to_sepseq lst with
    Some s -> ok @@ s
  | None   -> failwith "List is empty"

let nelist_to_npseq (hd, lst) = (hd, List.map (fun e -> (rg, e)) lst)
let npseq_cons hd lst = hd,(rg, fst lst)::(snd lst)

let par a = CST.{lpar=rg;inside=a;rpar=rg}
let inject compound a = CST.{compound;elements=a;terminator=Some(rg)}
let ne_inject compound a = CST.{compound;ne_elements=a;terminator=Some(rg)}
let prefix_colon a = (rg, a)
let braces = Some (CST.Braces (rg,rg))
let brackets = Some (CST.Brackets (rg,rg))
let beginEnd = Some (CST.BeginEnd (rg,rg))

(* Decompiler *)

let decompile_variable : type a. a Var.t -> CST.variable = fun var ->
  let var = Format.asprintf "%a" Var.pp var in
  if String.contains var '#' then
    let var = String.split_on_char '#' var in
    wrap @@ "gen__" ^ (String.concat "" var)
  else
    if String.length var > 4 && String.equal "gen__" @@ String.sub var 0 5 then
      wrap @@ "user__" ^ var
    else
      wrap @@ var

let rec decompile_type_expr : AST.type_expression -> _ result = fun te ->
  let return te = ok @@ te in
  match te.type_content with
    T_sum sum ->
    let sum = AST.LMap.to_kv_list sum in
    let aux (AST.Label c, AST.{associated_type;_}) =
      let constr = wrap c in
      let%bind arg = decompile_type_expr associated_type in
      let arg = Some (rg, arg) in
      let variant : CST.variant = {constr;arg} in
      ok @@ wrap variant
    in
    let%bind sum = bind_map_list aux sum in
    let%bind sum = list_to_nsepseq sum in
    return @@ CST.TSum (wrap sum)
  | T_record record ->
    let record = AST.LMap.to_kv_list record in
    let aux (AST.Label c, AST.{associated_type;_}) =
      let field_name = wrap c in
      let colon = rg in
      let%bind field_type = decompile_type_expr associated_type in
      let variant : CST.field_decl = {field_name;colon;field_type} in
      ok @@ wrap variant
    in
    let%bind record = bind_map_list aux record in
    let%bind record = list_to_nsepseq record in
    return @@ CST.TRecord (wrap @@ ne_inject (braces) record)
  | T_tuple tuple ->
    let%bind tuple = bind_map_list decompile_type_expr tuple in
    let%bind tuple = list_to_nsepseq @@ tuple in
    return @@ CST.TProd (wrap tuple)
  | T_arrow {type1;type2} ->
    let%bind type1 = decompile_type_expr type1 in
    let%bind type2 = decompile_type_expr type2 in
    let arrow = (type1, rg, type2) in
    return @@ CST.TFun (wrap arrow)
  | T_variable var ->
    let var = decompile_variable var in
    return @@ CST.TVar (var)
  | T_wildcard ->
    return @@ CST.TWild rg
  | T_constant (const, []) ->
    let const = Predefined.type_constant_to_string const in
    return @@ CST.TVar (wrap const)
  | T_constant (type_constant, lst) ->
    let type_constant = wrap @@ Predefined.type_constant_to_string type_constant in
    let%bind lst = bind_map_list decompile_type_expr lst in
    let%bind lst = list_to_nsepseq lst in
    let lst : _ CST.par = {lpar=rg;inside=lst;rpar=rg} in
    return @@ CST.TApp (wrap (type_constant,wrap lst))
  | T_annoted _annot ->
    failwith "let's work on it later"

let get_e_variable : AST.expression -> _ result = fun expr ->
  match expr.expression_content with
    E_variable var -> ok @@ var.wrap_content
  | _ -> failwith @@
    Format.asprintf "%a should be a variable expression"
    AST.PP.expression expr

let get_e_tuple : AST.expression -> _ result = fun expr ->
  match expr.expression_content with
    E_tuple tuple -> ok @@ tuple
  | E_variable _
  | E_literal _
  | E_constant _
  | E_lambda _ -> ok @@ [expr]
  | _ -> failwith @@
    Format.asprintf "%a should be a tuple expression"
    AST.PP.expression expr

let pattern_type var ty_opt =
  let var = CST.PVar (decompile_variable var) in
  match ty_opt with
    Some s ->
      let%bind type_expr = decompile_type_expr s in
      ok @@ CST.PTyped (wrap @@ CST.{pattern=var;colon=rg;type_expr})
  | None -> ok @@ var

let rec decompile_expression : AST.expression -> _ result = fun expr ->
  let return_expr expr = ok @@ expr in
  let return_expr_with_par expr = return_expr @@ CST.EPar (wrap @@ par @@ expr) in
  match expr.expression_content with
    E_variable name ->
    let var = decompile_variable name.wrap_content in
    return_expr @@ CST.EVar (var)
  | E_constant {cons_name; arguments} ->
    let expr = CST.EVar (wrap @@ Predefined.constant_to_string cons_name) in
    (match arguments with
      [] -> return_expr @@ expr
    | _ ->
      let%bind arguments = map List.Ne.of_list @@
        map (List.map (fun x -> CST.EPar (wrap @@ par @@ x))) @@
        bind_map_list decompile_expression arguments in
      let const = wrap (expr, arguments) in
      return_expr_with_par @@ CST.ECall const
    )
  | E_literal literal ->
    (match literal with
        Literal_unit  ->  return_expr @@ CST.EUnit (wrap (rg,rg))
      | Literal_int i ->  return_expr @@ CST.EArith (Int (wrap ("",i)))
      | Literal_nat n ->  return_expr @@ CST.EArith (Nat (wrap ("",n)))
      | Literal_timestamp time ->
        let time = Tezos_utils.Time.Protocol.to_notation @@
          Tezos_utils.Time.Protocol.of_seconds @@ Z.to_int64 time in
          (* TODO combinators for CSTs. *)
        let%bind ty = decompile_type_expr @@ AST.t_timestamp () in
        let time = CST.EString (String (wrap time)) in
        return_expr @@ CST.EAnnot (wrap @@ par (time, rg, ty))
      | Literal_mutez mtez -> return_expr @@ CST.EArith (Mutez (wrap ("",mtez)))
      | Literal_string (Standard str) -> return_expr @@ CST.EString (String   (wrap str))
      | Literal_string (Verbatim ver) -> return_expr @@ CST.EString (Verbatim (wrap ver))
      | Literal_bytes b ->
        let b = Hex.of_bytes b in
        let s = Hex.to_string b in
        return_expr @@ CST.EBytes (wrap (s,b))
      | Literal_address addr ->
        let addr = CST.EString (String (wrap addr)) in
        let%bind ty = decompile_type_expr @@ AST.t_address () in
        return_expr @@ CST.EAnnot (wrap @@ par (addr,rg,ty))
      | Literal_signature sign ->
        let sign = CST.EString (String (wrap sign)) in
        let%bind ty = decompile_type_expr @@ AST.t_signature () in
        return_expr @@ CST.EAnnot (wrap @@ par (sign,rg,ty))
      | Literal_key k ->
        let k = CST.EString (String (wrap k)) in
        let%bind ty = decompile_type_expr @@ AST.t_key () in
        return_expr @@ CST.EAnnot (wrap @@ par (k,rg,ty))
      | Literal_key_hash kh ->
        let kh = CST.EString (String (wrap kh)) in
        let%bind ty = decompile_type_expr @@ AST.t_key_hash () in
        return_expr @@ CST.EAnnot (wrap @@ par (kh,rg,ty))
      | Literal_chain_id _
      | Literal_operation _ ->
        failwith "chain_id, operation are not created currently ?"
    )
  | E_application {lamb;args} ->
    let%bind lamb = decompile_expression lamb in
    let%bind args = map List.Ne.of_list @@
      bind (bind_map_list decompile_expression) @@
      get_e_tuple args
    in
    return_expr @@ CST.ECall (wrap (lamb,args))
  | E_lambda lambda ->
    let%bind (binders,_lhs_type,_block_with,body) = decompile_lambda lambda in
    let fun_expr : CST.fun_expr = {kwd_fun=rg;binders;lhs_type=None;arrow=rg;body} in
    return_expr_with_par @@ CST.EFun (wrap @@ fun_expr)
  | E_recursive _ ->
    failwith "corner case : annonymous recursive function"
  | E_let_in {let_binder;rhs;let_result;inline} ->
    let var = CST.PVar (decompile_variable @@ (fst let_binder).wrap_content) in
    let binders = (var,[]) in
    let%bind lhs_type = bind_map_option (bind_compose (ok <@ prefix_colon) decompile_type_expr) @@ snd let_binder in
    let%bind let_rhs = decompile_expression rhs in
    let binding : CST.let_binding = {binders;lhs_type;eq=rg;let_rhs} in
    let%bind body = decompile_expression let_result in
    let attributes = decompile_attributes inline in
    let lin : CST.let_in = {kwd_let=rg;kwd_rec=None;binding;kwd_in=rg;body;attributes} in
    return_expr @@ CST.ELetIn (wrap lin)
  | E_raw_code {language; code} ->
    let language = wrap @@ wrap @@ language in
    let%bind code = decompile_expression code in
    let ci : CST.code_inj = {language;code;rbracket=rg} in
    return_expr @@ CST.ECodeInj (wrap ci)
  | E_constructor {constructor;element} ->
    let Label constr = constructor in
    let constr = wrap constr in
    let%bind element = decompile_expression element in
    return_expr_with_par @@ CST.EConstr (EConstrApp (wrap (constr, Some element)))
  | E_matching {matchee; cases} ->
    let%bind expr  = decompile_expression matchee in
    let%bind cases = decompile_matching_cases cases in
    let cases : _ CST.case = {kwd_match=rg;expr;kwd_with=rg;lead_vbar=None;cases} in
    return_expr @@ CST.ECase (wrap cases)
  | E_record record  ->
    let record = AST.LMap.to_kv_list record in
    let aux (AST.Label str, expr) =
      let field_name = wrap str in
      let%bind field_expr = decompile_expression expr in
      let field : CST.field_assign = {field_name;assignment=rg;field_expr} in
      ok @@ wrap field
    in
    let%bind record = bind_map_list aux record in
    let%bind record = list_to_nsepseq record in
    let record = ne_inject braces record in
    (* why is the record not empty ? *)
    return_expr @@ CST.ERecord (wrap record)
  | E_accessor {record; path} ->
    (match List.rev path with
      Access_map e :: [] ->
      let%bind map = decompile_expression record in
      let%bind e = decompile_expression e in
      let arg = e,[map] in
      return_expr @@ CST.ECall( wrap (CST.EVar (wrap "Map.find_opt"), arg))
    | Access_map e :: lst ->
      let path = List.rev lst in
      let%bind field_path = bind list_to_nsepseq @@ bind_map_list decompile_to_selection path in
      let%bind struct_name = map (decompile_variable) @@ get_e_variable record in
      let proj : CST.projection = {struct_name;selector=rg;field_path} in
      let%bind e = decompile_expression e in
      let arg = e,[CST.EProj (wrap proj)] in
      return_expr @@ CST.ECall( wrap (CST.EVar (wrap "Map.find_opt"), arg))
    | _ ->
      let%bind field_path = bind list_to_nsepseq @@ bind_map_list decompile_to_selection path in
       let%bind struct_name = map (decompile_variable) @@ get_e_variable record in
      let proj : CST.projection = {struct_name;selector=rg;field_path} in
      return_expr @@ CST.EProj (wrap proj)
    )
  (* Update on multiple field of the same record. may be removed by adding sugar *)
  | E_update {record={expression_content=E_update _;_} as record;path;update} ->
    let%bind record = decompile_expression record in
    let%bind (record,updates) = match record with
      CST.EUpdate {value;_} -> ok @@ (value.record,value.updates)
    | _ -> failwith @@ Format.asprintf "Inpossible case %a" AST.PP.expression expr
    in
    let%bind var,path = match path with
      Access_record var::path -> ok @@ (var,path)
    | _ -> failwith "Impossible case %a"
    in
    let%bind field_path = decompile_to_path (Location.wrap @@ Var.of_name var) path in
    let%bind field_expr = decompile_expression update in
    let field_assign : CST.field_path_assignment = {field_path;assignment=rg;field_expr} in
    let updates = updates.value.ne_elements in
    let updates = wrap @@ ne_inject braces @@ npseq_cons (wrap @@ field_assign) updates in
    let update : CST.update = {lbrace=rg;record;kwd_with=rg;updates;rbrace=rg} in
    return_expr @@ CST.EUpdate (wrap @@ update)
  | E_update {record; path; update} ->
    let%bind record = map (decompile_variable) @@ get_e_variable record in
    let%bind field_expr = decompile_expression update in
    let (struct_name,field_path) = List.Ne.of_list path in
    (match field_path with
      [] ->
      (match struct_name with
        Access_record name ->
        let record : CST.path = Name record in
        let field_path = CST.Name (wrap name) in
        let update : CST.field_path_assignment = {field_path;assignment=rg;field_expr} in
        let updates = wrap @@ ne_inject braces @@ (wrap update,[]) in
        let update : CST.update = {lbrace=rg;record;kwd_with=rg;updates;rbrace=rg} in
        return_expr @@ CST.EUpdate (wrap update)
      | Access_tuple i ->
        let record : CST.path = Name record in
        let field_path = CST.Name (wrap @@ Z.to_string i) in
        let update : CST.field_path_assignment = {field_path;assignment=rg;field_expr} in
        let updates = wrap @@ ne_inject braces @@ (wrap update,[]) in
        let update : CST.update = {lbrace=rg;record;kwd_with=rg;updates;rbrace=rg} in
        return_expr @@ CST.EUpdate (wrap update)
      | Access_map e ->
        let%bind e = decompile_expression e in
        let arg = field_expr,[e; CST.EVar record] in
        return_expr @@ CST.ECall (wrap (CST.EVar (wrap "Map.add"), arg))
      )
    | _ ->
      let%bind struct_name = match struct_name with
          Access_record name -> ok @@ wrap name
        | Access_tuple i -> ok @@ wrap @@ Z.to_string i
        | Access_map _ -> failwith @@ Format.asprintf "invalid map update %a" AST.PP.expression expr
      in
      (match List.rev field_path with
        Access_map e :: lst ->
        let field_path = List.rev lst in
        let%bind field_path = bind_map_list decompile_to_selection field_path in
        let%bind field_path = list_to_nsepseq field_path in
        let field_path : CST.projection = {struct_name; selector=rg;field_path} in
        let field_path = CST.EProj (wrap @@ field_path) in
        let%bind e = decompile_expression e in
        let arg = field_expr, [e; field_path] in
        return_expr @@ CST.ECall (wrap (CST.EVar (wrap "Map.add"),arg))
      | _ ->
        let%bind field_path = bind_map_list decompile_to_selection field_path in
        let%bind field_path = list_to_nsepseq field_path in
        let field_path : CST.projection = {struct_name; selector=rg;field_path} in
        let field_path = CST.Path (wrap @@ field_path) in
        let record : CST.path = Name record in
        let update : CST.field_path_assignment = {field_path;assignment=rg;field_expr} in
        let updates = wrap @@ ne_inject braces @@ (wrap update,[]) in
        let update : CST.update = {lbrace=rg;record;kwd_with=rg;updates;rbrace=rg} in
        return_expr @@ CST.EUpdate (wrap update)
      )
    )
  | E_ascription {anno_expr;type_annotation} ->
    let%bind expr = decompile_expression anno_expr in
    let%bind ty   = decompile_type_expr type_annotation in
    return_expr @@ CST.EAnnot (wrap @@ par (expr,rg,ty))
  | E_cond {condition;then_clause;else_clause} ->
    let%bind test  = decompile_expression condition in
    let%bind ifso  = decompile_expression then_clause in
    let%bind ifnot = decompile_expression else_clause in
    let ifnot = Some(rg,ifnot) in
    let cond : CST.cond_expr = {kwd_if=rg;test;kwd_then=rg;ifso;ifnot} in
    return_expr @@ CST.ECond (wrap cond)
  | E_sequence {expr1;expr2} ->
    let%bind expr1 = decompile_expression expr1 in
    let%bind expr2 = decompile_expression expr2 in
    return_expr @@ CST.ESeq (wrap @@ inject beginEnd @@ list_to_sepseq [expr1; expr2])
  | E_tuple tuple ->
    let%bind tuple = bind_map_list decompile_expression tuple in
    let%bind tuple = list_to_nsepseq tuple in
    return_expr @@ CST.ETuple (wrap @@ tuple)
  | E_map map ->
    let%bind map = bind_map_list (bind_map_pair decompile_expression) map in
    let aux (k,v) = CST.ETuple (wrap (k,[(rg,v)])) in
    let map = List.map aux map in
    (match map with
      [] -> return_expr @@ CST.EVar (wrap "Big_map.empty")
    | _  ->
      let var = CST.EVar (wrap "Map.literal") in
      return_expr @@ CST.ECall (wrap @@ (var, List.Ne.of_list @@ map))
    )
  | E_big_map big_map ->
    let%bind big_map = bind_map_list (bind_map_pair decompile_expression) big_map in
    let aux (k,v) = CST.ETuple (wrap (k,[(rg,v)])) in
    let big_map = List.map aux big_map in
    (match big_map with
      [] -> return_expr @@ CST.EVar (wrap "Big_map.empty")
    | _  ->
      let var = CST.EVar (wrap "Big_map.literal") in
      return_expr @@ CST.ECall (wrap @@ (var, List.Ne.of_list @@ big_map))
    )
  | E_list lst ->
    let%bind lst = bind_map_list decompile_expression lst in
    let lst = list_to_sepseq lst in
    return_expr @@ CST.EList (EListComp (wrap @@ inject brackets @@ lst))
  | E_set set ->
    let%bind set = bind_map_list decompile_expression set in
    let set = List.Ne.of_list @@ set in
    let var = CST.EVar (wrap "Set.literal") in
    return_expr @@ CST.ECall (wrap @@ (var,set))
    (* We should avoid to generate skip instruction*)
  | E_skip -> return_expr @@ CST.EUnit (wrap (rg,rg))
  | E_assign _
  | E_for _
  | E_for_each _
  | E_while _ ->
    failwith @@ Format.asprintf "Decompiling a imperative construct to CameLIGO %a"
    AST.PP.expression expr

and decompile_to_path : AST.expression_variable -> AST.access list -> (CST.path, _) result = fun var access ->
  let struct_name = decompile_variable var.wrap_content in
  match access with
    [] -> ok @@ CST.Name struct_name
  | lst ->
    let%bind field_path = bind list_to_nsepseq @@ bind_map_list decompile_to_selection lst in
    let path : CST.projection = {struct_name;selector=rg;field_path} in
    ok @@ (CST.Path (wrap @@ path) : CST.path)

and decompile_to_selection : AST.access -> (CST.selection, _) result = fun access ->
  match access with
    Access_tuple index -> ok @@ CST.Component (wrap @@ ("",index))
  | Access_record str  -> ok @@ CST.FieldName (wrap str)
  | Access_map _ ->
    failwith @@ Format.asprintf
    "Can't decompile access_map to selection"

and decompile_lambda : AST.lambda -> _ = fun {binder;input_type;output_type;result} ->
    let%bind param_decl = pattern_type binder.wrap_content input_type in
    let param = (param_decl, []) in
    let%bind ret_type = bind_map_option (bind_compose (ok <@ prefix_colon) decompile_type_expr) output_type in
    let%bind return = decompile_expression result in
    ok @@ (param,ret_type,None,return)

and decompile_attributes = function
    true -> [wrap "inline"]
  | false -> []

and decompile_matching_cases : AST.matching_expr -> ((CST.expr CST.case_clause Region.reg, Region.t) Simple_utils.Utils.nsepseq Region.reg,_) result =
fun m ->
  let%bind cases = match m with
    Match_variable (var, ty_opt, expr) ->
    let%bind pattern = pattern_type var.wrap_content ty_opt in
    let%bind rhs = decompile_expression expr in
    let case : _ CST.case_clause = {pattern; arrow=rg; rhs}in
    ok @@ [wrap case]
  | Match_tuple (lst, ty_opt, expr) ->
    let%bind tuple = match ty_opt with
      Some ty_lst ->
      let aux (var, ty) =
        let pattern = CST.PVar (decompile_variable var) in
        let%bind type_expr = decompile_type_expr ty in
        ok @@ CST.PTyped (wrap @@ CST.{pattern;colon=rg;type_expr})
      in
      bind list_to_nsepseq @@ bind_map_list aux @@ List.combine (List.map (fun (e:AST.expression_variable) -> e.wrap_content) lst) ty_lst
    | None ->
      let aux (var:AST.expression_variable) = CST.PVar (decompile_variable var.wrap_content) in
      list_to_nsepseq @@ List.map aux lst
    in
    let pattern : CST.pattern = PTuple (wrap @@ tuple) in
    let%bind rhs = decompile_expression expr in
    let case : _ CST.case_clause = {pattern; arrow=rg; rhs}in
    ok @@ [wrap case]
  | Match_record _ -> failwith "match_record not availiable yet"
  | Match_option {match_none;match_some}->
    let%bind rhs = decompile_expression match_none in
    let none_case : _ CST.case_clause = {pattern=PConstr (PNone rg);arrow=rg; rhs} in
    let%bind rhs = decompile_expression @@ snd match_some in
    let var = CST.PVar (decompile_variable @@ (fst match_some).wrap_content)in
    let some_case : _ CST.case_clause = {pattern=PConstr (PSomeApp (wrap (rg,var)));arrow=rg; rhs} in
    ok @@ [wrap some_case;wrap none_case]
  | Match_list {match_nil; match_cons} ->
    let (hd,tl,expr) = match_cons in
    let hd = CST.PVar (decompile_variable hd.wrap_content) in
    let tl = CST.PVar (decompile_variable tl.wrap_content) in
    let cons = (hd,rg,tl) in
    let%bind rhs = decompile_expression @@ expr in
    let cons_case : _ CST.case_clause = {pattern=PList (PCons (wrap cons));arrow=rg; rhs} in
    let%bind rhs = decompile_expression @@ match_nil in
    let nil_case : _ CST.case_clause = {pattern=PList (PListComp (wrap @@ inject brackets None));arrow=rg; rhs} in
    ok @@ [wrap cons_case; wrap nil_case]
  | Match_variant lst ->
    let aux ((c,(v:AST.expression_variable)),e) =
      let AST.Label c = c in
      let constr = wrap @@ c in
      let var : CST.pattern = PVar (decompile_variable v.wrap_content) in
      let tuple = var in
      let pattern : CST.pattern = PConstr (PConstrApp (wrap (constr, Some tuple))) in
      let%bind rhs = decompile_expression e in
      let case : _ CST.case_clause = {pattern;arrow=rg;rhs} in
      ok @@ wrap case
    in
    bind_map_list aux lst
  in
  map wrap @@ list_to_nsepseq cases
let decompile_declaration : AST.declaration Location.wrap -> (CST.declaration, _) result = fun decl ->
  let decl = Location.unwrap decl in
  let wrap value = ({value;region=Region.ghost} : _ Region.reg) in
  match decl with
    Declaration_type (name, te) ->
    let name = decompile_variable name in
    let%bind type_expr = decompile_type_expr te in
    ok @@ CST.TypeDecl (wrap (CST.{kwd_type=rg; name; eq=rg; type_expr}))
  | Declaration_constant (var, ty_opt, inline, expr) ->
    let attributes : CST.attributes = decompile_attributes inline in
    let var = CST.PVar (decompile_variable var.wrap_content) in
    let binders = (var,[]) in
    match expr.expression_content with
      E_lambda lambda ->
      let%bind lhs_type = bind_map_option (bind_compose (ok <@ prefix_colon) decompile_type_expr) ty_opt in
      let%bind let_rhs = decompile_expression @@ AST.make_e @@ AST.E_lambda lambda in
      let let_binding : CST.let_binding = {binders;lhs_type;eq=rg;let_rhs} in
      let let_decl : CST.let_decl = wrap (rg,None,let_binding,attributes) in
      ok @@ CST.Let let_decl
    | E_recursive {lambda; _} ->
      let%bind lhs_type = bind_map_option (bind_compose (ok <@ prefix_colon) decompile_type_expr) ty_opt in
      let%bind let_rhs = decompile_expression @@ AST.make_e @@ AST.E_lambda lambda in
      let let_binding : CST.let_binding = {binders;lhs_type;eq=rg;let_rhs} in
      let let_decl : CST.let_decl = wrap (rg,Some rg,let_binding,attributes) in
      ok @@ CST.Let (let_decl)
    | _ ->
      let%bind lhs_type = bind_map_option (bind_compose (ok <@ prefix_colon) decompile_type_expr) ty_opt in
      let%bind let_rhs = decompile_expression expr in
      let let_binding : CST.let_binding = {binders;lhs_type;eq=rg;let_rhs} in
      let let_decl : CST.let_decl = wrap (rg,None,let_binding,attributes) in
      ok @@ CST.Let let_decl

let decompile_program : AST.program -> (CST.ast, _) result = fun prg ->
  let%bind decl = bind_map_list decompile_declaration prg in
  let decl = List.Ne.of_list decl in
  ok @@ ({decl;eof=rg}: CST.ast)
