module AST = Ast_imperative
module CST = Cst.Pascaligo
module Predefined = Predefined.Tree_abstraction.Pascaligo

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
  | None   -> failwith "List is not a non_empty list"
let nelist_to_npseq (hd, lst) = (hd, List.map (fun e -> (rg, e)) lst)
let npseq_cons hd lst = hd,(rg, fst lst)::(snd lst)

let par a = CST.{lpar=rg;inside=a;rpar=rg}
let braces a = CST.{lbrace=rg;inside=a;rbrace=rg}
let brackets a = CST.{lbracket=rg;inside=a;rbracket=rg}
let inject kind a = CST.{kind;enclosing=Brackets (rg,rg);elements=a;terminator=Some(rg)}
let ne_inject kind a = CST.{kind;enclosing=Brackets (rg,rg);ne_elements=a;terminator=Some(rg)}
let prefix_colon a = (rg, a)
let suffix_with a = (a, rg)
let to_block a = CST.{enclosing=Block (rg,rg,rg);statements=a;terminator=Some rg}
let empty_block = to_block (CST.Instr (CST.Skip rg),[])

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
    return @@ CST.TRecord (wrap @@ ne_inject (NEInjRecord rg) record)
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
  | T_wildcard->
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

let rec get_e_accessor : AST.expression -> _ result = fun expr ->
  match expr.expression_content with
    E_variable var -> ok @@ (var, [])
  | E_accessor {record;path} ->
    let%bind (var, lst) = get_e_accessor record in
    ok @@ (var, lst @ path)
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
type eos =
| Expression
| Statements

type state = Cst_pascaligo.ParserLog.state

let statements_of_expression : CST.expr -> CST.statement List.Ne.t option = fun stat ->
  match stat with
  | CST.ECall call -> Some (CST.Instr (CST.ProcCall call), [])
  | _ -> None

let rec decompile_expression : AST.expression -> _ result = fun e ->
  let%bind (block,expr) = decompile_to_block e in
  match expr with
    Some expr ->
    ( match block with
      Some block ->
        let block = wrap @@ block in
        ok @@ CST.EBlock (wrap @@ CST.{block;kwd_with=rg;expr})
    | None -> ok @@ expr
    )
  | None ->
    failwith @@ Format.asprintf
      "An expression was expected, but this was decompile to statements. \n
      Expr : %a
      Loc : %a"
      AST.PP.expression e
      Location.pp e.location

and decompile_statements : AST.expression -> _ result = fun expr ->
  let%bind (stat,_) = decompile_eos Statements expr in
  match stat with
    Some stat -> ok @@ stat
  | None ->
      failwith @@ Format.asprintf
        "Statements was expected, but this was decompile to expression. \n
        Expr : %a
        Loc : %a"
        AST.PP.expression expr
        Location.pp expr.location

and decompile_to_block : AST.expression -> _ result = fun expr ->
  let to_block a = CST.{enclosing=Block (rg,rg,rg);statements=a;terminator=Some rg} in
  let%bind (stats,next) = decompile_eos Expression expr in
  let block = Option.map (to_block <@ nelist_to_npseq) stats in
  ok @@ (block, next)

and decompile_to_tuple_expr : AST.expression list -> (CST.tuple_expr,_) result = fun expr ->
  let%bind tuple_expr = bind_map_list decompile_expression expr in
  let%bind tuple_expr = list_to_nsepseq tuple_expr in
  let tuple_expr : CST.tuple_expr = wrap @@ par @@  tuple_expr in
  ok @@ tuple_expr

and decompile_eos : eos -> AST.expression -> ((CST.statement List.Ne.t option)* (CST.expr option), _) result = fun output expr ->
  let return (a,b) = ok @@ (a,b) in
  let return_expr expr = return @@ (None, Some expr) in
  let return_expr_with_par expr = return_expr @@ CST.EPar (wrap @@ par @@ expr) in
  let return_stat stat = return @@ (Some stat, None) in
  let return_stat_ez stat = return_stat @@ (stat, []) in
  let return_inst inst = return_stat_ez @@ CST.Instr inst in
  match expr.expression_content with
    E_variable name ->
    let var = decompile_variable name.wrap_content in
    return_expr @@ CST.EVar (var)
  | E_constant {cons_name; arguments} ->
    let expr = CST.EVar (wrap @@ Predefined.constant_to_string cons_name) in
    (match arguments with
      [] -> return_expr @@ expr
    | _ ->
      let%bind arguments = decompile_to_tuple_expr arguments in
      let const : CST.fun_call = wrap (expr, arguments) in
      (match output with
        Expression -> return_expr (CST.ECall const)
      | Statements -> return_inst (CST.ProcCall const)
      )
    )
  | E_literal literal ->
    (match literal with
        Literal_unit  ->  return_expr @@ CST.EUnit rg
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
    let%bind args = bind decompile_to_tuple_expr @@ get_e_tuple args in
    (match output with
      Expression ->
      return_expr @@ CST.ECall (wrap (lamb,args))
    | Statements ->
      return_inst @@ CST.ProcCall (wrap (lamb,args))
    )
  | E_lambda lambda ->
    let%bind (param,ret_type,return) = decompile_lambda lambda in
    let fun_expr : CST.fun_expr = {kwd_function=rg;param;ret_type;kwd_is=rg;return} in
    return_expr_with_par @@ CST.EFun (wrap @@ fun_expr)
  | E_recursive _ ->
    failwith "corner case : annonymous recursive function"
  | E_let_in {let_binder;rhs={expression_content=E_update {record={expression_content=E_variable var;_};path;update};_};let_result;inline=_}
      when Var.equal (fst let_binder).wrap_content var.wrap_content ->
    let%bind lhs = (match List.rev path with
      Access_map e :: path ->
      let%bind path  = decompile_to_path var @@ List.rev path in
      let%bind index = map (wrap <@ brackets) @@ decompile_expression e in
      let mlu : CST.map_lookup = {path; index} in
      ok @@ CST.MapPath (wrap @@ mlu)
    | _ ->
      let%bind path  = decompile_to_path var @@ path in
      ok @@ (CST.Path (path) : CST.lhs)
    )
    in
    let%bind rhs = decompile_expression update in
    let assign : CST.assignment = {lhs;assign=rg;rhs} in
    let assign = CST.Instr (CST.Assign (wrap @@ assign)) in
    let%bind (stat,expr) = decompile_eos output let_result in
    let stat = (match stat with
      Some (stat) -> Some (List.Ne.cons assign stat)
    | None -> Some (assign,[])
    )
    in
    return @@ (stat,expr)
  | E_let_in {let_binder;rhs;let_result;inline} ->
    let%bind lin = decompile_to_data_decl let_binder rhs inline in
    let%bind (lst, expr) = decompile_eos Expression let_result in
    let lst = match lst with
      Some lst -> List.Ne.cons (CST.Data lin) lst
    | None -> (CST.Data lin, [])
    in
    return @@ (Some lst, expr)
  | E_raw_code {language; code} ->
    let language = wrap @@ wrap @@ language in
    let%bind code = decompile_expression code in
    let ci : CST.code_inj = {language;code;rbracket=rg} in
    return_expr @@ CST.ECodeInj (wrap ci)
  | E_constructor {constructor;element} ->
    let Label constr = constructor in
    let constr = wrap constr in
    let%bind element = bind decompile_to_tuple_expr @@ get_e_tuple element in
    return_expr_with_par @@ CST.EConstr (ConstrApp (wrap (constr, Some element)))
  | E_matching {matchee; cases} ->
    let%bind expr  = decompile_expression matchee in
    (match output with
      Expression ->
      let%bind cases = decompile_matching_expr decompile_expression cases in
      let cases : _ CST.case = {kwd_case=rg;expr;kwd_of=rg;enclosing=End rg;lead_vbar=None;cases} in
      return_expr @@ CST.ECase (wrap cases)
    | Statements ->
      let%bind cases = decompile_matching_expr decompile_if_clause cases in
      let cases : _ CST.case = {kwd_case=rg;expr;kwd_of=rg;enclosing=End rg;lead_vbar=None;cases} in
      return_inst @@ CST.CaseInstr (wrap cases)
    )
  | E_record record  ->
    let record = AST.LMap.to_kv_list record in
    let aux (AST.Label str, expr) =
      let field_name = wrap str in
      let%bind field_expr = decompile_expression expr in
      let field : CST.field_assignment = {field_name;assignment=rg;field_expr} in
      ok @@ wrap field
    in
    let%bind record = bind_map_list aux record in
    let%bind record = list_to_nsepseq record in
    let record = ne_inject (NEInjRecord rg) record in
    (* why is the record not empty ? *)
    return_expr @@ CST.ERecord (wrap record)
  | E_accessor {record; path} ->
    (match List.rev path with
      Access_map e :: [] ->
      let%bind (var,lst) = get_e_accessor @@ record in
      let%bind path = decompile_to_path var lst in
      let%bind e = decompile_expression e in
      let index = wrap @@ brackets @@ e in
      let mlu : CST.map_lookup = {path;index} in
      return_expr @@ CST.EMap(MapLookUp (wrap @@ mlu))
    | Access_map e :: lst ->
      let path = List.rev lst in
      let%bind field_path = bind list_to_nsepseq @@ bind_map_list decompile_to_selection path in
      let%bind struct_name = map (decompile_variable) @@ get_e_variable record in
      let proj : CST.projection = {struct_name;selector=rg;field_path} in
      let path : CST.path = CST.Path (wrap proj) in
      let%bind e = decompile_expression e in
      let index = wrap @@ brackets @@ e in
      let mlu : CST.map_lookup = {path;index} in
      return_expr @@ CST.EMap(MapLookUp (wrap @@ mlu))
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
    let updates = wrap @@ ne_inject (NEInjRecord rg) @@ npseq_cons (wrap @@ field_assign) updates in
    let update : CST.update = {record;kwd_with=rg;updates} in
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
        let updates = wrap @@ ne_inject (NEInjRecord rg) @@ (wrap update,[]) in
        let update : CST.update = {record;kwd_with=rg;updates;} in
        return_expr @@ CST.EUpdate (wrap update)
      | Access_tuple _ -> failwith @@ Format.asprintf "invalid tuple update %a" AST.PP.expression expr
      | Access_map e ->
        let%bind e = decompile_expression e in
        let arg : CST.tuple_expr = wrap @@ par @@ nelist_to_npseq (field_expr,[e; CST.EVar record]) in
        return_expr @@ CST.ECall (wrap (CST.EVar (wrap "Map.add"), arg))
      )
    | _ ->
      let%bind struct_name = match struct_name with
          Access_record name -> ok @@ wrap name
        | Access_tuple _ -> failwith @@ Format.asprintf "invalid tuple update %a" AST.PP.expression expr
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
        let arg = wrap @@ par @@ nelist_to_npseq (field_expr, [e; field_path]) in
        return_expr @@ CST.ECall (wrap (CST.EVar (wrap "Map.add"),arg))
      | _ ->
        let%bind field_path = bind_map_list decompile_to_selection field_path in
        let%bind field_path = list_to_nsepseq field_path in
        let field_path : CST.projection = {struct_name; selector=rg;field_path} in
        let field_path : CST.path = CST.Path (wrap @@ field_path) in
        let record : CST.path = Name record in
        let update : CST.field_path_assignment = {field_path;assignment=rg;field_expr} in
        let updates = wrap @@ ne_inject (NEInjRecord rg) @@ (wrap update,[]) in
        let update : CST.update = {record;kwd_with=rg;updates;} in
        return_expr @@ CST.EUpdate (wrap update)
      )
    )
  | E_ascription {anno_expr;type_annotation} ->
    let%bind expr = decompile_expression anno_expr in
    let%bind ty   = decompile_type_expr type_annotation in
    return_expr @@ CST.EAnnot (wrap @@ par (expr,rg,ty))
  | E_cond {condition;then_clause;else_clause} ->
    let%bind test  = decompile_expression condition in
    (match output with
      Expression ->
      let%bind ifso = decompile_expression then_clause in
      let%bind ifnot = decompile_expression else_clause in
      let cond : CST.cond_expr = {kwd_if=rg;test;kwd_then=rg;ifso;terminator=Some rg;kwd_else=rg;ifnot} in
      return_expr @@ CST.ECond (wrap cond)
    | Statements ->
      let%bind ifso  = decompile_if_clause then_clause in
      let%bind ifnot = decompile_if_clause else_clause in
      let cond : CST.conditional = {kwd_if=rg;test;kwd_then=rg;ifso;terminator=Some rg; kwd_else=rg;ifnot} in
      return_inst @@ CST.Cond (wrap cond)
    )
  | E_sequence {expr1;expr2} ->
    let%bind expr1 = decompile_statements expr1 in
    let%bind (expr2,next) = decompile_eos Statements expr2 in
    let expr1 = Option.unopt ~default:expr1 @@ Option.map (List.Ne.append expr1) expr2 in
    return @@ (Some expr1, next)
  | E_skip -> return_inst @@ CST.Skip rg
  | E_tuple tuple ->
    let%bind tuple = bind_map_list decompile_expression tuple in
    let%bind tuple = list_to_nsepseq tuple in
    return_expr @@ CST.ETuple (wrap @@ par tuple)
  | E_map map ->
    let%bind map = bind_map_list (bind_map_pair decompile_expression) map in
    let aux (k,v) =
      let binding : CST.binding = {source=k;arrow=rg;image=v} in
      wrap @@ binding
    in
    let map = list_to_sepseq @@ List.map aux map in
    return_expr @@ CST.EMap (MapInj (wrap @@ inject (InjMap rg) @@ map))
  | E_big_map big_map ->
    let%bind big_map = bind_map_list (bind_map_pair decompile_expression) big_map in
    let aux (k,v) =
      let binding : CST.binding = {source=k;arrow=rg;image=v} in
      wrap @@ binding
    in
    let big_map = list_to_sepseq @@ List.map aux big_map in
    return_expr @@ CST.EMap (BigMapInj (wrap @@ inject (InjBigMap rg) @@ big_map))
  | E_list lst ->
    let%bind lst = bind_map_list decompile_expression lst in
    let lst = list_to_sepseq lst in
    return_expr @@ CST.EList (EListComp (wrap @@ inject (InjList rg) @@ lst))
  | E_set set ->
    let%bind set = bind_map_list decompile_expression set in
    let set = list_to_sepseq set in
    return_expr @@ CST.ESet (SetInj (wrap @@ inject (InjSet rg) @@ set))
  | E_assign {variable;access_path;expression} ->
    let%bind lhs = decompile_to_lhs variable access_path in
    let%bind rhs = decompile_expression expression in
    let assign : CST.assignment = {lhs;assign=rg;rhs} in
    return_inst @@ Assign (wrap assign)
  | E_for {binder;start;final;increment;body} ->
    let binder     = decompile_variable binder.wrap_content in
    let%bind init  = decompile_expression start in
    let%bind bound = decompile_expression final in
    let%bind step  = decompile_expression increment in
    let step       = Some (rg, step) in
    let%bind (block,_next) = decompile_to_block body in
    let block = wrap @@ Option.unopt ~default:(empty_block) block in
    let fl : CST.for_int = {kwd_for=rg;binder;assign=rg;init;kwd_to=rg;bound;step;block} in
    return_inst @@ CST.Loop (For (ForInt (wrap fl)))
  | E_for_each {binder;collection;collection_type;body} ->
    let var = decompile_variable @@ (fst binder).wrap_content in
    let bind_to = Option.map (fun (x:AST.expression_variable) -> (rg,decompile_variable x.wrap_content)) @@ snd binder in
    let%bind expr = decompile_expression collection in
    let collection = match collection_type with
      Map -> CST.Map rg | Set -> Set rg | List -> List rg in
    let%bind (block,_next) = decompile_to_block body in
    let block = wrap @@ Option.unopt ~default:(empty_block) block in
    let fc : CST.for_collect = {kwd_for=rg;var;bind_to;kwd_in=rg;collection;expr;block} in
    return_inst @@ CST.Loop (For (ForCollect (wrap fc)))
  | E_while {condition;body} ->
    let%bind cond  = decompile_expression condition in
    let%bind (block,_next) = decompile_to_block body in
    let block = wrap @@ Option.unopt ~default:(empty_block) block in
    let loop : CST.while_loop = {kwd_while=rg;cond;block} in
    return_inst @@ CST.Loop (While (wrap loop))

and decompile_if_clause : AST.expression -> (CST.if_clause, _) result = fun e ->
  let%bind clause = decompile_statements e in
  match clause with
    CST.Instr instr,[] ->
    ok @@ CST.ClauseInstr instr
  | _ ->
    let clause = nelist_to_npseq clause, Some rg in
    ok @@ CST.ClauseBlock (ShortBlock (wrap @@ braces @@ clause))

and decompile_to_data_decl : (AST.expression_variable * AST.type_expression option) -> AST.expression -> bool -> (CST.data_decl, _) result = fun (name,ty_opt) expr inline ->
  let name = decompile_variable name.wrap_content in
  let%bind const_type = bind_map_option (bind_compose (ok <@ prefix_colon) decompile_type_expr) ty_opt in
  let attributes : CST.attr_decl option = match inline with
    true -> Some (wrap @@ ne_inject (NEInjAttr rg) @@ (wrap @@ "inline",[]))
  | false -> None
  in
  let fun_name = name in
  match expr.expression_content with
    E_lambda lambda ->
    let%bind (param,ret_type,return) = decompile_lambda lambda in
    let fun_decl : CST.fun_decl = {kwd_recursive=None;kwd_function=rg;fun_name;param;ret_type;kwd_is=rg;return;terminator=Some rg;attributes} in
    ok @@ CST.LocalFun (wrap fun_decl)
  | E_recursive {lambda; _} ->
    let%bind (param,ret_type,return) = decompile_lambda lambda in
    let fun_decl : CST.fun_decl = {kwd_recursive=Some rg;kwd_function=rg;fun_name;param;ret_type;kwd_is=rg;return;terminator=Some rg;attributes} in
    ok @@ CST.LocalFun (wrap fun_decl)
  | _ ->
    let%bind init = decompile_expression expr in
    let const_decl : CST.const_decl = {kwd_const=rg;name;const_type;equal=rg;init;terminator=Some rg; attributes} in
    let data_decl  : CST.data_decl  =  LocalConst (wrap const_decl) in
    ok @@ data_decl

and decompile_to_lhs : AST.expression_variable -> AST.access list -> (CST.lhs, _) result = fun var access ->
  match List.rev access with
    [] -> ok @@ (CST.Path (Name (decompile_variable var.wrap_content)) : CST.lhs)
  | hd :: tl ->
    match hd with
    | AST.Access_map e ->
      let%bind path = decompile_to_path var @@ List.rev tl in
      let%bind index = map (wrap <@ brackets) @@ decompile_expression e in
      let mlu: CST.map_lookup = {path;index} in
      ok @@ CST.MapPath (wrap @@ mlu)
    | _ ->
      let%bind path = decompile_to_path var @@ access in
      ok @@ (CST.Path (path) : CST.lhs)

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
    let var = decompile_variable binder.wrap_content in
    let%bind param_type = bind_map_option (bind_compose (ok <@ prefix_colon) decompile_type_expr) input_type in
    let param_const : CST.param_const = {kwd_const=rg;var;param_type} in
    let param_decl : CST.param_decl = ParamConst (wrap param_const) in
    let param = nelist_to_npseq (param_decl, []) in
    let param : CST.parameters = wrap @@ par param in
    let%bind ret_type = bind_map_option (bind_compose (ok <@ prefix_colon) decompile_type_expr) output_type in
    let%bind return = decompile_expression result in
    ok @@ (param,ret_type,return)

and decompile_matching_expr : type a.(AST.expr ->(a,_) result) -> AST.matching_expr -> ((a CST.case_clause Region.reg, Region.t) Simple_utils.Utils.nsepseq Region.reg,_) result =
fun f m ->
  let%bind cases = match m with
    Match_variable (var, _ty_opt, expr) ->
    let pattern : CST.pattern = PVar (decompile_variable var.wrap_content) in
    let%bind rhs = f expr in
    let case : _ CST.case_clause = {pattern; arrow=rg; rhs}in
    ok @@ [wrap case]
  | Match_tuple (lst, _ty_opt, expr) ->
    let aux (var:AST.expression_variable) = CST.PVar (decompile_variable var.wrap_content) in
    let%bind tuple = list_to_nsepseq @@ List.map aux lst in
    let pattern : CST.pattern = PTuple (wrap @@ par @@ tuple) in
    let%bind rhs = f expr in
    let case : _ CST.case_clause = {pattern; arrow=rg; rhs}in
    ok @@ [wrap case]
  | Match_record _ -> failwith "match_record not availiable yet"
  | Match_option {match_none;match_some}->
    let%bind rhs = f match_none in
    let none_case : _ CST.case_clause = {pattern=PConstr (PNone rg);arrow=rg; rhs} in
    let%bind rhs = f @@ snd match_some in
    let var = wrap @@ par @@ CST.PVar (decompile_variable @@ (fst match_some).wrap_content)in
    let some_case : _ CST.case_clause = {pattern=PConstr (PSomeApp (wrap (rg,var)));arrow=rg; rhs} in
    ok @@ [wrap some_case;wrap none_case]
  | Match_list {match_nil; match_cons} ->
    let (hd,tl,expr) = match_cons in
    let hd = CST.PVar (decompile_variable hd.wrap_content) in
    let tl = CST.PVar (decompile_variable tl.wrap_content) in
    let cons = (hd,[rg,tl]) in
    let%bind rhs = f @@ expr in
    let cons_case : _ CST.case_clause = {pattern=PList (PCons (wrap cons));arrow=rg; rhs} in
    let%bind rhs = f @@ match_nil in
    let nil_case : _ CST.case_clause = {pattern=PList (PNil rg);arrow=rg; rhs} in
    ok @@ [wrap cons_case; wrap nil_case]
  | Match_variant lst ->
    let aux ((c,(v:AST.expression_variable)),e) =
      let AST.Label c = c in
      let constr = wrap @@ c in
      let var : CST.pattern = PVar (decompile_variable v.wrap_content) in
      let tuple = wrap @@ par @@ (var,[]) in
      let pattern : CST.pattern = PConstr (PConstrApp (wrap (constr, Some tuple))) in
      let%bind rhs = f e in
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
    let kwd_type = Region.ghost
    and name = decompile_variable name
    and kwd_is = Region.ghost in
    let%bind type_expr = decompile_type_expr te in
    let terminator = Some Region.ghost in
    ok @@ CST.TypeDecl (wrap (CST.{kwd_type; name; kwd_is; type_expr; terminator}))
  | Declaration_constant (var, ty_opt, inline, expr) ->
    let attributes = match inline with
      true ->
        let attr = wrap "inline" in
        let ne_inj : _ CST.ne_injection =
          {kind=NEInjAttr rg;enclosing=End rg;ne_elements=(attr, []);terminator=Some rg} in
        let attr_decl = wrap ne_inj in
        Some attr_decl
    | false -> None
    in
    let name = decompile_variable var.wrap_content in
    let fun_name = name in
    match expr.expression_content with
      E_lambda lambda ->
      let%bind (param,ret_type,return) = decompile_lambda lambda in
      let fun_decl : CST.fun_decl = {kwd_recursive=None;kwd_function=rg;fun_name;param;ret_type;kwd_is=rg;return;terminator=Some rg;attributes} in
      ok @@ CST.FunDecl (wrap fun_decl)
    | E_recursive {lambda; _} ->
      let%bind (param,ret_type,return) = decompile_lambda lambda in
      let fun_decl : CST.fun_decl = {kwd_recursive=Some rg;kwd_function=rg;fun_name;param;ret_type;kwd_is=rg;return;terminator=Some rg;attributes} in
      ok @@ CST.FunDecl (wrap fun_decl)
    | _ ->
      let%bind const_type = bind_map_option (bind_compose (ok <@ prefix_colon) decompile_type_expr) ty_opt in
      let%bind init = decompile_expression expr in
      let const_decl : CST.const_decl = {kwd_const=rg;name;const_type;equal=rg;init;terminator=Some rg; attributes} in
      ok @@ CST.ConstDecl (wrap const_decl)

let decompile_program : AST.program -> (CST.ast, _) result = fun prg ->
  let%bind decl = bind_map_list decompile_declaration prg in
  let decl = List.Ne.of_list decl in
  ok @@ ({decl;eof=rg}: CST.ast)
