module AST = Ast_imperative
module CST = Cst.Cameligo
module Token = Lexing_cameligo.Token
module Predefined = Predefined.Tree_abstraction
module Shared_helpers = Tree_abstraction_shared.Helpers

open Simple_utils.Function
module Region   = Simple_utils.Region
module Utils    = Simple_utils.Utils
module List     = Simple_utils.List
module Location = Simple_utils.Location
module Pair     = Simple_utils.Pair

open Ligo_prim

(* Utils *)

let wrap = Region.wrap_ghost

let list_to_sepseq ~sep lst =
  match lst with
    [] -> None
  |  hd :: lst ->
      let aux e = (sep, e) in
      Some (hd, List.map ~f:aux lst)

let list_to_nsepseq ~sep lst =
  match list_to_sepseq ~sep lst with
    Some s -> s
  | None   -> failwith "List is empty"

let nelist_to_npseq ~sep (hd, lst) = (hd, List.map ~f:(fun e -> (sep, e)) lst)

let npseq_cons ~sep hd lst = hd,(sep, fst lst)::(snd lst)

let par a = CST.{lpar=Token.ghost_lpar;inside=a;rpar=Token.ghost_rpar}

let type_vars_of_list : string Region.reg list -> CST.type_vars = fun lst ->
  let type_var_of_name : string Region.reg -> CST.type_var Region.reg = fun name -> wrap CST.{quote=Token.ghost_quote;name} in
  match lst with
  | [name] -> QParam (type_var_of_name name)
  | x ->
    let x = Utils.nsepseq_map type_var_of_name (list_to_nsepseq ~sep:(Token.ghost_comma) x) in
    QParamTuple (wrap (par x))

let inject compound a = CST.{compound;elements=a;terminator=None}

let ne_inject compound fields ~attr = CST.{
  compound;
  ne_elements=fields;
  terminator=None;
  attributes=attr
  }

let prefix_colon a = (Token.ghost_colon, a)

let braces   = Some (CST.Braces (Token.ghost_lbrace,Token.ghost_rbrace))
let brackets = Some (CST.Brackets (Token.ghost_lbracket,Token.ghost_rbracket))
let beginEnd = Some (CST.BeginEnd (Token.ghost_begin,Token.ghost_end))

(* Decompiler *)

module type X_var = sig
  type t
  val pp : Format.formatter -> t -> unit
end
let decompile_variable_abs (type a) (module X:X_var with type t = a): a -> CST.variable = fun var ->
  let var = Format.asprintf "%a" X.pp var in
  if String.contains var '#' then
    let var = String.split ~on:'#' var in
    wrap @@ "gen__" ^ (String.concat var)
  else
    if String.length var > 4 && String.equal "gen__" @@ String.sub var ~pos:0 ~len:5 then
      wrap @@ "user__" ^ var
    else
      wrap @@ var

let decompile_variable = decompile_variable_abs (module ValueVar)
let decompile_type_var = decompile_variable_abs (module TypeVar)
let decompile_mod_var  = decompile_variable_abs (module ModuleVar)

let rec decompile_type_expr : AST.type_expression -> CST.type_expr = fun te ->
  let return te = te in
  match te.type_content with
    T_sum { attributes ; fields } ->
    let attributes = Shared_helpers.decompile_attributes attributes in
    let aux (Label.Label c, Rows.{associated_type; attributes=row_attr; _}) =
      let constr = wrap c in
      let arg = decompile_type_expr associated_type in
      let arg = Some (Token.ghost_of, arg) in
      let row_attr = Shared_helpers.decompile_attributes row_attr in
      let variant : CST.variant = {constr; arg; attributes=row_attr} in
      wrap variant in
    let variants = List.map ~f:aux fields in
    let variants = list_to_nsepseq ~sep:Token.ghost_vbar variants in
    let lead_vbar = Some (Token.ghost_vbar) in
    let sum : CST.sum_type = {lead_vbar; variants; attributes} in
    return @@ CST.TSum (wrap sum)
  | T_record {fields; attributes} ->
     let aux (Label.Label c, Rows.{associated_type; attributes=field_attr; _}) =
      let field_name = wrap c in
      let field_type = decompile_type_expr associated_type in
      let field_attr = Shared_helpers.decompile_attributes field_attr in
      let field : CST.field_decl =
        {field_name; colon = Token.ghost_colon ; field_type; attributes=field_attr} in
      wrap field in
    let record = List.map ~f:aux fields in
    let record = list_to_nsepseq ~sep:Token.ghost_semi record in
    let attributes = Shared_helpers.decompile_attributes attributes in
    return @@ CST.TRecord (wrap @@ ne_inject braces record ~attr:attributes)
  | T_tuple tuple ->
    let tuple = List.map ~f:decompile_type_expr tuple in
    let tuple = list_to_nsepseq ~sep:Token.ghost_times @@ tuple in
    return @@ CST.TProd (wrap tuple)
  | T_arrow {type1;type2} ->
    let type1 = decompile_type_expr type1 in
    let type2 = decompile_type_expr type2 in
    let arrow = (type1, Token.ghost_arrow, type2) in
    return @@ CST.TFun (wrap arrow)
  | T_variable variable ->
    let var = decompile_type_var variable in
    return @@ CST.TVar var
  | T_app {type_operator; arguments} ->
    let type_constant = decompile_type_var type_operator in
    let arguments = List.map ~f:decompile_type_expr arguments in
    let arguments = list_to_nsepseq ~sep:Token.ghost_comma arguments in
    let par : _ CST.par = par arguments in
    let lst : CST.type_constr_arg = CST.CArgTuple (wrap par) in
    return @@ CST.TApp (wrap (type_constant,lst))
  | T_annoted _annot ->
    failwith "let's work on it later"
  | T_module_accessor {module_path;element} -> (
    let rec aux : ModuleVar.t list -> (CST.type_expr -> CST.type_expr) -> CST.type_expr = fun lst f_acc ->
      match lst with
      | module_name::tl ->
        let module_name = decompile_mod_var module_name in
        let f = fun field ->
          f_acc (CST.TModA (wrap CST.{module_name;selector=Token.ghost_dot;field}))
        in
        aux tl f
      | [] ->
        let element = CST.TVar (decompile_type_var element) in
        f_acc element
    in
    return @@ (aux module_path (fun x -> x))
  )
  | T_singleton x -> (
    match x with
    | Literal_int i ->
      let z : CST.type_expr = CST.TInt { region = Region.ghost ; value = (Z.to_string i, i) } in
      return z
    | _ -> failwith "unsupported singleton"
  )
  | T_abstraction x -> decompile_type_expr x.type_
  | T_for_all x -> decompile_type_expr x.type_

let get_e_variable : AST.expression -> _ = fun expr ->
  match expr.expression_content with
    E_variable var -> var
  | _ -> failwith @@
    Format.asprintf "%a should be a variable expression"
    AST.PP.expression expr

let get_e_tuple : AST.expression -> _  = fun expr ->
  match expr.expression_content with
    E_tuple tuple -> (tuple, false)
  | E_variable _
  | E_literal _
  | E_constant _
  | E_lambda _ -> ([expr], false)
  | E_type_abstraction _ -> ([expr], false)
  | E_application _ -> ([expr], true)
  | E_accessor _
  | E_module_accessor _ -> ([expr], false)
  | _ -> failwith @@
    Format.asprintf "%a should be a tuple expression"
    AST.PP.expression expr

let pattern_type ({var;ascr;attributes}: _ Binder.t) =
  let attributes = attributes |> Tree_abstraction_shared.Helpers.strings_of_binder_attributes `CameLIGO |> Shared_helpers.decompile_attributes in
  let var : CST.var_pattern = {variable = decompile_variable var; attributes = attributes} in
  let pattern : CST.pattern = CST.PVar (wrap var) in
  match ascr with
    Some s ->
      let type_expr = decompile_type_expr s in
      CST.PTyped (wrap @@ CST.{pattern;colon=Token.ghost_colon;type_expr})
  | None -> pattern

let decompile_type_params : AST.type_expression -> _ option * CST.type_expr = fun type_expr ->
  let q, t = AST.Combinators.destruct_for_alls type_expr in
  let t = decompile_type_expr t in
  match q with
  | [] -> None, t
  | _ -> let type_vars = List.rev @@ List.map q ~f:decompile_type_var in
         let type_vars = List.Ne.of_list type_vars in
         let inside : CST.type_params = {kwd_type=Token.ghost_type;type_vars} in
         let q = wrap (par inside) in
         Some q, t

let decompile_operator : Constant.rich_constant -> CST.expr List.Ne.t -> CST.expr option = fun cons_name arguments ->
  match cons_name, arguments with
  | Const C_ADD, (arg1, [arg2])
  | Const C_POLYMORPHIC_ADD, (arg1, [arg2]) ->
     Some CST.(EArith (Add (wrap { op = Token.ghost_plus ; arg1 ; arg2 })))
  | Const C_SUB, (arg1, [arg2])
  | Const C_POLYMORPHIC_SUB, (arg1, [arg2]) ->
     Some CST.(EArith (Sub (wrap { op = Token.ghost_minus ; arg1 ; arg2 })))
  | Const C_MUL, (arg1, [arg2]) ->
     Some CST.(EArith (Mult (wrap { op = Token.ghost_times ; arg1 ; arg2 })))
  | Const C_DIV, (arg1, [arg2]) ->
     Some CST.(EArith (Div (wrap { op = Token.ghost_slash ; arg1 ; arg2 })))
  | Const C_MOD, (arg1, [arg2]) ->
     Some CST.(EArith (Mod (wrap { op = Token.ghost_mod ; arg1 ; arg2 })))
  | Const C_NEG, (arg, []) ->
     Some CST.(EArith (Neg (wrap { op = Token.ghost_minus ; arg })))
  | Const C_LT, (arg1, [arg2]) ->
     Some CST.(ELogic (CompExpr (Lt (wrap { op = Token.ghost_lt ; arg1 ; arg2 }))))
  | Const C_LE, (arg1, [arg2]) ->
     Some CST.(ELogic (CompExpr (Leq (wrap { op = Token.ghost_le ; arg1 ; arg2 }))))
  | Const C_GT, (arg1, [arg2]) ->
     Some CST.(ELogic (CompExpr (Gt (wrap { op = Token.ghost_gt ; arg1 ; arg2 }))))
  | Const C_GE, (arg1, [arg2]) ->
     Some CST.(ELogic (CompExpr (Geq (wrap { op = Token.ghost_ge ; arg1 ; arg2 }))))
  | Const C_EQ, (arg1, [arg2]) ->
     Some CST.(ELogic (CompExpr (Equal (wrap { op = Token.ghost_eq ; arg1 ; arg2 }))))
  | Const C_NEQ, (arg1, [arg2]) ->
     Some CST.(ELogic (CompExpr (Neq (wrap { op = Token.ghost_ne ; arg1 ; arg2 }))))
  | _ -> None

let rec decompile_expression : AST.expression -> CST.expr = fun expr ->
  let return_expr expr = expr in
  let return_expr_with_par expr = return_expr @@ CST.EPar (wrap @@ par @@ expr) in
  match expr.expression_content with
    E_variable name ->
    let var = decompile_variable name in
    return_expr @@ CST.EVar (var)
  | E_constant {cons_name; arguments} ->
    (match arguments with
      [] -> let expr = CST.EVar (wrap @@ Predefined.constant_to_string cons_name) in
            return_expr @@ expr
    | _ ->
      let arguments = List.Ne.of_list @@
        List.map ~f:decompile_expression arguments in
      match decompile_operator cons_name arguments with
      | None ->
         let arguments = List.Ne.map (fun x -> CST.EPar (wrap @@ par @@ x)) arguments in
         let expr = CST.EVar (wrap @@ Predefined.constant_to_string cons_name) in
         let const = wrap (expr, arguments) in
         return_expr_with_par @@ CST.ECall const
      | Some expr ->
         return_expr_with_par @@ expr
    )
  | E_literal literal ->
    (match literal with
        Literal_unit  ->  return_expr @@ CST.EUnit (wrap (Token.ghost_lpar,Token.ghost_rpar))
      | Literal_int i ->  return_expr @@ CST.EArith (Int (wrap ("",i)))
      | Literal_nat n ->  return_expr @@ CST.EArith (Nat (wrap ("",n)))
      | Literal_timestamp time ->
        let time = Tezos_utils.Time.Protocol.to_notation @@
          Tezos_utils.Time.Protocol.of_seconds @@ Z.to_int64 time in
          (* TODO combinators for CSTs. *)
        let ty = decompile_type_expr @@ AST.t_timestamp () in
        let time = CST.EString (String (wrap time)) in
        return_expr @@ CST.EAnnot (wrap @@ par (time, Token.ghost_colon, ty))
      | Literal_mutez mtez -> return_expr @@ CST.EArith (Mutez (wrap ("", Z.to_int64 mtez)))
      | Literal_string (Standard str) -> return_expr @@ CST.EString (String   (wrap str))
      | Literal_string (Verbatim ver) -> return_expr @@ CST.EString (Verbatim (wrap ver))
      | Literal_bytes b ->
        let b = Hex.of_bytes b in
        let s = Hex.to_string b in
        return_expr @@ CST.EBytes (wrap (s,b))
      | Literal_address addr ->
        let addr = CST.EString (String (wrap addr)) in
        let ty = decompile_type_expr @@ AST.t_address () in
        return_expr @@ CST.EAnnot (wrap @@ par (addr,Token.ghost_colon,ty))
      | Literal_signature sign ->
        let sign = CST.EString (String (wrap sign)) in
        let ty = decompile_type_expr @@ AST.t_signature () in
        return_expr @@ CST.EAnnot (wrap @@ par (sign,Token.ghost_colon,ty))
      | Literal_key k ->
        let k = CST.EString (String (wrap k)) in
        let ty = decompile_type_expr @@ AST.t_key () in
        return_expr @@ CST.EAnnot (wrap @@ par (k,Token.ghost_colon,ty))
      | Literal_key_hash kh ->
        let kh = CST.EString (String (wrap kh)) in
        let ty = decompile_type_expr @@ AST.t_key_hash () in
        return_expr @@ CST.EAnnot (wrap @@ par (kh,Token.ghost_colon,ty))
      | Literal_chain_id _
      | Literal_operation _ ->
        failwith "chain_id, operation are not created currently ?"
      | Literal_bls12_381_g1 b ->
        let b = Hex.of_bytes b in
        let s = Hex.to_string b in
        let b = CST.EBytes (wrap (s, b)) in
        let ty = decompile_type_expr @@ AST.t_bls12_381_g1 () in
        return_expr @@ CST.EAnnot (Region.wrap_ghost @@ par (b,Token.ghost_colon,ty))
      | Literal_bls12_381_g2 b ->
        let b = Hex.of_bytes b in
        let s = Hex.to_string b in
        let b = CST.EBytes (wrap (s, b)) in
        let ty = decompile_type_expr @@ AST.t_bls12_381_g2 () in
        return_expr @@ CST.EAnnot (Region.wrap_ghost @@ par (b,Token.ghost_colon,ty))
      | Literal_bls12_381_fr b ->
        let b = Hex.of_bytes b in
        let s = Hex.to_string b in
        let b = CST.EBytes (wrap (s, b)) in
        let ty = decompile_type_expr @@ AST.t_bls12_381_fr () in
        return_expr @@ CST.EAnnot (Region.wrap_ghost @@ par (b,Token.ghost_colon,ty))
      | Literal_chest _ | Literal_chest_key _ -> failwith "chest / chest_key not allowed in the syntax (only tests need this type)"
    )
  | E_application {lamb;args} ->
    let f (expr, b) = if b then CST.EPar (wrap @@ par @@ expr) else expr in
    let lamb = decompile_expression lamb in
    let args = List.Ne.of_list @@
      List.map ~f @@
        (fun (e, b) -> List.map ~f:(fun e ->
                                let de = decompile_expression e in
                                (de, b)) e) @@
      get_e_tuple args
    in
    return_expr @@ CST.ECall (wrap (lamb,args))
  | E_lambda lambda ->
    let (binders,_rhs_type,_block_with,body) = decompile_lambda lambda in
    let fun_expr : CST.fun_expr = {kwd_fun=Token.ghost_fun;binders;rhs_type=None;arrow=Token.ghost_arrow;body;type_params=None;attributes=[]} in
    return_expr_with_par @@ CST.EFun (wrap @@ fun_expr)
  | E_type_abstraction _ -> failwith "corner case : annonymous type abstraction"
  | E_recursive _ ->
    failwith "corner case : annonymous recursive function"
  | E_let_in {let_binder={var;ascr;attributes=var_attributes};rhs;let_result;attributes} ->
    let var_attributes = var_attributes |> Tree_abstraction_shared.Helpers.strings_of_binder_attributes `CameLIGO |> Shared_helpers.decompile_attributes in
    let var : CST.pattern = CST.PVar (wrap ({variable = decompile_variable @@ var; attributes = var_attributes } : CST.var_pattern)) in
    let binders = (var,[]) in
    let type_params, rhs_type = Option.value_map ascr ~default:(None, None)
                                           ~f:(fun t -> let type_params, rhs_type = decompile_type_params t in
                                                        type_params, Some (prefix_colon rhs_type)) in
    let let_rhs = decompile_expression rhs in
    let binding : CST.let_binding = {binders;type_params;rhs_type;eq=Token.ghost_eq;let_rhs} in
    let body = decompile_expression let_result in
    let attributes = Shared_helpers.decompile_attributes attributes in
    let lin : CST.let_in = {kwd_let=Token.ghost_let;kwd_rec=None;binding;kwd_in=Token.ghost_in;body;attributes} in
    return_expr @@ CST.ELetIn (wrap lin)
  | E_type_in {type_binder;rhs;let_result} ->
    let name = decompile_type_var type_binder in
    let type_expr = decompile_type_expr rhs in
    let type_decl : CST.type_decl = {kwd_type=Token.ghost_type;params=None;name;eq=Token.ghost_eq;type_expr} in
    let body = decompile_expression let_result in
    let tin : CST.type_in = {type_decl;kwd_in=Token.ghost_in;body} in
    return_expr @@ CST.ETypeIn (wrap tin)
  | E_mod_in {module_binder;rhs;let_result} -> (
    let name = decompile_mod_var module_binder in
    match rhs.wrap_content with
    | M_struct prg -> (
      let module_ = decompile_module prg in
      let mod_decl : CST.module_decl = {kwd_module=Token.ghost_module;name;eq=Token.ghost_eq;kwd_struct=Token.ghost_struct;module_;kwd_end=Token.ghost_end} in
      let body = decompile_expression let_result in
      let min : CST.mod_in = {mod_decl;kwd_in=Token.ghost_in;body} in
      return_expr @@ CST.EModIn (wrap min)
    )
    | M_variable v -> (
      let alias = name in
      let binders = decompile_mod_var v , [] in
      let mod_alias : CST.module_alias = {kwd_module=Token.ghost_module;alias;eq=Token.ghost_eq;binders} in
      let body = decompile_expression let_result in
      let mod_alias : CST.mod_alias = {mod_alias;kwd_in=Token.ghost_in;body} in
      return_expr @@ CST.EModAlias (wrap mod_alias)
    )
    | M_module_path path -> (
      let alias = name in
      let binders =
        nelist_to_npseq ~sep:Token.ghost_dot @@ List.Ne.map (fun x -> wrap (Format.asprintf "%a" ModuleVar.pp x)) path
      in
      let mod_alias : CST.module_alias = {kwd_module=Token.ghost_module;alias;eq=Token.ghost_eq;binders} in
      let body = decompile_expression let_result in
      let mod_alias : CST.mod_alias = {mod_alias;kwd_in=Token.ghost_in;body} in
      return_expr @@ CST.EModAlias (wrap mod_alias)
    )
  )
  | E_raw_code {language; code} ->
    let language = wrap @@ wrap @@ language in
    let code = decompile_expression code in
    let ci : CST.code_inj = {language;code;rbracket=Token.ghost_rbracket} in
    return_expr @@ CST.ECodeInj (wrap ci)
  | E_constructor {constructor;element} ->
    let Label constr = constructor in
    let constr = wrap constr in
    let element = decompile_expression element in
    return_expr_with_par @@ CST.EConstr (wrap (constr, Some element))
  | E_matching {matchee; cases} ->
    let expr  = decompile_expression matchee in
    let aux : _ Match_expr.match_case -> _ CST.case_clause CST.reg =
      fun { pattern ; body } ->
        let rhs = decompile_expression body in
        let pattern = decompile_pattern pattern in
        (wrap ({pattern ; arrow = Token.ghost_arrow ; rhs }:_ CST.case_clause))
    in
    let case_clauses = List.map ~f:aux cases in
    let cases = list_to_nsepseq ~sep:Token.ghost_vbar case_clauses in
    let cases = wrap cases in
    let cases : _ CST.case = {kwd_match=Token.ghost_match;expr;kwd_with=Token.ghost_with;lead_vbar=None;cases} in
    return_expr @@ CST.ECase (wrap cases)
  | E_record record ->
    let aux (Label.Label str, expr) =
      let field_name = wrap str in
      let field_expr = decompile_expression expr in
      let field : CST.field_assign = {field_name;assignment=Token.ghost_eq;field_expr} in
      wrap field
    in
    let record = List.map ~f:aux record in
    let record = list_to_nsepseq ~sep:Token.ghost_eq record in
    let record = ne_inject braces record ~attr:[] in
    (* why is the record not empty ? *)
    return_expr @@ CST.ERecord (wrap record)
  | E_accessor {record; path} ->
    let rec aux : AST.expression -> AST.expression Access_path.t -> AST.expression * AST.expression Access_path.t = fun e acc_path ->
      match e.expression_content with
      | E_accessor { record ; path } ->
        aux record (path @ acc_path)
      | _ -> e,acc_path
    in
    let (record,path) = aux record path in
    (match List.rev path with
      Access_map e :: [] ->
      let map = decompile_expression record in
      let e = decompile_expression e in
      let arg = e,[map] in
      return_expr @@ CST.ECall( wrap (CST.EVar (wrap "Map.find_opt"), arg))
    | Access_map e :: lst ->
      let path = List.rev lst in
      let field_path = list_to_nsepseq ~sep:Token.ghost_dot @@ List.map ~f:decompile_to_selection path in
      let struct_name = decompile_variable @@ get_e_variable record in
      let proj : CST.projection = {struct_name;selector=Token.ghost_dot;field_path} in
      let e = decompile_expression e in
      let arg = e,[CST.EProj (wrap proj)] in
      return_expr @@ CST.ECall( wrap (CST.EVar (wrap "Map.find_opt"), arg))
    | _ ->
      let field_path = list_to_nsepseq ~sep:Token.ghost_dot @@ List.map ~f:decompile_to_selection path in
       let struct_name = decompile_variable @@ get_e_variable record in
      let proj : CST.projection = {struct_name;selector=Token.ghost_dot;field_path} in
      return_expr @@ CST.EProj (wrap proj)
    )
  (* Update on multiple field of the same record. may be removed by adding sugar *)
  | E_update {record={expression_content=E_update _;_} as record;path;update} ->
    let record = decompile_expression record in
    let (record,updates) = match record with
      CST.EUpdate {value;_} -> (value.record,value.updates)
    | _ -> failwith @@ Format.asprintf "Inpossible case %a" AST.PP.expression expr
    in
    let var,path = match path with
      Access_record var::path -> (var,path)
    | _ -> failwith "Impossible case %a"
    in
    let field_path = decompile_to_path (ValueVar.of_input_var var) path in
    let field_expr = decompile_expression update in
    let field_assign : CST.field_path_assignment = {field_path;assignment=Token.ghost_eq;field_expr} in
    let updates = updates.value.ne_elements in
    let updates = wrap @@ ne_inject ~attr:[] braces @@ npseq_cons ~sep:Token.ghost_semi (wrap @@ field_assign) updates in
    let update : CST.update = {lbrace=Token.ghost_lbrace;record;kwd_with=Token.ghost_with;updates;rbrace=Token.ghost_rbrace} in
    return_expr @@ CST.EUpdate (wrap @@ update)
  | E_update {record; path; update} ->
    let rec aux : AST.expression -> AST.expression Access_path.t -> AST.expression * AST.expression Access_path.t = fun e acc_path ->
      match e.expression_content with
      | E_accessor { record ; path } ->
        aux record (path @ acc_path)
      | _ -> e,acc_path
    in
    let (record,path) = aux record path in
    let record = decompile_variable @@ get_e_variable record in
    let field_expr = decompile_expression update in
    let (struct_name,field_path) = List.Ne.of_list path in
    (match field_path with
      [] ->
      (match struct_name with
        Access_record name ->
        let record : CST.path = Name record in
        let field_path = CST.Name (wrap name) in
        let update : CST.field_path_assignment = {field_path;assignment=Token.ghost_eq;field_expr} in
        let updates = wrap @@ ne_inject ~attr:[] braces @@ (wrap update,[]) in
        let update : CST.update = {lbrace=Token.ghost_lbrace;record;kwd_with=Token.ghost_with;updates;rbrace=Token.ghost_rbrace} in
        return_expr @@ CST.EUpdate (wrap update)
      | Access_tuple i ->
        let record : CST.path = Name record in
        let field_path = CST.Name (wrap @@ Z.to_string i) in
        let update : CST.field_path_assignment = {field_path;assignment=Token.ghost_eq;field_expr} in
        let updates = wrap @@ ne_inject ~attr:[] braces @@ (wrap update,[]) in
        let update : CST.update = {lbrace=Token.ghost_lbrace;record;kwd_with=Token.ghost_with;updates;rbrace=Token.ghost_rbrace} in
        return_expr @@ CST.EUpdate (wrap update)
      | Access_map e ->
        let e = decompile_expression e in
        let arg = field_expr,[e; CST.EVar record] in
        return_expr @@ CST.ECall (wrap (CST.EVar (wrap "Map.add"), arg))
      )
    | _ ->
      let struct_name = match struct_name with
          Access_record name -> wrap name
        | Access_tuple i -> wrap @@ Z.to_string i
        | Access_map _ -> failwith @@ Format.asprintf "invalid map update %a" AST.PP.expression expr
      in
      (match List.rev field_path with
        Access_map e :: lst ->
        let field_path = List.rev lst in
        let field_path = List.map ~f:decompile_to_selection field_path in
        let field_path = list_to_nsepseq ~sep:Token.ghost_dot field_path in
        let field_path : CST.projection = {struct_name; selector=Token.ghost_dot;field_path} in
        let field_path = CST.EProj (wrap @@ field_path) in
        let e = decompile_expression e in
        let arg = field_expr, [e; field_path] in
        return_expr @@ CST.ECall (wrap (CST.EVar (wrap "Map.add"),arg))
      | _ ->
        let field_path = List.map ~f:decompile_to_selection field_path in
        let field_path = list_to_nsepseq ~sep:Token.ghost_dot field_path in
        let field_path : CST.projection = {struct_name; selector=Token.ghost_dot;field_path} in
        let field_path = CST.Path (wrap @@ field_path) in
        let record : CST.path = Name record in
        let update : CST.field_path_assignment = {field_path;assignment=Token.ghost_eq;field_expr} in
        let updates = wrap @@ ne_inject ~attr:[] braces @@ (wrap update,[]) in
        let update : CST.update = {lbrace=Token.ghost_lbrace;record;kwd_with=Token.ghost_with;updates;rbrace=Token.ghost_rbrace} in
        return_expr @@ CST.EUpdate (wrap update)
      )
    )
  | E_ascription {anno_expr;type_annotation} ->
    let expr = decompile_expression anno_expr in
    let ty   = decompile_type_expr type_annotation in
    return_expr @@ CST.EAnnot (wrap @@ par (expr,Token.ghost_colon,ty))
  | E_module_accessor {module_path;element} -> (
    let rec aux : ModuleVar.t list -> (CST.expr -> CST.expr) -> CST.expr = fun lst f_acc ->
      match lst with
      | module_name::tl ->
        let module_name = decompile_mod_var module_name in
        let f = fun field ->
          f_acc (CST.EModA (wrap CST.{module_name;selector=Token.ghost_dot;field}))
        in
        aux tl f
      | [] ->
        let element = CST.EVar (decompile_variable element) in
        f_acc element
    in
    return_expr @@ (aux module_path (fun x -> x))
  )
  | E_cond {condition;then_clause;else_clause} ->
    let test  = decompile_expression condition in
    let ifso  = decompile_expression then_clause in
    let ifnot = decompile_expression else_clause in
    let ifnot = Some(Token.ghost_else,ifnot) in
    let cond : CST.cond_expr = {kwd_if=Token.ghost_if;test;kwd_then=Token.ghost_then;ifso;ifnot} in
    return_expr @@ CST.ECond (wrap cond)
  | E_sequence {expr1;expr2} ->
    let expr1 = decompile_expression expr1 in
    let expr2 = decompile_expression expr2 in
    return_expr @@ CST.ESeq (wrap @@ inject beginEnd @@ list_to_sepseq ~sep:Token.ghost_semi [expr1; expr2])
  | E_tuple tuple ->
    let tuple = List.map ~f:decompile_expression tuple in
    let tuple = list_to_nsepseq ~sep:Token.ghost_comma tuple in
    return_expr @@ CST.ETuple (wrap @@ tuple)
  | E_map map ->
    let map = List.map ~f:(Pair.map ~f:decompile_expression) map in
    let aux (k,v) = CST.ETuple (wrap (k,[(Token.ghost_comma,v)])) in
    let map = List.map ~f:aux map in
    (match map with
      [] -> return_expr @@ CST.EVar (wrap "Big_map.empty")
    | _  ->
      let var = CST.EVar (wrap "Map.literal") in
      return_expr @@ CST.ECall (wrap @@ (var, List.Ne.of_list @@ map))
    )
  | E_big_map big_map ->
    let big_map = List.map ~f:(Pair.map ~f:decompile_expression) big_map in
    let aux (k,v) = CST.ETuple (wrap (k,[(Token.ghost_comma,v)])) in
    let big_map = List.map ~f:aux big_map in
    (match big_map with
      [] -> return_expr @@ CST.EVar (wrap "Big_map.empty")
    | _  ->
      let var = CST.EVar (wrap "Big_map.literal") in
      return_expr @@ CST.ECall (wrap @@ (var, List.Ne.of_list @@ big_map))
    )
  | E_list lst ->
    let lst = List.map ~f:decompile_expression lst in
    let lst = list_to_sepseq ~sep:Token.ghost_semi lst in
    return_expr @@ CST.EList (EListComp (wrap @@ inject brackets @@ lst))
  | E_set set ->
    let set = List.map ~f:decompile_expression set in
    let set = List.Ne.of_list @@ set in
    let var = CST.EVar (wrap "Set.literal") in
    return_expr @@ CST.ECall (wrap @@ (var,set))
    (* We should avoid to generate skip instruction*)
  | E_skip _ -> return_expr @@ CST.EUnit (wrap (Token.ghost_lpar,Token.ghost_rpar))
  | E_assign {binder={var;ascr;attributes};expression} ->
    let var_attributes = attributes |> Tree_abstraction_shared.Helpers.strings_of_binder_attributes `CameLIGO |> Shared_helpers.decompile_attributes in
    let binder : CST.pattern = CST.PVar (wrap ({variable = decompile_variable @@ var; attributes = var_attributes } : CST.var_pattern)) in
    let binders = (binder,[]) in
    let type_params, rhs_type = Option.value_map ascr ~default:(None, None)
                                           ~f:(fun t -> let type_params, lhs_type = decompile_type_params t in
                                                        type_params, Some (prefix_colon lhs_type)) in
    let let_rhs = decompile_expression expression in
    let binding : CST.let_binding = {binders;type_params;rhs_type;eq=Token.ghost_eq;let_rhs} in
    let body = decompile_expression (AST.e_unit ()) in
    let lin : CST.let_in = {kwd_let=Token.ghost_let;kwd_rec=None;binding;kwd_in=Token.ghost_in;body;attributes=[]} in
    return_expr @@ CST.ELetIn (wrap lin)
  | E_for _
  | E_for_each _
  | E_while _ ->
    failwith @@ Format.asprintf "Decompiling a imperative construct to CameLIGO %a"
    AST.PP.expression expr

and decompile_to_path : ValueVar.t -> _ Access_path.t -> CST.path = fun var access ->
  let struct_name = decompile_variable var in
  match access with
    [] -> CST.Name struct_name
  | lst ->
    let field_path = list_to_nsepseq ~sep:Token.ghost_dot @@ List.map ~f:decompile_to_selection lst in
    let path : CST.projection = {struct_name;selector=Token.ghost_dot;field_path} in
    (CST.Path (wrap @@ path) : CST.path)

and decompile_to_selection : _ Access_path.access -> CST.selection = fun access ->
  match access with
    Access_tuple index -> CST.Component (wrap @@ ("",index))
  | Access_record str  -> CST.FieldName (wrap str)
  | Access_map _ ->
    failwith @@ Format.asprintf
    "Can't decompile access_map to selection"

and decompile_lambda : (AST.expr,AST.ty_expr option) Lambda.t -> _ = fun {binder;output_type;result} ->
    let param_decl = pattern_type binder in
    let param = (param_decl, []) in
    let ret_type = Option.map ~f:(prefix_colon <@ decompile_type_expr) output_type in
    let result = decompile_expression result in
    (param,ret_type,None,result)

and decompile_declaration : AST.declaration -> CST.declaration = fun decl ->
  let decl = Location.unwrap decl in
  let wrap value = ({value;region=Region.ghost} : _ Region.reg) in
  match decl with
    Declaration_type {type_binder;type_expr; type_attr=_} -> (
    let name = decompile_type_var type_binder in
    let params =
      match type_expr.type_content with
      | T_abstraction _ -> (
        let rec aux : AST.type_expression -> _ list -> _ list  =
          fun t lst ->
            match t.type_content with
            | T_abstraction x -> aux x.type_ (x.ty_binder::lst)
            | _ -> lst
        in
        let vars = aux type_expr [] in
        let params = type_vars_of_list @@
          List.map ~f:decompile_type_var vars
        in
        Some params
      )
      | _ -> None
    in
    let type_expr = decompile_type_expr type_expr in
    CST.TypeDecl (wrap (CST.{kwd_type=Token.ghost_type;params;name; eq=Token.ghost_eq; type_expr}))
  )
  | Declaration_constant {binder;attr;expr}-> (
    let attributes : CST.attributes = Shared_helpers.decompile_attributes attr in
    let var_attributes = binder.attributes |> Tree_abstraction_shared.Helpers.strings_of_binder_attributes `CameLIGO |> Shared_helpers.decompile_attributes in
    let var = CST.PVar (wrap ({variable = decompile_variable binder.var; attributes = var_attributes } : CST.var_pattern)) in
    let binders = (var,[]) in
    let type_params, rhs_type = Option.value_map binder.ascr ~default:(None, None)
                                           ~f:(fun t -> let type_params, rhs_type = decompile_type_params t in
                                                        type_params, Some (prefix_colon rhs_type)) in
    match expr.expression_content with
      E_lambda lambda ->
      let let_rhs = decompile_expression (AST.make_e (AST.E_lambda lambda)) in
      let let_binding : CST.let_binding = {binders;type_params;rhs_type;eq=Token.ghost_eq;let_rhs} in
      let let_decl : CST.let_decl = (Token.ghost_let,None,let_binding,attributes) in
      CST.Let (wrap @@ let_decl)
    | E_recursive {lambda; _} ->
      let lambda = Lambda.map Fun.id Option.return lambda in
      let let_rhs = decompile_expression (AST.make_e (AST.E_lambda lambda)) in
      let let_binding : CST.let_binding = {binders;type_params;rhs_type;eq=Token.ghost_eq;let_rhs} in
      let let_decl : CST.let_decl = (Token.ghost_let,Some Token.ghost_rec,let_binding,attributes) in
      CST.Let (wrap @@ let_decl)
    | _ ->
      let let_rhs = decompile_expression expr in
      let let_binding : CST.let_binding = {binders;type_params;rhs_type;eq=Token.ghost_eq;let_rhs} in
      let let_decl : CST.let_decl = (Token.ghost_let,None,let_binding,attributes) in
      CST.Let (wrap @@ let_decl)
  )
  | Declaration_module {module_binder;module_;module_attr=_} -> (
    let name    = decompile_mod_var module_binder in
    match module_.wrap_content with
    | M_struct prg -> (
      let module_ = decompile_module prg in
      let module_decl :CST.module_decl = {
          kwd_module=Token.ghost_module; name; eq=Token.ghost_eq;
          kwd_struct=Token.ghost_struct; module_; kwd_end=Token.ghost_end
        }
      in
      CST.ModuleDecl (wrap @@ module_decl)
    )
    | M_variable v -> (
      let alias = name in
      let binders = decompile_mod_var v , [] in
      let mod_alias : CST.module_alias = {kwd_module=Token.ghost_module;alias;eq=Token.ghost_eq;binders} in
      CST.ModuleAlias (wrap mod_alias)
    )
    | M_module_path path -> (
      let alias = name in
      let binders =
        nelist_to_npseq ~sep:Token.ghost_dot @@ List.Ne.map (fun x -> wrap (Format.asprintf "%a" ModuleVar.pp x)) path
      in
      let mod_alias : CST.module_alias = {kwd_module=Token.ghost_module;alias;eq=Token.ghost_eq;binders} in
      CST.ModuleAlias (wrap mod_alias)
    )
  )

and decompile_pattern : AST.type_expression option Pattern.t -> CST.pattern =
  fun pattern ->
    let is_unit_pattern (p : AST.type_expression option Pattern.t) =
      match p.wrap_content with P_unit -> true | _ -> false in
    match pattern.wrap_content with
    | P_unit -> CST.PUnit (wrap (Token.ghost_lpar, Token.ghost_rpar))
    | P_var v ->
      let name = (decompile_variable v.var).value in
      let attributes = v.attributes |> Tree_abstraction_shared.Helpers.strings_of_binder_attributes `CameLIGO |> Shared_helpers.decompile_attributes in
      let pvar : CST.var_pattern = {variable = wrap name; attributes } in
      CST.PVar (wrap pvar)
    | P_list pl -> (
      let ret x = (CST.PList x) in
      match pl with
      | Cons (pa,pb) ->
        let pa = decompile_pattern pa in
        let pb = decompile_pattern pb in
        let cons = wrap (pa,Token.ghost_cons,pb) in
        ret (PCons cons)
      | List [] ->
        let nil = list_to_sepseq ~sep:Token.ghost_cons [] in
        let injection = wrap @@ inject (brackets) nil in
        ret (PListComp injection)
      | List plst ->
        let plst = List.map ~f:decompile_pattern plst in
        let plst = list_to_sepseq ~sep:Token.ghost_semi plst in
        let injection = wrap @@ inject (brackets) plst in
        ret (PListComp injection)
    )
    | P_variant (Label.Label constructor, p) ->
        let p = if is_unit_pattern p then None else Some (decompile_pattern p) in
        let constr = wrap (wrap constructor, p) in
        CST.PConstr constr
    | P_tuple lst ->
      let pl = List.map ~f:decompile_pattern lst in
      let pl = list_to_nsepseq ~sep:Token.ghost_comma pl in
      CST.PPar (wrap (par (CST.PTuple (wrap pl))))
    | P_record (llst,lst) ->
      let pl = List.map ~f:decompile_pattern lst in
      let fields_name = List.map ~f:(fun (Label x) -> wrap x) llst in
      let field_patterns =
        List.map
          ~f:(fun (field_name,pattern) -> wrap ({ field_name ; eq = Token.ghost_eq ; pattern }:CST.field_pattern))
          (List.zip_exn fields_name pl)
      in
      let field_patterns = list_to_nsepseq ~sep:Token.ghost_semi field_patterns in
      let inj = ne_inject braces field_patterns ~attr:[] in
      CST.PRecord (wrap inj)

and decompile_module : AST.module_ -> CST.ast = fun prg ->
  let decl = List.map ~f:(fun (Decl d) -> decompile_declaration d) prg in
  let decl = List.Ne.of_list decl in
  ({decl;eof=Token.ghost_eof}: CST.ast)

let decompile_program : AST.program -> CST.ast = fun prg ->
  let decl = List.map ~f:decompile_declaration prg in
  let decl = List.Ne.of_list decl in
  ({decl;eof=Token.ghost_eof}: CST.ast)
