[@@@warning "-27"]
[@@@warning "-26"]
[@@@warning "-39"]

module AST = Ast_imperative
module CST = Cst.Jsligo
module Predefined = Predefined.Tree_abstraction
module Region     = Simple_utils.Region
module Location   = Simple_utils.Location
module List       = Simple_utils.List
module Pair       = Simple_utils.Pair
module Utils      = Simple_utils.Utils
module Token = CST.Token

open Simple_utils.Function
open Ligo_prim

(* Utils *)

let decompile_attributes : string list -> CST.attribute list = fun kvl ->
  let f : string -> CST.attribute = fun str -> Region.wrap_ghost str in
  List.map ~f kvl

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

let par a = CST.{lpar=Token.ghost_lpar;inside=a;rpar=Token.ghost_rpar}

let ne_inject compound fields ~attr = CST.{
  compound;
  ne_elements=fields;
  terminator=None;
  attributes=attr
  }

let prefix_colon a = (Token.ghost_colon, a)

let braces = Some (CST.Braces (Token.ghost_lbrace,Token.ghost_rbrace))

let chevrons x = CST.{lchevron=Token.ghost_lt;inside=x;rchevron=Token.ghost_gt}
let type_vars_of_list : string Region.reg list -> CST.type_vars = fun lst ->
  let lst = list_to_nsepseq ~sep:Token.ghost_comma lst in
  Region.wrap_ghost (chevrons lst)
let brackets x = CST.{lbracket=Token.ghost_lbracket;inside=x;rbracket=Token.ghost_rbracket}
let fun_type_arg x = CST.{ name = Region.wrap_ghost "_" ; colon = Token.ghost_colon ; type_expr = x }
let braced d = CST.{lbrace=Token.ghost_lbrace; rbrace=Token.ghost_rbrace; inside=d}

let filter_private (attributes: CST.attributes) =
  List.filter ~f:(fun v -> not (String.equal v.value "private")) attributes

(* Decompiler *)

module type X_var = sig
  type t
  val pp : Format.formatter -> t -> unit
end
let decompile_variable_abs (type a) (module X:X_var with type t = a): a -> CST.variable = fun var ->
  let var = Format.asprintf "%a" X.pp var in
  if String.contains var '#' then
    let var = String.split ~on:'#' var in
    Region.wrap_ghost @@ "gen__" ^ (String.concat var)
  else
    if String.length var > 4 && String.equal "gen__" @@ String.sub var ~pos:0 ~len:5 then
      Region.wrap_ghost @@ "user__" ^ var
    else
      Region.wrap_ghost @@ var

let decompile_variable = decompile_variable_abs (module Value_var)
let decompile_type_var = decompile_variable_abs (module Type_var)
let decompile_mod_var = decompile_variable_abs (module Module_var)
let decompile_variable2 : Value_var.t -> CST.var_pattern Region.reg = fun var ->
  let var = Format.asprintf "%a" Value_var.pp var in
  if String.contains var '#' then
    let var = String.split ~on:'#' var in
    Region.wrap_ghost @@ CST.{variable = Region.wrap_ghost ("gen__" ^ (String.concat var)); attributes = []}
  else
    if String.length var > 4 && String.equal "gen__" @@ String.sub var ~pos:0 ~len:5 then
      Region.wrap_ghost @@ CST.{variable = Region.wrap_ghost ("user__" ^ var); attributes = []}
    else
      Region.wrap_ghost @@ CST.{variable = Region.wrap_ghost var; attributes = []}

let rec decompile_type_expr : AST.type_expression -> CST.type_expr = fun te ->
  let return te = te in
  match te.type_content with
    T_sum { attributes ; fields } ->
    let aux (Label.Label c,Rows.{associated_type;attributes=row_attr; _}) =
      let constr = Region.wrap_ghost c in
      let arg = decompile_type_expr associated_type in
      let arg = (match arg with
        TProd {inside; _ } ->
          inside.value.inside
      | _ as p ->
        (p, [])
      )
      in
      let arg = Some (Token.ghost_comma, arg) in
      let row_attr = decompile_attributes row_attr in
      let variant_comp : CST.variant_comp = {constr; params = arg} in
      let tuple = Region.wrap_ghost @@ brackets variant_comp in
      let variant : CST.variant = {tuple; attributes=row_attr} in
      Region.wrap_ghost variant
    in
    let variants: (CST.variant Region.reg) list = List.map ~f:aux fields in
    let variants = list_to_nsepseq ~sep:Token.ghost_vbar variants in
    let variants = Region.wrap_ghost variants in
    let attributes = decompile_attributes attributes in
    let sum : CST.sum_type = { leading_vbar = (match attributes with [] -> None | _ -> Some Token.ghost_vbar); variants ; attributes} in
    return @@ CST.TSum (Region.wrap_ghost sum)
  | T_record {fields; attributes} ->
     let aux (Label.Label c, Rows.{associated_type; attributes; _}) =
       let field_name = Region.wrap_ghost c in
       let colon = Token.ghost_colon in
       let field_type: CST.type_expr = decompile_type_expr associated_type in
       let attributes = decompile_attributes attributes in
       let field : CST.field_decl =
         {field_name; colon; field_type; attributes} in
       Region.wrap_ghost field in
     let record = List.map ~f:aux fields in
     let record = list_to_nsepseq ~sep:Token.ghost_semi record in
     let attributes = List.map ~f:(fun el -> Region.wrap_ghost el) attributes in
     return @@ CST.TObject (Region.wrap_ghost @@ ne_inject braces record ~attr:attributes)
  | T_tuple tuple ->
    let tuple = List.map ~f:decompile_type_expr tuple in
    let tuple = list_to_nsepseq ~sep:Token.ghost_comma tuple in
    let tuple = brackets tuple in
    return @@ CST.TProd {inside = {value = tuple; region = Region.ghost}; attributes = []}
  | T_arrow {type1;type2} ->
    let type1 = decompile_type_expr type1 in
    let type_arg = fun_type_arg type1 in
    let type_args = par @@ nelist_to_npseq ~sep:Token.ghost_comma (type_arg , []) in
    let type2 = decompile_type_expr type2 in
    let arrow = (type_args, Token.ghost_arrow, type2) in
    return @@ CST.TFun (Region.wrap_ghost arrow)
  | T_variable variable ->
    let var = decompile_type_var variable in
    return @@ CST.TVar var
  | T_app {type_operator; arguments} ->
    let type_operator = decompile_type_var type_operator in
    let lst = List.map ~f:decompile_type_expr arguments in
    let lst = list_to_nsepseq ~sep:Token.ghost_comma lst in
    let lst = Region.wrap_ghost @@ chevrons lst in
    return @@ CST.TApp (Region.wrap_ghost (type_operator,lst))
  | T_annoted _annot ->
    failwith "let's work on it later"
  | T_module_accessor {module_path;element} -> (
    let rec aux : Module_var.t list -> (CST.type_expr -> CST.type_expr) -> CST.type_expr = fun lst f_acc ->
      match lst with
      | module_name::tl ->
        let module_name = decompile_mod_var module_name in
        let f = fun field ->
          f_acc (CST.TModA (Region.wrap_ghost CST.{module_name;selector=Token.ghost_dot;field}))
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
    | Literal_value.Literal_int i ->
      let z : CST.type_expr = CST.TInt { region = Region.ghost ; value = (Z.to_string i, i) } in
      return z
      | _ -> failwith "unsupported singleton"
  )
  | T_abstraction x -> decompile_type_expr x.type_
  | T_for_all x -> decompile_type_expr x.type_

let get_e_tuple : AST.expression -> _ = fun expr ->
  match expr.expression_content with
    E_tuple tuple -> tuple
  | E_variable _
  | E_literal _
  | E_constant _
  | E_module_accessor _
  | E_lambda _ -> [expr]
  | _ -> failwith @@
    Format.asprintf "%a should be a tuple expression"
    AST.PP.expression expr

type statement_or_expr =
  | Statement of CST.statement
  | Expr of CST.expr

let e_hd : _ -> CST.expr = function
  [Expr hd] -> hd
| _ -> failwith "not supported"

let rec s_hd = function
  [Statement hd] -> hd
| [Expr e] -> CST.SExpr e
| lst ->
  let lst = List.map ~f:(fun e -> s_hd [e]) lst in
  let lst = list_to_nsepseq ~sep:Token.ghost_semi lst in
  CST.SBlock (Region.wrap_ghost @@ braced @@ lst)

(* UNUSED .... :) *)
let decompile_operator : Constant.rich_constant -> CST.expr List.Ne.t -> CST.expr option = fun cons_name arguments ->
  match cons_name, arguments with
  | Const C_ADD, (arg1, [arg2])
  | Const C_POLYMORPHIC_ADD, (arg1, [arg2]) ->
     Some CST.(EArith (Add (Region.wrap_ghost { op = Token.ghost_plus ; arg1 ; arg2 })))
  | Const C_SUB, (arg1, [arg2])
  | Const C_POLYMORPHIC_SUB, (arg1, [arg2]) ->
     Some CST.(EArith (Sub (Region.wrap_ghost { op = Token.ghost_minus ; arg1 ; arg2 })))
  | Const C_MUL, (arg1, [arg2]) ->
     Some CST.(EArith (Mult (Region.wrap_ghost { op = Token.ghost_times ; arg1 ; arg2 })))
  | Const C_DIV, (arg1, [arg2]) ->
     Some CST.(EArith (Div (Region.wrap_ghost { op = Token.ghost_slash ; arg1 ; arg2 })))
  | Const C_MOD, (arg1, [arg2]) ->
     Some CST.(EArith (Mod (Region.wrap_ghost { op = Token.ghost_rem ; arg1 ; arg2 })))
  | Const C_NEG, (arg, []) ->
     Some CST.(EArith (Neg (Region.wrap_ghost { op = Token.ghost_minus ; arg })))
  | Const C_LT, (arg1, [arg2]) ->
     Some CST.(ELogic (CompExpr (Lt (Region.wrap_ghost { op = Token.ghost_lt ; arg1 ; arg2 }))))
  | Const C_LE, (arg1, [arg2]) ->
     Some CST.(ELogic (CompExpr (Leq (Region.wrap_ghost { op = Token.ghost_le ; arg1 ; arg2 }))))
  | Const C_GT, (arg1, [arg2]) ->
     Some CST.(ELogic (CompExpr (Gt (Region.wrap_ghost { op = Token.ghost_gt ; arg1 ; arg2 }))))
  | Const C_GE, (arg1, [arg2]) ->
     Some CST.(ELogic (CompExpr (Geq (Region.wrap_ghost { op = Token.ghost_ge ; arg1 ; arg2 }))))
  | Const C_EQ, (arg1, [arg2]) ->
     Some CST.(ELogic (CompExpr (Equal (Region.wrap_ghost { op = Token.ghost_eq ; arg1 ; arg2 }))))
  | Const C_NEQ, (arg1, [arg2]) ->
     Some CST.(ELogic (CompExpr (Neq (Region.wrap_ghost { op = Token.ghost_ne ; arg1 ; arg2 }))))
  | _ -> None

let rec decompile_expression_in : AST.expression -> statement_or_expr list = fun expr ->
  let return_expr expr = expr in
  match expr.expression_content with
  | E_variable name ->
    let var = decompile_variable name in
    return_expr @@ [Expr (CST.EVar (var))]
  | E_constant {cons_name; arguments} -> (
    let expr = CST.EVar (Region.wrap_ghost @@ Predefined.constant_to_string cons_name) in
    match arguments with
      [] -> return_expr @@ [Expr expr]
    | _ ->
      let args = List.map arguments
        ~f:(fun e ->
          let e = decompile_expression_in e in
          match e with
          | Expr hd :: [] -> CST.EPar (Region.wrap_ghost (par hd))
          | _ -> failwith "should not happen"
        )
      in
      let ne_args = list_to_nsepseq ~sep:Token.ghost_comma args in
      let arguments = CST.Multiple (Region.wrap_ghost (par ne_args)) in
      let const = Region.wrap_ghost (expr, arguments) in
      return_expr @@ [Expr (CST.ECall const)]
    )
  | E_literal literal -> (* TODO: Check those cases coming from dev *)
      (match literal with
          Literal_unit  ->  return_expr @@ [Expr (CST.EUnit (Region.wrap_ghost (Token.ghost_lpar,Token.ghost_rpar)))]
        | Literal_int i ->  return_expr @@ [Expr (CST.EArith (Int (Region.wrap_ghost (Z.to_string i, i))))]
        | Literal_nat n ->
           return_expr @@
             [Expr (CST.EAnnot {value = CST.EArith (Int (Region.wrap_ghost (Z.to_string n, n))),
                                        Token.ghost_colon,
                                        CST.TVar {value = "nat"; region = Region.ghost}; region = Region.ghost })]
        | Literal_timestamp time ->
          let time = Tezos_utils.Time.Protocol.to_notation @@
            Tezos_utils.Time.Protocol.of_seconds @@ Z.to_int64 time in
            (* TODO combinators for CSTs. *)
          let ty = decompile_type_expr @@ AST.t_timestamp () in
          let time = CST.EString (String (Region.wrap_ghost time)) in
          return_expr @@ [Expr (CST.EAnnot (Region.wrap_ghost @@ (time, Token.ghost_colon, ty)))]
        | Literal_mutez mtez ->
           return_expr @@
             [Expr (CST.EAnnot {value = CST.EArith (Int (Region.wrap_ghost (Z.to_string mtez, mtez))),
                                        Token.ghost_colon,
                                        CST.TVar {value = "mutez"; region = Region.ghost}; region = Region.ghost })]
        | Literal_string (Standard str) -> return_expr @@ [Expr (CST.EString (String   (Region.wrap_ghost str)))]
        | Literal_string (Verbatim ver) -> return_expr @@ [Expr (CST.EString (Verbatim (Region.wrap_ghost ver)))]
        | Literal_bytes b ->
          let b = Hex.of_bytes b in
          let s = Hex.to_string b in
          return_expr @@ [Expr (CST.EBytes (Region.wrap_ghost (s,b)))]
        | Literal_address addr ->
          let addr = CST.EString (String (Region.wrap_ghost addr)) in
          let ty = decompile_type_expr @@ AST.t_address () in
          return_expr @@ [Expr (CST.EAnnot (Region.wrap_ghost (addr,Token.ghost_colon,ty)))]
        | Literal_signature sign ->
          let sign = CST.EString (String (Region.wrap_ghost sign)) in
          let ty = decompile_type_expr @@ AST.t_signature () in
          return_expr @@ [Expr (CST.EAnnot (Region.wrap_ghost (sign,Token.ghost_colon,ty)))]
        | Literal_key k ->
          let k = CST.EString (String (Region.wrap_ghost k)) in
          let ty = decompile_type_expr @@ AST.t_key () in
          return_expr @@ [Expr (CST.EAnnot (Region.wrap_ghost (k,Token.ghost_colon,ty)))]
        | Literal_key_hash kh ->
          let kh = CST.EString (String (Region.wrap_ghost kh)) in
          let ty = decompile_type_expr @@ AST.t_key_hash () in
          return_expr @@ [Expr (CST.EAnnot (Region.wrap_ghost (kh,Token.ghost_colon,ty)))]
        | Literal_chain_id _
        | Literal_operation _ ->
          failwith "chain_id, operation are not created currently ?"
      | Literal_bls12_381_g1 b ->
        let b = Hex.of_bytes b in
        let s = Hex.to_string b in
        let b = CST.EBytes (Region.wrap_ghost (s, b)) in
        let ty = decompile_type_expr @@ AST.t_bls12_381_g1 () in
        return_expr @@ [Expr (CST.EAnnot (Region.wrap_ghost (b,Token.ghost_colon,ty)))]
      | Literal_bls12_381_g2 b ->
        let b = Hex.of_bytes b in
        let s = Hex.to_string b in
        let b = CST.EBytes (Region.wrap_ghost (s, b)) in
        let ty = decompile_type_expr @@ AST.t_bls12_381_g2 () in
        return_expr @@ [Expr (CST.EAnnot (Region.wrap_ghost (b,Token.ghost_colon,ty)))]
      | Literal_bls12_381_fr b ->
        let b = Hex.of_bytes b in
        let s = Hex.to_string b in
        let b = CST.EBytes (Region.wrap_ghost (s, b)) in
        let ty = decompile_type_expr @@ AST.t_bls12_381_fr () in
        return_expr @@ [Expr (CST.EAnnot (Region.wrap_ghost (b,Token.ghost_colon,ty)))]
      | Literal_chest _ | Literal_chest_key _ -> failwith "chest / chest_key not allowed in the syntax (only tests need this type)"
      )

  | E_application {lamb;args} ->
    let lamb = decompile_expression_in lamb in
    let lamb = match lamb with
      Expr hd :: [] -> hd
    |  _ -> failwith "should not happen"
    in
    let args = List.map (get_e_tuple args)
      ~f:(fun e ->
        let e = decompile_expression_in e in
        match e with
        | Expr hd :: [] -> hd
        | _ -> failwith "should not happen"
      )
    in
    let ne_args = list_to_nsepseq ~sep:Token.ghost_comma args in
    let arguments = CST.Multiple (Region.wrap_ghost (par ne_args)) in
    return_expr @@ [Expr (CST.ECall (Region.wrap_ghost (lamb,arguments)))]
  | E_lambda lambda ->
    let (parameters,lhs_type,body) = decompile_lambda lambda in
    let fun_expr : CST.fun_expr = {parameters;lhs_type;arrow=Token.ghost_arrow;body} in
    return_expr @@ [Expr (CST.EFun (Region.wrap_ghost @@ fun_expr))]
  | E_recursive {lambda; _} ->
    let (parameters,lhs_type,body) = decompile_lambda @@
      Lambda.map Fun.id Option.return lambda in
    let fun_expr : CST.fun_expr = {parameters;lhs_type;arrow=Token.ghost_arrow;body} in
    return_expr @@ [Expr (CST.EFun (Region.wrap_ghost @@ fun_expr))]
  | E_let_in {let_binder={var;ascr;attributes=_};rhs;let_result;attributes} ->
    let attributes = decompile_attributes attributes in
    let attributes = filter_private attributes in
    let var = CST.PVar (decompile_variable2 var) in
    let binders = var in
    let lhs_type = Option.map ~f:(prefix_colon <@ decompile_type_expr) ascr in
    let expr = decompile_expression_in rhs in
    let expr = e_hd expr in
    let let_binding = CST.{
      binders;
      lhs_type;
      type_params=None;
      eq = Token.ghost_eq;
      expr
    } in
    let const = CST.SConst (Region.wrap_ghost CST.{
      kwd_const = Token.ghost_const;
      bindings  = (Region.wrap_ghost let_binding, []);
      attributes
    }) in
    let body = decompile_expression_in let_result in
    return_expr @@ Statement const :: body
  | E_type_abstraction _ -> failwith "type_abstraction not supported yet"
  | E_type_in {type_binder;rhs;let_result} ->
    let name = decompile_type_var type_binder in
    let type_expr = decompile_type_expr rhs in
    let type_decl : CST.type_decl = {kwd_type=Token.ghost_type;name;params=None;eq=Token.ghost_eq;type_expr;attributes=[]} in
    let body = decompile_expression_in let_result in
    return_expr @@ Statement (CST.SType (Region.wrap_ghost type_decl)) :: body
  | E_mod_in {module_binder;rhs;let_result} -> (
    let name = decompile_mod_var module_binder in
    match rhs.wrap_content with
    | M_struct prg -> (
      let module_ = decompile_module prg in
      let toplevel_to_statement = function
        | CST.TopLevel (s, _) -> s
        | _ -> failwith "not implemented"
      in
      let a = (fst module_.statements) in
      let statements: CST.statements =
        toplevel_to_statement a, List.map ~f:(fun e -> (Token.ghost_semi, toplevel_to_statement e)) (snd module_.statements)
      in
      let statements: CST.statements CST.braces Region.reg = Region.wrap_ghost @@ braced statements in
      let body = decompile_expression_in let_result in
      let attributes = [] in
      [Statement (CST.SNamespace (Region.wrap_ghost (Token.ghost_namespace, name, statements, attributes)))] @ body
    )
    | M_variable v -> (
      let alias = name in
      let module_path = decompile_mod_var v , [] in
      let mod_alias : CST.import = {kwd_import=Token.ghost_import;alias;equal=Token.ghost_eq;module_path} in
      let body = decompile_expression_in let_result in
      [Statement (CST.SImport (Region.wrap_ghost mod_alias))] @ body
    )
    | M_module_path path -> (
      let alias = name in
      let module_path =
        nelist_to_npseq ~sep:Token.ghost_dot @@ List.Ne.map decompile_mod_var path
      in
      let mod_alias : CST.import = {kwd_import=Token.ghost_import;alias;equal=Token.ghost_eq;module_path} in
      let body = decompile_expression_in let_result in
      [Statement (CST.SImport (Region.wrap_ghost mod_alias))] @ body
    )
  )
  | E_raw_code {language; code} ->
    let language = Region.wrap_ghost language in
    let code = decompile_expression_in code in
    let (code, kwd_as, type_expr) = match code with
      [Expr (CST.EAnnot {value = hd; _})] -> hd
    | _ -> failwith "not implemented"
    in
    return_expr @@ [Expr (CST.EAnnot {value = CST.ECodeInj (Region.wrap_ghost CST.{language; code}), kwd_as,type_expr; region = Region.ghost })]
  | E_constructor {constructor;element} ->
    let Label constr = constructor in
    let constr = Region.wrap_ghost constr in
    let element = decompile_expression_in element in
    let element = e_hd element in
    return_expr @@ [Expr (CST.EConstr (Region.wrap_ghost (constr, Some element)))]
  | E_matching {matchee; cases} ->
    let expr  = decompile_expression_in matchee in
    let expr = e_hd expr in
    let cases = decompile_matching_cases cases in
    let var = CST.EVar (Region.wrap_ghost "match") in
    let args = CST.Multiple (Region.wrap_ghost (par (list_to_nsepseq ~sep:Token.ghost_comma [expr;cases]))) in
    return_expr @@ [Expr (CST.ECall (Region.wrap_ghost (var, args)))]
  | E_record record  ->
    let aux (Label.Label str, expr) =
      let field_expr = decompile_expression_in expr in
      let expr = e_hd field_expr in
      let field : CST.property = CST.Property (Region.wrap_ghost CST.{name = EVar (Region.wrap_ghost str); colon = Token.ghost_colon; value = expr}) in
      field
    in
    let record = List.map ~f:aux record in
    let record = list_to_nsepseq ~sep:Token.ghost_comma record in
    let record = braced record in
    return_expr @@ [Expr (CST.EObject (Region.wrap_ghost record))]
  | E_accessor {struct_; path} ->
    let rec aux : AST.expression -> AST.expression Access_path.t -> AST.expression * AST.expression Access_path.t = fun e acc_path ->
      match e.expression_content with
      | E_accessor { struct_ ; path } ->
        aux struct_ (path @ acc_path)
      | _ -> e,acc_path
    in
    let (struct_,path) = aux struct_ path in
    let struct_ = decompile_expression_in struct_ in
    let rec proj expr = function
      Access_path.Access_map e :: rest ->
        let e = decompile_expression_in e in
        let e = e_hd e in
        let arg = CST.Multiple (Region.wrap_ghost (par (list_to_nsepseq ~sep:Token.ghost_comma [e;expr]))) in
        proj (CST.ECall( Region.wrap_ghost (CST.EVar (Region.wrap_ghost "Map.find_opt"), arg))) rest
    | Access_tuple index :: rest ->
      let i = CST.EArith (Int (Region.wrap_ghost ("", index))) in
      let p = CST.{
        expr;
        selection = Component (Region.wrap_ghost @@ brackets @@ i)
      } in
      proj (CST.EProj (Region.wrap_ghost p)) rest
    | Access_record e :: rest ->
      let p = CST.{
        expr;
        selection = FieldName (Region.wrap_ghost {dot = Token.ghost_dot; value = Region.wrap_ghost e})
      } in
      proj (CST.EProj (Region.wrap_ghost p)) rest
    | [] -> expr
    in
    let x = proj (e_hd struct_) path in
    [Expr x]
  | E_ascription {anno_expr;type_annotation} ->
    let expr = decompile_expression_in anno_expr in
    let expr = e_hd expr in
    let ty   = decompile_type_expr type_annotation in
    return_expr @@ [Expr (CST.EAnnot (Region.wrap_ghost @@ (expr,Token.ghost_as,ty)))]
  | E_module_accessor {module_path;element} -> (
    let rec aux : Module_var.t list -> (CST.expr -> CST.expr) -> CST.expr = fun lst f_acc ->
      match lst with
      | module_name::tl ->
        let module_name = decompile_mod_var module_name in
        let f = fun field ->
          f_acc (CST.EModA (Region.wrap_ghost CST.{module_name;selector=Token.ghost_dot;field}))
        in
        aux tl f
      | [] ->
        let element = CST.EVar (decompile_variable element) in
        f_acc element
    in
    return_expr @@ [ Expr (aux module_path (fun x -> x)) ]
  )
  | E_sequence {expr1;expr2} ->
    let expr1 = decompile_expression_in expr1 in
    let s1 = s_hd expr1 in
    let expr2 = decompile_expression_in expr2 in
    let s2 = s_hd expr2 in
    let l2: statement_or_expr list = [Statement s1; Statement s2] in
    let s = statements_to_block l2 in
    return_expr [Statement (CST.SBlock s)]
  | E_cond {condition;then_clause;else_clause} ->
    let test  = decompile_expression_in condition in
    let test = CST.{lpar = Token.ghost_lpar; rpar = Token.ghost_rpar; inside = e_hd test} in
    let ifso  = decompile_expression_in then_clause in
    let ifso = s_hd ifso in
    let ifnot = decompile_expression_in else_clause in
    let ifnot = s_hd ifnot in
    let ifnot = Some(Token.ghost_else,ifnot) in
    let cond : CST.cond_statement = {kwd_if=Token.ghost_if;test;ifso;ifnot} in
    return_expr @@ [Statement (CST.SCond (Region.wrap_ghost cond))]
  | E_tuple tuple ->
    let tuple = List.map ~f:(fun e ->
      let e = decompile_expression_in e in
      (CST.Expr_entry (e_hd e))
    ) tuple in
    let tuple = list_to_nsepseq ~sep:Token.ghost_comma tuple in
    return_expr @@ [Expr (CST.EArray (Region.wrap_ghost @@ brackets @@ Some tuple))]
  | E_map map ->
    let map = List.map ~f:(Pair.map ~f:(fun e ->
      let e = decompile_expression_in e in
      (CST.Expr_entry (e_hd e))
    )) map in
    let aux (k,v) = CST.EArray (Region.wrap_ghost @@ brackets @@ Some (k,[(Token.ghost_comma,v)])) in
    let map = List.map ~f:aux map in
    (match map with
      [] -> return_expr @@ [Expr (CST.EVar (Region.wrap_ghost "Big_map.empty"))]
    | hd::tl  ->
        let var = CST.EVar (Region.wrap_ghost "Map.literal") in
        let args = CST.Multiple (Region.wrap_ghost (par (hd,List.map ~f:(fun x -> Token.ghost_comma,x) tl))) in
      return_expr @@ [Expr (CST.ECall (Region.wrap_ghost @@ (var, args)))]
    )
    | E_big_map big_map ->
      let big_map = List.map ~f:(Pair.map ~f:(fun e ->
        let e = decompile_expression_in e in
        (CST.Expr_entry (e_hd e))
      )) big_map in
      let aux (k,v) = CST.EArray (Region.wrap_ghost @@ brackets @@ Some (k,[(Token.ghost_comma,v)])) in
      let big_map = List.map ~f:aux big_map in
      (match big_map with
        [] -> return_expr @@ [Expr (CST.EVar (Region.wrap_ghost "Big_map.empty"))]
      | hd::tl  ->
        let var = CST.EVar (Region.wrap_ghost "Big_map.literal") in
        let args = CST.Multiple (Region.wrap_ghost (par (hd,List.map ~f:(fun x -> Token.ghost_comma,x) tl))) in
        return_expr @@ [Expr (CST.ECall (Region.wrap_ghost @@ (var, args)))]
      )
  | E_list lst ->
    let lst = List.map ~f:(fun e ->
      let e = decompile_expression_in e in
      (CST.Expr_entry (e_hd e))
    ) lst in
      let lst = list_to_nsepseq ~sep:Token.ghost_comma lst in
      return_expr @@ [Expr (ECall (Region.wrap_ghost (CST.EVar (Region.wrap_ghost "list"), CST.Multiple (Region.wrap_ghost @@ par @@ (CST.EArray (Region.wrap_ghost @@ brackets @@ Some lst), [] )))))]
  | E_set set ->
    let set = List.map ~f:decompile_expression_in set in
    let set = List.map ~f:e_hd set in
    let hd,tl = List.Ne.of_list @@ set in
    let var = CST.EVar (Region.wrap_ghost "Set.literal") in
    let args = CST.Multiple (Region.wrap_ghost (par (hd,List.map ~f:(fun x -> Token.ghost_comma,x) tl))) in
    return_expr @@ [Expr (CST.ECall (Region.wrap_ghost @@ (var,args)))]
  (* We should avoid to generate skip instruction*)
  | E_skip _ -> return_expr @@ [Expr (CST.EUnit (Region.wrap_ghost (Token.ghost_lpar,Token.ghost_rpar)))]
  | E_assign {binder;expression} ->
    let name = decompile_variable binder.var in
    let evar = CST.EVar name in
    let rhs = decompile_expression_in expression in
    return_expr @@ [Expr (CST.EAssign (evar, {value = CST.Eq; region = Region.ghost}, e_hd rhs))]
  | E_for_each {fe_binder;collection;fe_body; _} ->
    let var = decompile_variable (fst fe_binder) in
    let expr = decompile_expression_in collection in
    let expr = e_hd expr in
    let block = decompile_expression_in fe_body in
    let statement = s_hd block in
    let for_of : CST.for_of = {kwd_for=Token.ghost_for;lpar=Token.ghost_lpar;index_kind=`Const Token.ghost_const;index=var;kwd_of=Token.ghost_of;expr;rpar=Token.ghost_rpar;statement} in
    return_expr [Statement (CST.SForOf (Region.wrap_ghost for_of))]
  | E_while {cond;body} ->
    let cond  = decompile_expression_in cond in
    let expr = e_hd cond in
    let block = decompile_expression_in body in
    let statement = s_hd block in
    let loop : CST.while_stmt = {kwd_while=Token.ghost_while;lpar=Token.ghost_lpar;expr;rpar=Token.ghost_rpar;statement} in
    return_expr @@ [Statement (CST.SWhile (Region.wrap_ghost loop))]
  | E_for _ ->
    failwith @@ Format.asprintf "Decompiling a for loop to JsLIGO %a"
    AST.PP.expression expr
  (* Update on multiple field of the same record. may be removed by adding sugar *)
  | E_update {struct_;path;update} when List.length path > 1 ->
    failwith "Nested updates are not supported in JsLIGO."
  | E_update {struct_; path; update} ->
    let struct_ = decompile_expression_in struct_ in
    let expr = e_hd struct_ in
    let name = match path with
      [Access_record name] -> CST.EVar (Region.wrap_ghost name)
    | _ -> failwith "not supported"
    in
    let update = decompile_expression_in update in
    let update = e_hd update in
    let p:CST.property = CST.Property (Region.wrap_ghost CST.{
      name;
      colon = Token.ghost_colon;
      value = update
    }) in
    return_expr @@ [Expr (CST.EObject (Region.wrap_ghost @@ braced (CST.Property_rest (Region.wrap_ghost ({expr; ellipsis = Token.ghost_ellipsis}: CST.property_rest)), [(Token.ghost_comma, p)])))]

and statements_to_block (statements: statement_or_expr list) =
  let statements = List.map ~f:(fun f ->
    match f with
      Statement s -> s
    | Expr e -> SExpr e
  ) statements in
  let s = list_to_nsepseq ~sep:Token.ghost_semi statements in
  Region.wrap_ghost @@ braced s

and add_return statements =
  let statements = List.rev statements in
  let (last, before) = match statements with
    Statement last :: before -> (last, before)
  | Expr last :: before -> (SExpr last, before)
  | _ -> failwith "not implemented"
  in
  let rec aux l =
    match l with
      CST.SExpr (EUnit _) -> CST.SReturn (Region.wrap_ghost CST.{kwd_return = Token.ghost_return; expr = None})
    | CST.SExpr e -> CST.SReturn (Region.wrap_ghost CST.{kwd_return = Token.ghost_return; expr = Some e})
    | CST.SCond {value = {kwd_if; test; ifso; ifnot}; region} ->
      let ifso = aux ifso in
      let ifnot = match ifnot with
        Some (e, s) ->
          let s = aux s in
          Some (e, s)
      | None -> None
      in
      CST.SCond {value = {kwd_if; test; ifso; ifnot}; region }
    | CST.SBlock {value = {lbrace; inside; rbrace}; region} ->
      let inside = Utils.nsepseq_to_list inside in
      let inside = List.rev inside in
      let (last, before) = (match inside with
        last :: before -> (last, before)
      | [] -> failwith "not implemented"
      ) in
      let last = aux last in
      let inside = last :: before in
      let inside = List.rev inside in
      let inside = list_to_nsepseq ~sep:Token.ghost_semi inside in
      CST.SBlock {value = {lbrace; inside; rbrace}; region}
    | _ -> failwith "not implemented"
  in
  let last = aux last in
  List.rev (Statement last :: before)

and function_body body =
  let body = match body with
  | [Expr e] -> CST.ExpressionBody e
  | (_ :: _) as s ->
    let s = add_return s in
    let o = statements_to_block s in

    CST.FunctionBody o
  | _ -> failwith "not supported"
  in
  body

and decompile_lambda : (AST.expr, AST.ty_expr option) Lambda.t -> _ =
  fun {binder;output_type;result} ->
    let type_expr = Option.map ~f:decompile_type_expr binder.ascr in
    let type_expr = Option.value ~default:(TVar {value = "_"; region = Region.ghost}) type_expr in
    let v = decompile_variable binder.var in
    let seq = CST.ESeq (Region.wrap_ghost (CST.EAnnot (Region.wrap_ghost (CST.EVar v,Token.ghost_as,type_expr)), [])) in
    let parameters = CST.EPar (Region.wrap_ghost @@ par seq ) in
    let lhs_type = Option.map ~f:(prefix_colon <@ decompile_type_expr) output_type in
    let body = decompile_expression_in result in
    let body = function_body body in
    (parameters, lhs_type, body)

and decompile_matching_cases : _ Match_expr.match_case list -> CST.expr =
  fun m -> ignore m ; failwith "TODO: decompile matching cases"
    (* old version (before deep pattern matching) :
    let cases = match m with
    | Match_variant lst ->
      let aux ((c,(v:AST.expression_variable)),e) =
        let AST.Label c = c in
        let rhs = decompile_expression_in e in
        let rhs = e_hd rhs in
        (CST.Property (wrap ({
          name = CST.EVar (wrap c);
          colon = ghost;
          value = rhs;
        }: CST.property2)))
      in
      let fields = List.map ~f:aux lst in
      let fields = list_to_nsepseq fields in
      CST.EObject (wrap @@ braced fields)
    | Match_list {match_nil; match_cons} ->
      let (hd,tl,expr) = match_cons in
      let nil_expr = decompile_expression_in match_nil in
      let body = function_body nil_expr in
      let nil  = CST.EFun (wrap CST.{
        parameters = EAnnot (wrap (EArray (wrap @@ brackets (Empty_entry ghost, [])), ghost, TVar (wrap "any")));
        lhs_type = None;
        arrow = ghost;
        body}) in
      let cons_expr = decompile_expression_in expr in
      let body = function_body cons_expr in
      let hd = Var.to_name hd.wrap_content in
      let tl = ({expr = EVar (wrap (Var.to_name tl.wrap_content)); ellipsis = ghost }: CST.array_item_rest) in
      let cons  = CST.EFun (wrap CST.{
        parameters = EAnnot (wrap (EArray (wrap @@ brackets (Expr_entry (EVar (wrap hd)), [(ghost, (Rest_entry (wrap tl)))])), ghost, TVar (wrap "any")));
        lhs_type = None;
        arrow = ghost;
        body}) in
      let args = (CST.Expr_entry cons, [(ghost, CST.Expr_entry nil)]) in
      CST.ECall (wrap ((CST.EVar (wrap "list")), CST.Multiple (wrap @@ par (CST.EArray (wrap @@ brackets args), []))))
    | Match_variable (var, expr) -> failwith "not implemented"
    | Match_record _ -> failwith "match_record not available yet"
    | Match_tuple (lst, expr) ->  failwith "not implemented"
    | Match_option {match_none;match_some} ->
      let a = decompile_expression_in match_none in
      let body = function_body a in
      let name = Var.to_name (fst match_some).wrap_content in
      let body2 = decompile_expression_in (snd match_some) in
      let body2 = function_body body2 in
      let fields = CST.[
        CST.Property (wrap
          {name = CST.EVar (wrap "Some");
          colon = ghost;
          value = EFun (wrap CST.{parameters = EAnnot (wrap (EVar (wrap name), ghost, TVar (wrap "any"))); lhs_type = None; arrow = ghost; body = body2})
          });
          CST.Property (wrap
            {name = CST.EVar (wrap "None");
            colon = ghost;
            value = EFun (wrap CST.{parameters = EUnit (wrap (ghost, ghost)); lhs_type = None; arrow = ghost; body})
          })
      ] in
      let fields = list_to_nsepseq fields in
      CST.EObject (wrap @@ braced fields)
  in
  cases
  *)

and decompile_declaration : AST.declaration -> CST.statement = fun decl ->
  let decl = Location.unwrap decl in
  match decl with
    D_type {type_binder;type_expr;type_attr} ->
    let attr = type_attr in
    let is_private = List.mem ~equal:Caml.(=) attr "private" in
    let attributes : CST.attributes = decompile_attributes attr in
    let attributes = filter_private attributes in
    let name = decompile_type_var type_binder in
    let (params : CST.type_vars option) =
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
    let type_ = CST.SType (Region.wrap_ghost (CST.{attributes;kwd_type=Token.ghost_type; name; params;eq=Token.ghost_eq; type_expr})) in
    if is_private then
      type_
    else
      CST.SExport (Region.wrap_ghost (Token.ghost_export, type_))
  | D_value {binder; attr; expr } ->
    let is_private = List.mem ~equal:Caml.(=) attr "private" in
    let attributes : CST.attributes = decompile_attributes attr in
    let var = CST.PVar (decompile_variable2 binder.var) in
    let binders = var in
    let lhs_type = Option.map ~f:(prefix_colon <@ decompile_type_expr) binder.ascr in
    let expr = decompile_expression_in expr in
    let expr = e_hd expr in
    let binding = CST.({
      binders;
      lhs_type;
      type_params=None;
      eq = Token.ghost_eq;
      expr
    }) in
    let const = CST.SConst (Region.wrap_ghost (CST.{kwd_const=Token.ghost_const; bindings = (Region.wrap_ghost binding, []); attributes})) in
    if is_private then
      const
    else
      CST.SExport (Region.wrap_ghost (Token.ghost_export, const))
  | D_module {module_binder; module_; module_attr} -> (
    let name = decompile_mod_var module_binder in
    let is_private = List.mem ~equal:Caml.(=) module_attr "private" in
    let attributes = decompile_attributes module_attr in
    let attributes = filter_private attributes in
    match module_.wrap_content with
    | M_struct prg -> (
      let module_ = decompile_module prg in
      let toplevel_to_statement = function
          CST.TopLevel (s, _) -> s
        | _ -> failwith "not implemented"
        in
      let a = (fst module_.statements) in
      let statements: CST.statements = (toplevel_to_statement a, List.map ~f:(fun e -> (Token.ghost_semi, toplevel_to_statement e)) (snd module_.statements)) in
      let statements: CST.statements CST.braces Region.reg = Region.wrap_ghost @@ braced statements in
      let ns = CST.SNamespace (Region.wrap_ghost (Token.ghost_namespace, name, statements, attributes)) in
      if is_private then ns
      else CST.SExport (Region.wrap_ghost (Token.ghost_export, ns))
    )
    | M_variable v -> (
      let binders = name , [] in
      CST.SImport (Region.wrap_ghost CST.{alias = name; module_path = binders; kwd_import = Token.ghost_import; equal = Token.ghost_eq})
    )
    | M_module_path path -> (
      let alias = name in
      let binders =
        nelist_to_npseq ~sep:Token.ghost_dot @@ List.Ne.map decompile_mod_var path
      in
      CST.SImport (Region.wrap_ghost CST.{alias; module_path = binders; kwd_import = Token.ghost_import; equal = Token.ghost_eq})
    )
  )

and decompile_module : AST.module_ -> CST.ast = fun prg ->
  let decl = List.map ~f:(decompile_declaration) prg in
  let statements = List.Ne.of_list decl in
  let statements = Utils.nseq_map (fun s -> CST.TopLevel (s, None)) statements in
  (* let statements = ((fst statements, None), List.map ~f:(fun e -> (e, None)) (snd statements)) in *)
  ({statements;eof=Token.ghost_eof}: CST.ast)

let decompile_expression : AST.expression -> CST.expr list = fun expr ->
  match decompile_expression_in expr with
  | [] -> []
  | [Expr expr] -> [expr]
  | _ ->
     failwith @@ Format.asprintf
                   "An expression was expected, but this was decompiled to statements.\
                    @.Expr : %a@ @,Loc : %a"
                   AST.PP.expression expr
                   Location.pp expr.location

let rec decompile_pattern p =
  match (Location.unwrap p) with
  | Pattern.P_variant (constructor,_) -> (
      match constructor with
      | Label constructor -> (
        Ok (CST.PConstr (Region.wrap_ghost constructor))
      )
    )
  (* Note: Currently only the above branch AST.P_variant is valid
     as pattern-matching is only supported on Varaints.
     Pattern matching on lists cannot be incomplete as there is a check
     for this in tree-abstractor
     The rest of the cases are a best approximation of decompilation, these
     will not be really used, modify the rest of the cases when pattern
     matching will be handled in a better manner *)
  | P_unit -> Error "no PUnit in JsLIGO CST"
  | P_var v ->
    let name = { CST.variable = decompile_variable v.var ; attributes = [] } in
    Ok (CST.PVar (Region.wrap_ghost name))
  | P_tuple lst ->
    let rec aux = function
      [] -> Ok []
    | p::ps ->
      let p = decompile_pattern p in
      match p with
        Ok p -> Result.map (aux ps) ~f:(fun ps -> p :: ps)
      | Error e -> Error e
    in
    Result.map (aux lst) ~f:(fun pl ->
      let pl = list_to_nsepseq ~sep:Token.ghost_comma pl in
      CST.PArray (Region.wrap_ghost (brackets pl))
    )
  | P_list pl -> Error "no PList in JsLIGO CST"
  | P_record (llst,_) ->
    let fields_name = List.map ~f:(fun (Label x) ->
      CST.PVar (
        Region.wrap_ghost
          { CST.variable = Region.wrap_ghost x
          ; attributes = [] })) llst in
    let inj = list_to_nsepseq ~sep:Token.ghost_comma fields_name in
    let inj = Region.wrap_ghost @@ braced inj in
    Ok (CST.PObject inj)

let decompile_program : AST.program -> CST.ast = fun prg ->
  let decl = List.map ~f:decompile_declaration prg in
  let statements = List.Ne.of_list decl in
  let statements = Utils.nseq_map (fun s -> CST.TopLevel (s, None)) statements in
  (* let statements = ((fst statements, None), List.map ~f:(fun e -> (e, None)) (snd statements)) in *)
  ({statements;eof=Token.ghost_eof}: CST.ast)
